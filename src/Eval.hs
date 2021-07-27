{-# LANGUAGE TupleSections #-}

module Eval where
import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Function
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Debug.Pretty.Simple
import           Debug.Trace
import           GHC.Stack
import           Helper.AssocMap
import           Syntax
import           System.IO
import           System.IO.Unsafe
import           Text.Pretty.Simple
import           Text.Show

data RTVal
    = RTInt    Int
    | RTFloat  Double
    | RTString String
    | RTBool   Bool
    | RTList   [ RTVal ]
    | RTTuple  [ RTVal ]
    | RTFunc   Exp
    | RTRef    Exp State
    | RTNil

instance Show RTVal where
    show (RTInt a)    = show a
    show (RTFloat a)  = show a
    show (RTString a) = "`" <> a <> "`"
    show (RTBool a)   = show a
    show (RTList l)   = "["
                      <> intercalate ", "
                       ( map show l )
                      <> "]"
    show (RTTuple t)  = "("
                      <> intercalate ", "
                       ( map show t )
                      <> ")"
    show (RTFunc (Cell _ (Func l args _))) =
        "~" <> fromMaybe "anonymous" l
            <> ": "
            <> intercalate ", "
                (map show args)
    show RTNil        = "nil"
    show (RTFunc a)   = show a
    show (RTRef e e') = "$ref: " <> show e <> "; " <> show e'

instance Eq RTVal where
    RTNil           == RTNil = True

    a@(RTInt aa)    == b = let RTInt bb = unifyType a b
                            in aa == bb
    a@(RTFloat aa)  == b = let RTFloat bb = unifyType a b
                            in aa == bb
    a@(RTString aa) == b = let RTString bb = unifyType a b
                            in aa == bb
    a@(RTBool aa)   == b = let RTBool bb = unifyType a b
                            in aa == bb
    a@(RTList aa)   == b = let RTList bb = unifyType a b
                            in aa == bb
    a@(RTTuple aa)  == b = let RTTuple bb = unifyType a b
                            in aa == bb
    a@(RTFunc aa)   == b = let RTFunc bb = unifyType a b
                            in aa == bb
    a@(RTRef a0 a1) == b = let RTRef b0 b1 = unifyType a b
                            in a0 == b0 && a1 == b1
    RTNil           == b = False

instance Ord RTVal where
    a@(RTInt aa)    <= b = let RTInt bb = unifyType a b
                            in aa <= bb
    a@(RTFloat aa)  <= b = let RTFloat bb = unifyType a b
                            in aa <= bb
    a@(RTString aa) <= b = let RTString bb = unifyType a b
                            in aa <= bb
    a@(RTBool aa)   <= b = let RTBool bb = unifyType a b
                            in aa <= bb
    a@(RTList aa)   <= b = let RTList bb = unifyType a b
                            in aa <= bb
    a@(RTTuple aa)  <= b = let RTTuple bb = unifyType a b
                            in aa <= bb
    (RTFunc _)      <= b = False
    (RTRef _ _)     <= b = False
    RTNil           <= b = True

type Frame = AssocMap String RTVal

data State
    = State { stLast  :: RTVal
            , stStack :: [ Frame ]
            } deriving (Show, Eq)
{-
instance Show State where
    show (State last stack) =
           "[Last]: "    <> show last
        <> "\n[Stack]: " <> show stack
-}
data Datum
    = Datum { datumExp   :: Exp
            , datumState :: State
            } deriving (Show, Eq)


{--------------------------------:
    Type Operations
:--------------------------------}

cast :: Type -> RTVal -> RTVal
-- Cast Id
cast TInt a@(RTInt _)       = a
cast TFloat a@(RTFloat _)   = a
cast TString a@(RTString _) = a
cast TBool a@(RTBool _)     = a
cast (TList t) (RTList ls)  = RTList (map (cast t) ls)

-- Int Cast
cast TInt (RTFloat d)       = RTInt $ floor d
cast TInt (RTString s)
    | all isNumber s = RTInt (read s)
    | otherwise      = RTInt (length s)

cast TInt (RTBool True)     = RTInt 1
cast TInt (RTBool False)    = RTInt 0

-- Float Cast
cast TFloat (RTInt i)       = RTFloat $ fromIntegral i
cast TFloat a               = let RTInt i = cast TInt a
                              in RTFloat (fromIntegral i)
-- Truthiness
cast TBool (RTInt a)        = RTBool (a /= 0)
cast TBool (RTFloat f)      = RTBool (f /= 0.0)
cast TBool (RTString s)     = RTBool (s /= "")
cast TBool (RTList l)       = RTBool (not $ null l)

-- String Cast
cast TString a              = RTString (show a)

cast (TFunc t ts) (RTFunc (Func l args e)) =
    RTFunc ( Cell MNone (Func l args' e) )
    where
        args'                      = zipWith aux (tFuncTypes ts [t]) args
        tFuncTypes (TFunc t f) acc = tFuncTypes f (t:acc)
        tFuncTypes t acc           = t:acc
        aux t (Arg n _)            = Arg n t

cast t a                    =
    error ( "Unmatched type cast: "
            <> show a
            <> " -> "
            <> show t
          )

rtParse :: Type -> String -> RTVal
rtParse TInt s   = RTInt (read s :: Int)
rtParse TFloat s = RTFloat (read s :: Double)
rtParse from str = undefined

typeOfRt :: RTVal -> Type
typeOfRt (RTInt _)      = TInt
typeOfRt (RTFloat _)    = TFloat
typeOfRt (RTString _)   = TString
typeOfRt (RTBool _)     = TBool
typeOfRt (RTList (l:_)) = TList (typeOfRt l)
typeOfRt r@(RTFunc (Cell _ (Func _ (a : args) _))) =
    foldr aux (TFunc (argType a) ) args TAny
    where
        aux a f = TFunc (f (argType a))

typeOfRt r              = error "Unknown typeOfRt"


-- Cast the latter to the type of the former
unifyType :: RTVal -> RTVal -> RTVal
unifyType a = cast (typeOfRt a)

fOfBop :: Operator -> RTVal -> RTVal -> RTVal
-- Int x Int
fOfBop OpAdd (RTInt a) (RTInt b) = RTInt $ a + b
fOfBop OpSub (RTInt a) (RTInt b) = RTInt $ a - b
fOfBop OpMul (RTInt a) (RTInt b) = RTInt $ a * b
fOfBop OpDiv (RTInt a) (RTInt b) = RTFloat $ (fromIntegral a :: Double) / (fromIntegral b :: Double)
fOfBop OpExp (RTInt a) (RTInt b) = RTFloat $ (fromIntegral a :: Double) ** (fromIntegral b :: Double)

-- Float x Float
fOfBop OpAdd (RTFloat a) (RTFloat b) = RTFloat $ a + b
fOfBop OpSub (RTFloat a) (RTFloat b) = RTFloat $ a - b
fOfBop OpMul (RTFloat a) (RTFloat b) = RTFloat $ a * b
fOfBop OpExp (RTFloat a) (RTFloat b) = RTFloat $ a ** b

-- Float x Int / Int x Float
fOfBop op a@(RTFloat _) b@(RTInt _)    = fOfBop op a (cast TFloat b)
fOfBop op a@(RTInt _)   b@(RTFloat _)  = fOfBop op a (cast TInt   b)

-- Modulo
fOfBop OpMod (RTInt a) (RTInt b)       = RTInt (a `mod` b)
fOfBop OpMod a         b               = fOfBop OpMod (cast TInt a) (cast TInt b)

-- String x String
fOfBop OpAdd (RTString a) (RTString b) = RTString ( a <> b )
-- String Ops
fOfBop OpMul (RTString a) (RTInt n)
    | n > 0     = RTString $ concat $ replicate n a
    | otherwise = RTString ""

-- Truthy
-- Truthy: Logic
fOfBop OpAnd a b =
    let
        RTBool a' = cast TBool a
        RTBool b' = cast TBool b
    in RTBool (a' && b')
fOfBop OpOr a b =
    let
        RTBool a' = cast TBool a
        RTBool b' = cast TBool b
    in RTBool (a' || b')
-- Truthy: Comparison
fOfBop OpGt   a b = RTBool (a > b )
fOfBop OpGtEq a b = RTBool (a >= b)
fOfBop OpLt   a b = RTBool (a < b )
fOfBop OpLtEq a b = RTBool (a <= b)
fOfBop OpEq   a b = RTBool (a == b)
fOfBop OpNeq  a b = RTBool (a /= b)
-- General
fOfBop op a b = fOfBop op a (unifyType a b)

rtLength :: RTVal -> Int
rtLength RTNil        = 0
rtLength (RTTuple ts) = length ts
rtLength (RTString s) = length s
rtLength _            = 1

{--------------------------------:
    Function Helpers
:--------------------------------}

funcExists :: String -> State -> Bool
funcExists k s = assocExists ( concat $ stStack s ) k

funcFind :: String -> State -> Exp
funcFind k s = toExp $ aux ( concat $ stStack s )
    where
        toExp (RTFunc f) = f
        toExp _          = Nil
        aux []           = RTNil
        aux ( (k', v) : ds )
            | k == k'   = v
            | otherwise = aux ds

funcArgs :: Exp -> [ Arg ]
funcArgs (Func _ as _) = as
funcArgs (Cell _ f)    = funcArgs f
funcArgs e             = error ("call funcArgs of non-func: " <> show e)

argName :: Arg -> String
argName (Arg n _) = n

argType :: Arg -> Type
argType (Arg _ t) = t

assignVars :: RTVal -> [ Arg ] -> [ (String, RTVal) ]
assignVars (RTTuple ts) args = zipWith bindVar ts args
assignVars v          (a: _) = [bindVar v a]
assignVars _             []  = []

bindVar :: RTVal -> Arg -> (String, RTVal)
bindVar v (Arg k t)          = (k, cast t v)

deleteVars :: [ String ] -> [ Frame ] -> [ Frame ]
deleteVars [] frames      = frames
deleteVars _ []           = []
deleteVars (v:vs) (f:fs)
    | isEmpty f' = deleteVars vs fs
    | otherwise  = deleteVars vs (f':fs)
    where
        f'   = vrs'
        vrs' = deleteBy ((==) `on` fst) (v, RTNil) f
        isEmpty [] = True
        isEmpty _  = False

getVarF :: String -> Frame -> Maybe RTVal
getVarF = lookup

getVar :: String -> State -> Maybe RTVal
getVar k s = foldl aux Nothing (stStack s)
    where aux a f = a <|> getVarF k f

{--------------------------------:
    Tuple Helpers
:--------------------------------}

deTuple :: RTVal -> [ RTVal ]
deTuple (RTTuple a)  = a
deTuple RTNil        = []
deTuple (RTList a)   = a
deTuple (RTString s) = map (RTString . (:[])) s
deTuple a            = [a]

wrapperOfState :: RTVal -> ([RTVal] -> RTVal)
wrapperOfState (RTTuple _) = RTTuple
wrapperOfState _           = RTList

{--------------------------------:
    Main Line
:--------------------------------}

step :: Exp -> State -> IO [ Datum ]
step Nil _ = pure []

step (LInt a) s    = pure [Datum Nil (s { stLast = RTInt a })]
step (LFloat a) s  = pure [Datum Nil (s { stLast = RTFloat a })]
step (LBool a) s   = pure [Datum Nil (s { stLast = RTBool a })]
step (LString a) s = pure [Datum Nil (s { stLast = RTString a })]

-- Concurrent tuple eval
step (LTuple ts) s = (:[]) . aux . concat <$> mapM (`step` s) ts
    where
        aux ds =
            let l' = map (stLast . datumState) ds
            in Datum Nil (s { stLast = RTTuple l' })
---}

step (LList ls) s =   step (LTuple ls) s
                  >>= toTuple
    where
        toTuple [] = pure []
        toTuple (Datum _ s : _) = pure [
            Datum
                Nil
                s {stLast = RTList (deTuple $ stLast s)}
            ]

step (Var k) s
    | RTFunc f <- v
    = pure [ Datum f s ]
    | otherwise = pure [ Datum Nil s { stLast = v } ]
    where
        v                   = findVar (concat $ stStack s)
        findVar []          = RTNil
        findVar ( (k', v) : ds )
            | k == k'   = v
            | otherwise = findVar ds

{--------------------------------:
    Cells
:--------------------------------}

step (Cell MNone a) s = step a s
step (Cell MMap e) s0 =
    mapM (runner []) [[Datum e (s0 { stLast = a })] | a <- as0]
    >>= rejoin
    where
        as0               = (deTuple . stLast) s0
        iter (Datum e s)  = step e s
        rejoin ls         = pure [Datum Nil s0 { stLast = wrapperOfState (stLast s0) (concat (reverse <$> ls)) }]

        runner :: [RTVal] -> [Datum] -> IO [RTVal]
        runner ready []   = pure ready
        runner ready circ =
            let (rs, cs)  = retire circ ready []
            in mapM iter cs >>= runner rs . concat

        retire :: [ Datum ] -> [ RTVal ] -> [ Datum ] -> ([ RTVal ], [ Datum ])
        retire [] rs bs = (rs, bs)
        retire (c:cs) rs bs
            | Datum Nil s <- c
            = retire cs (stLast s : rs) bs
            | otherwise
            = retire cs rs (c:bs)


step (Cell MKeep e) s = step e s >>= mapM aux <&> concat
    where
        discriminant r
            | RTBool d <- cast TBool r
            = d
            | otherwise
            = error "Non-bool cast in keep predicate"

        aux (Datum Nil s')
            | discriminant (stLast s')
            = pure [ Datum Nil s ]
            | otherwise
            = pure []
        aux (Datum e' s')
            = pure [ Datum (Cell MKeep e') s' ]

step (Cell MKeepEnum e) s
    =   mapM runBranch ((deTuple . stLast) s) 
    <&> rewrap . foldl1 (<>)
    where
        runBranch :: RTVal -> IO [ RTVal ]
        runBranch v = exhaustBranch v [] [ Datum e (s { stLast = v }) ]

        retire :: [ Datum ] -> ( [ RTVal ], [ Datum ] ) -> ( [ RTVal ], [ Datum ] )
        retire [] (rs, ys)
            = ( rs, ys )
        retire (d:ds) (rs, ys)
            | Nil         == datumExp d
            , RTBool True == (stLast . datumState) d
            = retire ds ( (stLast . datumState) d : rs, ys )
            | Nil == datumExp d
            = retire ds ( rs, ys )
            | otherwise
            = retire ds ( rs, d : ys )

        exhaustBranch :: RTVal -> [ RTVal ] -> [ Datum ] -> IO [ RTVal ]
        exhaustBranch v retired []
            = pure [ v | r <- retired, cast TBool r == RTBool True ]
        exhaustBranch v retired ds
            = let (retired', ds') = retire ds (retired, [])
              in iter ds' >>= exhaustBranch v retired'

        iter :: [Datum] -> IO [Datum]
        iter ds = concat <$> mapM (\(Datum e s) -> step e s) ds

        rewrap :: [RTVal] -> [Datum]
        rewrap vs = [ Datum Nil (s {stLast = (wrapperOfState . stLast) s vs}) ]


step (Cell MGen a) s =
    pure [Datum (Anchor MGen a a) s]

step (Anchor MGen e0 e) s0 = step e s0 >>= aux
    where
        aux :: [ Datum ] -> IO [ Datum ]
        aux ds
            = mapM discriminant ds <&> concat

        discriminant :: Datum -> IO [ Datum ]
        discriminant d
            | Datum Nil s' <- d
            = (pure . matchResult) s'
            | Datum e'  s' <- d
            = pure [ Datum (Anchor MGen e0 e') s']

        matchResult :: State -> [ Datum ]
        matchResult s
            | RTTuple [ y, k, RTBool doEmit ] <- l
            , doEmit
            = [ Datum Nil (s { stLast = y })
              , Datum (Anchor MGen e0 e0) (s0 { stLast = k })
              ]
            | RTTuple [ y, k ] <- l
            = matchResult $ s { stLast = RTTuple [ y, k, RTBool True ] }
            | RTTuple [ y ] <- l
            = matchResult $ s { stLast = RTTuple [ y, y, RTBool True ] }
            | otherwise
            = []
            where l = stLast s

{--------------------------------:
    Operations
:--------------------------------}

step (BinOp op a b) s = do
    ra <- step a s
    case ra of
        (Datum Nil s' : _) -> do
            rb <- step b s'
            case rb of
                (Datum Nil s'' : _) ->
                    pure [Datum Nil (s''
                        { stLast = fOfBop op
                                    (stLast s')
                                    (stLast s'')
                        })]
                _ -> pure [ Datum (BinOp op a b) s ]
        _ -> pure [ Datum (BinOp op a b) s ]

step node@(Func l args f_body) s = do
    let
        -- Define the function if is labeled
        fns = maybe [] (\k -> [(k, RTFunc node) | not ( funcExists k s )]) l

        -- Check for partial application
        (last, vars, doRun) =
            if length args > rtLength (stLast s)
                then
                    let (appliedArgs, leftArgs) = splitAt (rtLength (stLast s)) args
                    in ( RTFunc (Func l leftArgs f_body)
                       , assignVars (stLast s) appliedArgs
                       , False
                       )
                else
                    (RTNil, assignVars (stLast s) args, True)
        f' = vars `union` fns
        s' = State { stLast= last
                   , stStack= f' : stStack s
                   }
        in
            if doRun
                then stepFExp f_body s'
                else pure [ Datum Nil s' ]
    where
        stepFExp :: FuncExp -> State -> IO [ Datum ]
        stepFExp FNil s              = pure [ Datum Nil s ]
        stepFExp (Single a) s        = step a s
        stepFExp (Cond c a b) s      = do
            (rc : _) <- step c s
            case (cast TBool . stLast . datumState) rc of
                RTBool True  -> step a s
                RTBool False -> stepFExp b s
                r            -> error
                    (  "Undefined boolean cast in func conditional:"
                         <> "\n[Cast]: "      <> show r
                         <> "\n[State]: "     <> show s
                         <> "\n[Condition]: " <> show c
                    )

step (FRef k) s = do
    if funcExists k s
    then step f s
    else error ("Undefined function '" <> k <> "'. State:\n" <> show s)
    where f = funcFind k s

step self@(Flow a b) s = do
    map aux <$> step a s
    where
        aux r
            | Datum Nil s' <- r
            = Datum b s'
            | Datum a'  s' <- r
            = Datum (Flow a' b) s'

step self@(Program a b) s = do
    map aux <$> step a s
    where
        aux r
            | Datum Nil s' <- r
            = Datum b (s' { stLast = RTNil })
            | Datum a'  s' <- r
            = Datum (Program a' b) s'

step (Io (IoStdIn t)) s = do
    l <- getLine
    pure [Datum Nil (s { stLast = rtParse t l })]

step (Io (IoStdOut t)) s = do
    case stLast s of
        (RTTuple (l : ls)) -> do
            print (cast t l)
            >> pure [Datum Nil (s { stLast = RTTuple ls })]
        l ->
            print (cast t l)
            >> pure [Datum Nil (s { stLast = RTNil })]

step _ _ = undefined

check :: Show a => a -> State -> [ Datum ] -> [ Datum ]
check l s d =
    let
        st = datumState $ head d
        vs = stStack st

    in trace
        (  "\n" <> show l <> "\n"
        <> "{Stack}: "     <> intercalate ", " (show <$> vs) <> "\n"
        <> "{Last}: "      <> show (stLast st) <> "\n"
        <> "{Prev Last}: " <> show (stLast s)
        )
        d

runFlow :: Maybe (Exp, a0) -> IO ()
runFlow Nothing         = error "Filed to compile the source"
runFlow (Just (ast, _)) = step ast (State RTNil []) >>= aux >> pure ()
    where
        aux []           = pure ()
        aux ds           = sequence ( iter <$> ds )
                         >>= aux . concat
        iter (Datum e s) = step e s
        help ds = trace ("\n" <> intercalate " <<<<<<<< END >>>>>>>> \n\n" (fmap (show . (\(Datum e s) -> Datum e s{stStack = []})) ds) <> "\n\n") ds
