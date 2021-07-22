module Eval where
import           Control.Applicative
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Maybe
import           Debug.Pretty.Simple
import           Debug.Trace
import           Helper.AssocMap
import           Syntax
import           System.IO

data RTVal
    = RTInt    Int
    | RTFloat  Double
    | RTString String
    | RTBool   Bool
    | RTList   [ RTVal ]
    | RTTuple  [ RTVal ]
    | RTFunc   Exp        -- Maybe
    | RTNil

instance Show RTVal where
    show (RTInt a)    = show a
    show (RTFloat a)  = show a
    show (RTString a) = a
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
    a@(RTFunc aa)   <= b = False
    RTNil           <= b = True

data Frame
    = Frame { frameVars :: AssocMap String RTVal
            , frameFns  :: AssocMap String Exp    -- Cell pointers
            } deriving (Show, Eq)

data State
    = State { stLast  :: RTVal
            , stStack :: [ Frame ]
            } deriving Eq

instance Show State where
    show (State last stack) =
           "[Last]: "    <> show last
        <> "\n[Stack]: " <> show stack

data Datum
    = Datum { datumExp   :: Exp
            , datumState :: State
            }
    deriving (Show, Eq)


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
cast TInt (RTString s)      = RTInt $ length s
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

cast (TFunc _ _) f@(RTFunc _) = f
cast (TFunc _ _) a          = pTraceShowId a

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
    pTraceShow r $ foldr aux (TFunc (argType a) ) args TAny
    where
        aux a f = TFunc (f (argType a))


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
funcExists k s = assocExists ( frameFns `concatMap` stStack s ) k

funcFind :: String -> State -> Exp
funcFind k s = aux ( frameFns `concatMap` stStack s )
    where
        aux []          = Nil
        aux ( (k', v) : ds )
            | k == k'   = v
            | otherwise = aux ds

funcArgs :: Exp -> [Arg]
funcArgs (Func _ as _) = as
funcArgs (Cell _ f)    = funcArgs f
funcArgs e             = errorWithoutStackTrace ("call funcArgs of non-func: " <> show e)

argName :: Arg -> String
argName (Arg n _) = n

argType :: Arg -> Type
argType (Arg _ t) = t

assignVars :: RTVal -> [Arg] -> [(String, RTVal)]
assignVars (RTTuple ts) args = zipWith bindVar ts args
assignVars v          (a: _) = [bindVar v a]
assignVars _             []  = []
bindVar v (Arg k t)          = (k, cast t v)

deleteVars :: [String] -> [Frame] -> [Frame]
deleteVars [] frames      = frames
deleteVars _ []           = []
deleteVars (v:vs) (f:fs)
    | isEmpty f' = deleteVars vs fs
    | otherwise  = deleteVars vs (f':fs)
    where
        f'   = Frame vrs' (frameFns f)
        vrs' = deleteBy ((==) `on` fst) (v, RTNil) (frameVars f)
        isEmpty (Frame [] []) = True
        isEmpty _             = False

correctLast :: [Arg] -> [Datum] -> [Datum]
correctLast args [] = []
correctLast args ds = fmap aux ds
    where
        aux d  = d { datumState=
                    (datumState d) { stStack = stack' }
                    }
            where
                stack' = deleteVars (fmap argName args) (stStack $ datumState d)



{--------------------------------:
    Tuple Helpers
:--------------------------------}

deTuple :: RTVal -> [RTVal]
deTuple (RTTuple a)  = a
deTuple RTNil        = []
deTuple (RTList a)   = a
deTuple (RTString s) = map RTString [s]
deTuple a            = [a]

{--------------------------------:
    Main Line
:--------------------------------}

step :: Exp -> State -> IO [Datum]
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
        v                   = findVar (frameVars `concatMap` stStack s)
        findVar []          = RTNil
        findVar ( (k', v) : ds )
            | k == k'   = v
            | otherwise = findVar ds

{--------------------------------:
    Cells
:--------------------------------}

step (Cell MNone a) s = step a s
step (Cell MMap a) s = (:[]) . foldl1 aux . concat <$> mapM (step a) ss0
    where
        prepLast                     = deTuple . stLast
        aux (Datum _ s) (Datum _ s') =
            Datum Nil ( State
                        (RTList (prepLast s <> prepLast s'))
                        (stStack s `union` stStack s' )
                      )
        ss0 = (\a -> State a (stStack s)) <$> prepLast s

step (Cell MKeep a) s =   (:[])
                      .   (\ls -> Datum Nil (s {stLast=RTList ls}))
                      <$> mLasts'
    where
        mLasts'                    =   map fst
                                   .   filter discriminant
                                   .   zip (stLast <$> ss0)
                                   .   map toPred
                                   .   concat
                                   <$> mapM (step a) ss0
        discriminant (_, RTBool a) = a
        discriminant (a, b)        = discriminant (a, cast TBool b)
        toPred (Datum _ s)         = cast TBool (stLast s)
        prepLast                   = deTuple . stLast
        ss0                        = (\a -> s {stLast = a}) <$> prepLast s

-- MGen needs the flow reference
step self@(Flow (Cell MGen a) b) s = do
    manageReturn <$> step a s
    where
        manageReturn :: [ Datum ] -> [ Datum ]
        manageReturn []               = []
        manageReturn (Datum _ s : ds) = matchResult (stLast s) <> ds

        matchResult :: RTVal -> [ Datum ]
        matchResult (RTTuple [yield, next, RTBool do_emit])
            | do_emit    =
                [ Datum b    (s { stLast = yield })
                , Datum self (s { stLast = next  })
                ]
            | otherwise  = []
        matchResult (RTTuple [yield, next]) =
            matchResult (RTTuple [yield, next, RTBool True])
        matchResult (RTTuple []) =
            []
        matchResult r =
            matchResult (RTTuple [r, r, RTBool True])

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
    -- Define the function if is labeled
    let
        fns = maybe [] (\k -> [(k, Cell MNone  node) | not ( funcExists k s )]) l

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
        f' = Frame { frameVars=vars, frameFns=fns }
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
    else ioError $ userError ("Undefined function '" <> k <> "'. State:\n" <> show s)
    where f = funcFind k s

step self@(Flow a b) s = do
    r <- step a s
    case r of
        ( Datum Nil s' : ds ) -> pure ( Datum b s' : ds )
        ( Datum a'  s' : ds ) -> pure ( Datum (Flow a' b) s' : ds )
        rs                    -> error
            ("Empty Flow Return:\n[Node]: "  <> show a
                            <> "\n[State]: " <> show s
            )

step self@(Program a b) s = do
    r <- step a s
    case r of
        ( Datum Nil s' : ds ) -> pure ( Datum b (s' {stLast = RTNil}) : ds )
        ( Datum a'  s' : ds ) -> pure ( Datum (Program a' b) s' : ds )
        rs                    -> error
            ("Empty Flow Return:\n[Node]: "  <> show a
                            <> "\n[State]: " <> show s
            )

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

check :: Show a => a -> State -> [Datum] -> [Datum]
check l s d                      =
            let
                st = datumState $ head d
                vs = frameVars <$> stStack st

            in trace
                (  "\n" <> show l <> "\n"
                <> "{Stack}: "     <> intercalate ", " (show <$> vs) <> "\n"
                <> "{Last}: "      <> show (stLast st) <> "\n"
                <> "{Prev Last}: " <> show (stLast s)
                )
                d


runFlow :: Maybe (Exp, a0) -> IO ()
runFlow Nothing         = ioError $ userError "Filed to compile the source"
runFlow (Just (ast, _)) = step ast (State RTNil []) >>= aux >> pure ()
    where
        aux []           = pure ()
        aux ds           = sequence ( iter <$> ds )
                         >>= aux . concat
        iter (Datum e s) = step e s
        help ds = trace ("\n" <> intercalate ", " (fmap show ds)) ds
