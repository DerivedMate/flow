module Eval where
import Syntax
import Helper.AssocMap 
import Debug.Trace
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe


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
    show a            = undefined

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
    = State { stLast   :: RTVal
            , stStack  :: [ Frame ]
            } deriving (Show, Eq)

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

cast TBool (RTInt a)        = RTBool (a /= 0)
cast TBool (RTFloat f)      = RTBool (f /= 0.0)
cast TBool (RTString s)     = RTBool (s /= "")
cast TBool (RTList l)       = RTBool (not $ null l)

cast t a                    = 
    trace 
        ("Unmatched type cast: " <> show t <> " <- " <> show a) 
        a

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
typeOfRt _              = undefined

-- Cast the latter to the type of the former
unifyType :: RTVal -> RTVal -> RTVal
unifyType a = cast (typeOfRt a)

fOfBop :: Operator -> RTVal -> RTVal -> RTVal
-- Int x Int
fOfBop OpAdd (RTInt a) (RTInt b) = RTInt $ a + b
fOfBop OpSub (RTInt a) (RTInt b) = RTInt $ a - b
fOfBop OpMul (RTInt a) (RTInt b) = RTInt $ a * b
fOfBop OpDiv (RTInt a) (RTInt b) = RTFloat $ (fromIntegral a :: Double) / (fromIntegral b :: Double)

-- Float x Float
fOfBop OpAdd (RTFloat a) (RTFloat b) = RTFloat $ a + b
fOfBop OpSub (RTFloat a) (RTFloat b) = RTFloat $ a - b
fOfBop OpMul (RTFloat a) (RTFloat b) = RTFloat $ a * b

-- Float x Int / Int x Float
fOfBop op a@(RTFloat _) b@(RTInt _)    = fOfBop op a (cast TFloat b)
fOfBop op a@(RTInt _)   b@(RTFloat _)  = fOfBop op a (cast TInt   b)

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
rtLength _            = 1

{--------------------------------:
    Function Helpers
:--------------------------------}

funcExists :: String -> State -> Bool
funcExists k s = assocExists (frameFns `concatMap` stStack s) k

funcFind :: String -> State -> Exp
funcFind k s = aux ( frameFns `concatMap` stStack s )
    where 
        aux []          = Nil
        aux ( (k', v) : ds )
            | k == k'   = v
            | otherwise = aux ds

{--------------------------------:
    Tuple Helpers
:--------------------------------}

deTuple :: RTVal -> [RTVal]
deTuple (RTTuple a) = a
deTuple (RTNil)     = []
deTuple a           = [a]

{--------------------------------:
    Main Line
:--------------------------------}

step :: Exp -> State -> IO [Datum]
step Nil _ = pure []

step (LInt a) s    = pure [Datum Nil (s { stLast = RTInt a })]
step (LFloat a) s  = pure [Datum Nil (s { stLast = RTFloat a })]
step (LBool a) s   = pure [Datum Nil (s { stLast = RTBool a })]
step (LString a) s = pure [Datum Nil (s { stLast = RTString a })]

-- Sequential tuple eval
step (LTuple ts) s = (:[]) 
                   . Datum Nil 
                   . (\s -> s { stLast = RTTuple 
                                       $ reverse 
                                       $ deTuple 
                                       $ stLast s 
                              }
                     ) 
                   <$> foldM aux s ts
    where
        aux :: State -> Exp -> IO State
        aux s t = mergeStates s . datumState . head <$> step t s 

        mergeStates :: State -> State -> State
        mergeStates s s' = 
            s' { stLast = RTTuple (deTuple (stLast s') <> deTuple (stLast s))}

{-
-- Concurrent tuple eval
step (LTuple ts) s = (:[]) . aux . concat <$> mapM (`step` s) ts 
    where 
        aux ds = 
            let l' = map (stLast . datumState) ds
            in Datum Nil (s { stLast = RTTuple l' })
-}

step (LList ls) s =   step (LTuple ls) s
                  >>= toTuple
    where 
        toTuple (Datum _ s : _) = pure [
            Datum 
                Nil 
                s {stLast = RTList (deTuple $ stLast s)}
            ]

step (Var k) s = pure [ Datum Nil s { stLast = aux (frameVars `concatMap` stStack s) } ]
    where 
        aux []          = RTNil
        aux ( (k', v) : ds )
            | k == k'   = v
            | otherwise = aux ds

{--------------------------------:
    Cells
:--------------------------------}
step (Cell MNone a) s = step a s 

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
    let s' = case l of
                (Just k) -> 
                    if not ( funcExists k s ) 
                    then
                        s { stStack 
                                = ( Frame { frameVars = []
                                        , frameFns  = [(k, Cell MNone  node)] 
                                        }  
                                ) : stStack s 
                            }
                    else s
                Nothing  -> s
    
    if length args > rtLength (stLast s') then
        pure [ Datum Nil s' ]
    else
        let sWithArgs = s' { stStack 
                              = ( Frame { frameVars = assignVars (stLast s') args
                                        , frameFns  = [] 
                                        }  
                                ) : stStack s' 
                           , stLast = RTNil
                           }
        in stepFExp f_body sWithArgs
    where 
        assignVars (RTTuple ts) args = zipWith bindVar ts args
        assignVars v          (a: _) = [bindVar v a]
        assignVars _             []  = []
        bindVar v (Arg k t)          = (k, cast t v)
        stepFExp FNil s              = pure [ Datum Nil s ]
        stepFExp (Single a) s        = step a s
        stepFExp (Cond c a b) s      = do
            (rc : _) <- step c s
            case (cast TBool . stLast . datumState) rc of
                RTBool True  -> step a s
                RTBool False -> stepFExp b s

step (FRef k) s = do
    guard ( funcExists k s )
    let f = funcFind k s
        in step f s

step (Flow a b) s = do
    r <- step a s
    case r of
        ( Datum Nil s' : ds ) -> pure ( Datum b s' : ds )
        ( Datum a'  s' : ds ) -> pure ( Datum (Flow a' b) s' : ds )

step (Program a b) s = do
    r <- step a s
    case r of
        ( Datum Nil s' : ds ) -> pure ( Datum b s' : ds )
        ( Datum a'  s' : ds ) -> pure ( Datum (Program a' b) s' : ds )

step (Io (IoStdIn t)) s = do
    l <- getLine
    pure [Datum Nil (s { stLast = rtParse t l })]

step (Io (IoStdOut t)) s = do
    case stLast s of
        (RTTuple (l : ls)) -> 
            print (cast t l)
            >> pure [Datum Nil (s { stLast = RTTuple ls })]
        l -> 
            print (cast t l)
            >> pure [Datum Nil (s { stLast = RTNil })]

runFlow :: (Maybe (Exp, a0)) -> IO ()
runFlow (Just (ast, _)) = step ast (State RTNil []) >>= aux >> (pure ())
    where 
        aux []           = pure () 
        aux ds           = (concat <$> sequence ( iter <$> ds )) 
                         >>= aux
        iter (Datum e s) = step e s
