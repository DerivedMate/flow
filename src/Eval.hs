module Eval where
import Syntax


data RTVal 
    = RTInt    Int
    | RTFloat  Double
    | RTString String
    | RTBool   Bool
    | RTList   [ RTVal ]
    | RTFunc   Exp        -- Maybe
    | RTNil 
    deriving (Show, Eq)

type AssocMap k v = [ (k, v) ]
data Frame 
    = Frame { frameVars :: AssocMap String RTVal
            , frameFns  :: AssocMap String Exp    -- Cell pointers
            }

data State 
    = State { stLast   :: RTVal
            , stStack  :: [ Frame ]
            } 

data Datum = Datum Exp State 

{--------------------------------:
    Type Operations
:--------------------------------}

cast :: Type -> RTVal -> RTVal
-- Cast Id
cast TInt a@(RTInt _)       = a
cast TFloat a@(RTFloat _)   = a
cast TString a@(RTString _) = a
cast TBool a@(RTBool _)     = a
-- Int Cast
cast TInt (RTFloat d)       = RTInt $ floor d 
cast TInt (RTString s)      = RTInt $ length s
cast TInt (RTBool True)     = RTInt 1
cast TInt (RTBool False)    = RTInt 0

rtParse :: Type -> String -> RTVal
rtParse from str = undefined

fOfBop :: Operator -> RTVal -> RTVal -> RTVal
-- Int x Int
fOfBop OpAdd (RTInt a) (RTInt b) = RTInt $ a + b
fOfBop OpSub (RTInt a) (RTInt b) = RTInt $ a - b
fOfBop OpMul (RTInt a) (RTInt b) = RTInt $ a * b
fOfBop OpDiv (RTInt a) (RTInt b) = RTFloat $ (fromIntegral a :: Double) / (fromIntegral b :: Double)

fOfBop op a b = undefined 

{--------------------------------:
    IO helpers
:--------------------------------}

rtPrintStdIo (RTInt a)    = print a
rtPrintStdIo (RTFloat a)  = print a
rtPrintStdIo (RTString a) = print a
rtPrintStdIo (RTBool a)   = print a
rtPrintStdIo RTNil        = print "Nil"
rtPrintStdIo a            = print a

step :: Exp -> State -> IO [Datum]
step Nil _ = pure []

step (LInt a) s    = pure [Datum Nil (s { stLast = RTInt a })]
step (LFloat a) s  = pure [Datum Nil (s { stLast = RTFloat a })]
step (LBool a) s   = pure [Datum Nil (s { stLast = RTBool a })]
step (LString a) s = pure [Datum Nil (s { stLast = RTString a })]

step (Var k) s = pure [ Datum Nil s { stLast = aux (frameVars `concatMap` stStack s) } ]
    where 
        aux []          = RTNil
        aux ( (k', v) : ds )
            | k == k'   = v
            | otherwise = aux ds

step (Cell fmod a) s = interpretMod fmod <$> step a s 
    where
        interpretMod MNone r = r

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
    rtPrintStdIo (cast t (stLast s))
    >> pure [Datum Nil (s { stLast = RTNil })]

runFlow (Just (ast, _)) = step ast (State RTNil []) >>= aux
    where 
        aux [] = mempty 
        aux ds = concat <$> sequence ( iter <$> ds )
        iter (Datum e s) = step e s
