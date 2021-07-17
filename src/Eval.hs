module Eval where
import Syntax
import Helper.AssocMap 
import Debug.Trace
import Control.Monad


data RTVal 
    = RTInt    Int
    | RTFloat  Double
    | RTString String
    | RTBool   Bool
    | RTList   [ RTVal ]
    | RTFunc   Exp        -- Maybe
    | RTNil 
    deriving (Show, Eq)


data Frame 
    = Frame { frameVars :: AssocMap String RTVal
            , frameFns  :: AssocMap String Exp    -- Cell pointers
            } deriving (Show, Eq)

data State 
    = State { stLast   :: RTVal
            , stStack  :: [ Frame ]
            } deriving (Show, Eq)

data Datum = Datum Exp State 
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
-- Int Cast
cast TInt (RTFloat d)       = RTInt $ floor d 
cast TInt (RTString s)      = RTInt $ length s
cast TInt (RTBool True)     = RTInt 1
cast TInt (RTBool False)    = RTInt 0
-- Float Cast
cast TFloat (RTInt i)       = RTFloat $ fromIntegral i 
cast TFloat a               = let RTInt i = cast TInt a 
                              in RTFloat (fromIntegral i)

rtParse :: Type -> String -> RTVal
rtParse TInt s   = RTInt (read s :: Int)
rtParse TFloat s = RTFloat (read s :: Double)
rtParse from str = undefined

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

fOfBop op a b = undefined 

rtLength :: RTVal -> Int
rtLength RTNil = 0
rtLength _ = 1

{--------------------------------:
    IO helpers
:--------------------------------}

rtPrintStdIo (RTInt a)    = print a
rtPrintStdIo (RTFloat a)  = print a
rtPrintStdIo (RTString a) = putStrLn a
rtPrintStdIo (RTBool a)   = print a
rtPrintStdIo RTNil        = print "Nil"
rtPrintStdIo a            = print a

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

step node@(Func l args f_body) s = do
    -- Define the function if is labeled
    let s' = case l of
                (Just k) -> if not ( funcExists k s ) 
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
                              = ( Frame { frameVars = map (assignVar (stLast s')) args
                                        , frameFns  = [] 
                                        }  
                                ) : stStack s' 
                           , stLast = RTNil
                           }
        in stepFExp f_body sWithArgs
    where 
        -- TODO: Tuple zipping
        assignVar a (Arg k t) = (k, cast t a)
        stepFExp FNil s       = pure [ Datum Nil s ]
        stepFExp (Single a) s = step a s

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
    rtPrintStdIo (cast t (stLast s))
    >> pure [Datum Nil (s { stLast = RTNil })]

runFlow :: (Maybe (Exp, a0)) -> IO ()
runFlow (Just (ast, _)) = step ast (State RTNil []) >>= aux >> (pure ())
    where 
        aux []           = pure () 
        aux ds           = (concat <$> sequence ( iter <$> ds )) 
                         >>= aux
        iter (Datum e s) = step e s
