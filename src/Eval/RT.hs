module Eval.RT where

import           Control.Applicative
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import           Helper.AssocMap
import           Syntax



data RTVal
    = RTInt    Int
    | RTFloat  Double
    | RTString String
    | RTBool   Bool
    | RTList   [ RTVal ]
    | RTTuple  [ RTVal ]
    | RTFunc   Exp
    | RTNil

instance Show RTVal where
    show (RTInt    a) = show a
    show (RTFloat  a) = show a
    show (RTString a) = "`" <> a <> "`"
    show (RTBool   a) = show a
    show (RTList   l) = "[" <> intercalate ", " (map show l) <> "]"
    show (RTTuple  t) = "(" <> intercalate ", " (map show t) <> ")"
    show (RTFunc (Cell _ (Func l args _))) =
        "~" <> fromMaybe "anonymous" l <> ": " <> intercalate
            ", "
            (map show args)
    show RTNil      = "nil"
    show (RTFunc a) = show a

instance Eq RTVal where
    RTNil           == RTNil = True

    a@(RTInt    aa) == b     = let RTInt bb = unifyType a b in aa == bb
    a@(RTFloat  aa) == b     = let RTFloat bb = unifyType a b in aa == bb
    a@(RTString aa) == b     = let RTString bb = unifyType a b in aa == bb
    a@(RTBool   aa) == b     = let RTBool bb = unifyType a b in aa == bb
    a@(RTList   aa) == b     = let RTList bb = unifyType a b in aa == bb
    a@(RTTuple  aa) == b     = let RTTuple bb = unifyType a b in aa == bb
    a@(RTFunc   aa) == b     = let RTFunc bb = unifyType a b in aa == bb
    RTNil           == b     = False

instance Ord RTVal where
    a@(RTInt    aa) <= b = let RTInt bb = unifyType a b in aa <= bb
    a@(RTFloat  aa) <= b = let RTFloat bb = unifyType a b in aa <= bb
    a@(RTString aa) <= b = let RTString bb = unifyType a b in aa <= bb
    a@(RTBool   aa) <= b = let RTBool bb = unifyType a b in aa <= bb
    a@(RTList   aa) <= b = let RTList bb = unifyType a b in aa <= bb
    a@(RTTuple  aa) <= b = let RTTuple bb = unifyType a b in aa <= bb
    (  RTFunc   _ ) <= b = False
    RTNil           <= b = True


type Frame = AssocMap String RTVal

data State = State
    { stLast  :: RTVal
    , stStack :: [Frame]
    }
    deriving (Show, Eq)

data Datum = Datum
    { datumExp   :: Exp
    , datumState :: State
    }
    deriving (Show, Eq)

{--------------------------------:
    Type Operations
:--------------------------------}

cast :: Type -> RTVal -> RTVal
-- Cast Id
cast TInt      a@(RTInt    _ ) = a
cast TFloat    a@(RTFloat  _ ) = a
cast TString   a@(RTString _ ) = a
cast TBool     a@(RTBool   _ ) = a
cast (TList t) (  RTList   ls) = RTList (map (cast t) ls)

-- Int Cast
cast TInt      (  RTFloat  d ) = RTInt $ floor d
cast TInt (RTString s) | all isNumber s = RTInt (read s)
                       | otherwise      = RTInt (length s)

cast TInt         (RTBool True )           = RTInt 1
cast TInt         (RTBool False)           = RTInt 0

-- Float Cast
cast TFloat       (RTInt  i    )           = RTFloat $ fromIntegral i
cast TFloat a = let RTInt i = cast TInt a in RTFloat (fromIntegral i)
-- Truthiness
cast TBool        (RTInt    a)             = RTBool (a /= 0)
cast TBool        (RTFloat  f)             = RTBool (f /= 0.0)
cast TBool        (RTString s)             = RTBool (s /= "")
cast TBool        (RTList   l)             = RTBool (not $ null l)

-- String Cast
cast TString      a                        = RTString (show a)

cast (TFunc t ts) (RTFunc (Func l args e)) = RTFunc
    (Cell MNone (Func l args' e))
  where
    args' = zipWith aux (tFuncTypes ts [t]) args
    tFuncTypes (TFunc t f) acc = tFuncTypes f (t : acc)
    tFuncTypes t           acc = t : acc
    aux t (Arg n _) = Arg n t

cast t a = error ("Unmatched type cast: " <> show a <> " -> " <> show t)

rtParse :: Type -> String -> RTVal
rtParse TInt   s   = RTInt (read s :: Int)
rtParse TFloat s   = RTFloat (read s :: Double)
rtParse from   str = undefined

typeOfRt :: RTVal -> Type
typeOfRt (  RTInt    _                             ) = TInt
typeOfRt (  RTFloat  _                             ) = TFloat
typeOfRt (  RTString _                             ) = TString
typeOfRt (  RTBool   _                             ) = TBool
typeOfRt (  RTList   (l    : _                    )) = TList (typeOfRt l)
typeOfRt r@(RTFunc   (Cell _ (Func _ (a : args) _))) = foldr
    aux
    (TFunc (argType a))
    args
    TAny
    where aux a f = TFunc (f (argType a))

typeOfRt r = error "Unknown typeOfRt"


-- Cast the latter to the type of the former
unifyType :: RTVal -> RTVal -> RTVal
unifyType a = cast (typeOfRt a)

fOfBop :: Operator -> RTVal -> RTVal -> RTVal
-- Int x Int
fOfBop OpAdd (RTInt a) (RTInt b) = RTInt $ a + b
fOfBop OpSub (RTInt a) (RTInt b) = RTInt $ a - b
fOfBop OpMul (RTInt a) (RTInt b) = RTInt $ a * b
fOfBop OpDiv (RTInt a) (RTInt b) =
    RTFloat $ (fromIntegral a :: Double) / (fromIntegral b :: Double)
fOfBop OpExp (RTInt a) (RTInt b) =
    RTFloat $ (fromIntegral a :: Double) ** (fromIntegral b :: Double)

-- Float x Float
fOfBop OpAdd (  RTFloat a) (  RTFloat b) = RTFloat $ a + b
fOfBop OpSub (  RTFloat a) (  RTFloat b) = RTFloat $ a - b
fOfBop OpMul (  RTFloat a) (  RTFloat b) = RTFloat $ a * b
fOfBop OpExp (  RTFloat a) (  RTFloat b) = RTFloat $ a ** b

-- Float x Int / Int x Float
fOfBop op    a@(RTFloat _) b@(RTInt   _) = fOfBop op a (cast TFloat b)
fOfBop op    a@(RTInt   _) b@(RTFloat _) = fOfBop op a (cast TInt b)

-- Modulo
fOfBop OpMod (  RTInt   a) (  RTInt   b) = RTInt (a `mod` b)
fOfBop OpMod a b = fOfBop OpMod (cast TInt a) (cast TInt b)

-- String x String
fOfBop OpAdd (RTString a)  (RTString b)  = RTString (a <> b)
-- String Ops
fOfBop OpMul (RTString a) (RTInt n) | n > 0 = RTString $ concat $ replicate n a
                                    | otherwise = RTString ""

-- Truthy
-- Truthy: Logic
fOfBop OpAnd a b =
    let RTBool a' = cast TBool a
        RTBool b' = cast TBool b
    in  RTBool (a' && b')
fOfBop OpOr a b =
    let RTBool a' = cast TBool a
        RTBool b' = cast TBool b
    in  RTBool (a' || b')
-- Truthy: Comparison
fOfBop OpGt   a b = RTBool (a > b)
fOfBop OpGtEq a b = RTBool (a >= b)
fOfBop OpLt   a b = RTBool (a < b)
fOfBop OpLtEq a b = RTBool (a <= b)
fOfBop OpEq   a b = RTBool (a == b)
fOfBop OpNeq  a b = RTBool (a /= b)
-- General
fOfBop op     a b = fOfBop op a (unifyType a b)

rtLength :: RTVal -> Int
rtLength RTNil         = 0
rtLength (RTTuple  ts) = length ts
rtLength (RTString s ) = length s
rtLength _             = 1

{--------------------------------:
    Func Helpers
:--------------------------------}

funcExists :: String -> State -> Bool
funcExists k s = assocExists (concat $ stStack s) k

funcFind :: String -> State -> Exp
funcFind k s = toExp $ aux (concat $ stStack s)
  where
    toExp (RTFunc f) = f
    toExp _          = Nil
    aux [] = RTNil
    aux ((k', v) : ds) | k == k'   = v
                       | otherwise = aux ds

funcArgs :: Exp -> [Arg]
funcArgs (Func _ as _) = as
funcArgs (Cell _ f   ) = funcArgs f
funcArgs e             = error ("call funcArgs of non-func: " <> show e)

argName :: Arg -> String
argName (Arg n _) = n

argType :: Arg -> Type
argType (Arg _ t) = t

assignVars :: RTVal -> [Arg] -> [(String, RTVal)]
assignVars (RTTuple ts) args    = zipWith bindVar ts args
assignVars v            (a : _) = [bindVar v a]
assignVars _            []      = []

bindVar :: RTVal -> Arg -> (String, RTVal)
bindVar v (Arg k t) = (k, cast t v)

deleteVars :: [String] -> [Frame] -> [Frame]
deleteVars [] frames = frames
deleteVars _  []     = []
deleteVars (v : vs) (f : fs) | isEmpty f' = deleteVars vs fs
                             | otherwise  = deleteVars vs (f' : fs)
  where
    f'      = vrs'
    vrs'    = deleteBy ((==) `on` fst) (v, RTNil) f
    isEmpty = null

getVarF :: String -> Frame -> Maybe RTVal
getVarF = lookup

getVar :: String -> State -> Maybe RTVal
getVar k s = foldl aux Nothing (stStack s) where aux a f = a <|> getVarF k f

correctLast :: [Arg] -> [Datum] -> [Datum]
correctLast args [] = []
correctLast args ds = fmap aux ds
  where
    aux d =
        let prevState = datumState d
            stack'    = deleteVars (fmap argName args) (stStack prevState)
        in  d { datumState = prevState { stStack = stack' } }

{--------------------------------:
    Tuple Helpers
:--------------------------------}

deTuple :: RTVal -> [RTVal]
deTuple (RTTuple a)  = a
deTuple RTNil        = []
deTuple (RTList   a) = a
deTuple (RTString s) = map (RTString . (: [])) s
deTuple a            = [a]

wrapperOfState :: RTVal -> ([RTVal] -> RTVal)
wrapperOfState (RTTuple _) = RTTuple
wrapperOfState _           = RTList
