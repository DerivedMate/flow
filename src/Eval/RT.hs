module Eval.RT where

import           Control.Applicative
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import           Debug.Pretty.Simple
import           Helper.AssocMap
import           Lexer
import           Syntax

data RTArg = RTArg
  { rtArgLabel :: String
  , rtArgType  :: Type
  , rtArgValue :: RTVal
  }
  deriving Eq

instance Show RTArg where
  show a
    | RTNil <- rtArgValue a = rtArgLabel a <> "(" <> show (rtArgType a) <> ")"
    | otherwise = rtArgLabel a <> "(" <> show (rtArgType a) <> ") = " <> show
      (rtArgValue a)

rtArgOfArg :: Arg -> RTArg
rtArgOfArg (Arg n t) = RTArg n t RTNil

argOfRtArg :: RTArg -> Arg
argOfRtArg (RTArg n t _) = Arg n t

data RTVal
    = RTInt    Int
    | RTFloat  Double
    | RTString String
    | RTBool   Bool
    | RTList   [ RTVal ]
    | RTTuple  [ RTVal ]
    | RTFunc   (Maybe String) [RTArg] Type FuncExp
    | RTNil

instance Show RTVal where
  show (RTInt    a) = show a
  show (RTFloat  a) = show a
  show (RTString a) = a
  show (RTBool   a) = show a
  show (RTList   l) = "[" <> intercalate ", " (map show l) <> "]"
  show (RTTuple  t) = "(" <> intercalate ", " (map show t) <> ")"
  show (RTFunc l args rt _) =
    "~"
      <> fromMaybe "anonymous" l
      <> ": "
      <> intercalate ", " (map show args)
      <> ". "
      <> show rt
  show RTNil = "nil"

instance Eq RTVal where
  RTNil           == RTNil = True
  b               == RTNil = False
  RTNil           == b     = False

  a@(RTInt    aa) == b     = let RTInt bb = unifyType a b in aa == bb
  a@(RTFloat  aa) == b     = let RTFloat bb = unifyType a b in aa == bb
  a@(RTString aa) == b     = let RTString bb = unifyType a b in aa == bb
  a@(RTBool   aa) == b     = let RTBool bb = unifyType a b in aa == bb
  a@(RTList   aa) == b     = let RTList bb = unifyType a b in aa == bb
  a@(RTTuple  aa) == b     = let RTTuple bb = unifyType a b in aa == bb
  a@(RTFunc la aa fa ra) == b =
    let RTFunc lb ab fb rb = unifyType a b
    in  la == lb && aa == ab && fa == fb && ra == rb

instance Ord RTVal where
  a@(RTInt    aa) <= b = let RTInt bb = unifyType a b in aa <= bb
  a@(RTFloat  aa) <= b = let RTFloat bb = unifyType a b in aa <= bb
  a@(RTString aa) <= b = let RTString bb = unifyType a b in aa <= bb
  a@(RTBool   aa) <= b = let RTBool bb = unifyType a b in aa <= bb
  a@(RTList   aa) <= b = let RTList bb = unifyType a b in aa <= bb
  a@(RTTuple  aa) <= b = let RTTuple bb = unifyType a b in aa <= bb
  RTFunc{}        <= b = False
  RTNil           <= b = True


type Frame = AssocMap String RTVal

data State = State
  { stLast  :: RTVal
  , stStack :: [Frame]
  }
  deriving (Show, Eq)

emptyState :: State
emptyState = State { stLast = RTNil, stStack = [] }

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
cast TInt (RTString s) | all isNumber s 
                       , not (null s)
                       = RTInt (read s)
                       | otherwise      = RTInt (length s)

cast TInt         (RTBool True )        = RTInt 1
cast TInt         (RTBool False)        = RTInt 0

cast TInt         (RTList ls   )        = RTInt (length ls)

-- Float Cast
cast TFloat       (RTInt  i    )        = RTFloat $ fromIntegral i
cast TFloat a = let RTInt i = cast TInt a in RTFloat (fromIntegral i)
-- Truthiness
cast TBool        (RTInt    a)          = RTBool (a /= 0)
cast TBool        (RTFloat  f)          = RTBool (f /= 0.0)
cast TBool (RTString s) = RTBool (s `notElem` ["", "\r\n", "\r", "\n"])
cast TBool        (RTList   l)          = RTBool (not $ null l)
cast TBool        RTNil                 = RTBool False

-- String Cast
cast TString      a                     = RTString (show a)

cast (TFunc t ts) (RTFunc l args rt fe) = RTFunc l args' (last types) fe
 where
  args' = zipWith aux types args
  types = tFuncTypes ts [t]
  tFuncTypes (TFunc t f) acc = tFuncTypes f (t : acc)
  tFuncTypes t           acc = reverse $ t : acc
  aux t (RTArg n _ v) | RTNil <- v = RTArg n t v
                      | otherwise  = RTArg n t (cast t v)

cast TAny a = a
cast t    a = error ("Unmatched type cast: " <> show a <> " -> " <> show t)

rtParse :: Type -> String -> RTVal
rtParse t s
  | null s, TList _ <- t = RTList []
  | null s = RTNil
  | Just pr <- r = rtOfExp t (prResult pr)
  | Just pe <- e = error $ show pe
  | otherwise = error ("Failed to rtParse: " <> s <> "; as: " <> show t)
 where
  parserOfType :: Type -> Parser Exp
  parserOfType t
    | TInt <- t = flInt
    | TFloat <- t = flFloat
    | TString <- t = LString
    <$> many (qcProp (\c -> isLatin1 c && (c `notElem` definiteSeps)))
    | TBool <- t = flBool
    | (TList tt) <- t = LList
    <$> qcSeparatedBy (qcToken (qcProp (`elem` definiteSeps))) (qcToken (parserOfType tt))
    | TAny <- t = flExpr
    | (TFunc _ _) <- t = flFunc
    where definiteSeps = ",;.\t"

  (e, r) = flDistillReturn $ runParser (parserOfType t) (qcCtxOfString s)


rtOfExp :: Type -> Exp -> RTVal
rtOfExp _ (  LTuple  []       ) = RTNil
rtOfExp _ (  LInt    a        ) = RTInt a
rtOfExp _ (  LFloat  a        ) = RTFloat a
rtOfExp _ (  LBool   a        ) = RTBool a
rtOfExp _ (  LString a        ) = RTString a
rtOfExp t (  LList   as       ) = RTList (fmap (rtOfExp t) as)
rtOfExp t f@(Func l args rt fe) = cast t $ RTFunc l (map rtArgOfArg args) rt fe

expOfRt :: RTVal -> Exp
expOfRt (RTInt    a        ) = LInt a
expOfRt (RTFloat  a        ) = LFloat a
expOfRt (RTString a        ) = LString a
expOfRt (RTBool   a        ) = LBool a
expOfRt (RTTuple  ls       ) = LTuple $ map expOfRt ls
expOfRt (RTList   ls       ) = LList $ map expOfRt ls
expOfRt (RTFunc l args rt f) = Func l [ Arg n v | RTArg n v _ <- args ] rt f
expOfRt RTNil                = Nil


typeOfRt :: RTVal -> Type
typeOfRt (  RTInt    _              ) = TInt
typeOfRt (  RTFloat  _              ) = TFloat
typeOfRt (  RTString _              ) = TString
typeOfRt (  RTBool   _              ) = TBool
typeOfRt (  RTList   (l : _)        ) = TList (typeOfRt l)
typeOfRt r@(RTFunc l (a : args) rt f) = foldr aux (TFunc (rtArgType a)) args rt
  where aux a f = TFunc (f (rtArgType a))

typeOfRt r = pTraceShow r $ error ("Unknown typeOfRt: " <> show r)


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
fOfBop OpMul (RTString a) (RTInt n) | n > 0 = RTString . concat $ replicate n a
                                    | otherwise = RTString ""

-- List x List
fOfBop OpAdd (RTList as) (RTList bs) = RTList (as <> bs)
fOfBop OpSub (RTList as) (RTList bs) = RTList [ a | a <- as, a `notElem` bs ]
fOfBop OpMul (RTList as) (RTList bs) =
  RTList [ RTTuple [a, b] | a <- as, b <- bs ]

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


funcArgs :: Exp -> [Arg]
funcArgs (Func _ as _ _) = as
funcArgs (Cell _ f     ) = funcArgs f
funcArgs e               = error ("call funcArgs of non-func: " <> show e)

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

bindArgs :: RTVal -> [Arg] -> [RTArg]
bindArgs l args =
  let tArgs = map (\(Arg n t) -> (n, t)) args
  in  [ RTArg n t v | ((n, t), v) <- tArgs `zip` splatLast l ]

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

getCaptureSingle :: Int -> State -> Either String RTVal
getCaptureSingle i s
  | i < length scope = Right $ scope !! i
  | otherwise = Left
    (  "Cannot capture &"
    <> show i
    <> ": index exceeds scope length ("
    <> show (length scope)
    <> ")"
    )
  where scope = (deTuple . stLast) s

getCaptureSlice :: Int -> Maybe Int -> State -> Either String RTVal
getCaptureSlice f t s = Right $ (wrapperOfState . stLast) s (subset scope)
 where
  t' = fromMaybe (-1) t
  subset | t' /= -1  = take (t' - f + 1) . drop f
         | otherwise = drop f
  scope      = (deTuple . stLast) s
  lastLength = rtLength . stLast

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

splatLast :: RTVal -> [RTVal]
splatLast (RTTuple ls) = ls
splatLast r            = [r]

wrapperOfState :: RTVal -> ([RTVal] -> RTVal)
wrapperOfState (RTTuple  _) = RTTuple
wrapperOfState (RTString _) = RTString . concatMap stringify
  where stringify r = let RTString s = cast TString r in s
wrapperOfState _ = RTList
