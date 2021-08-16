{-# LANGUAGE TupleSections #-}

module Syntax where
import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Functor
import           Lexer
import           Module.Module
import           PreProcessor
import           System.IO
import           Text.Pretty.Simple

data ParseResult = ParseResult
  { prExp  :: Maybe Exp
  , prLeft :: Maybe String
  , prSrc  :: String
  }
  deriving (Show, Eq)

data CType
  = CSingle Int
  | CSlice Int (Maybe Int)
  deriving ( Show, Eq )

data AnchorType
  = AGen
  | AUnfold
  | AFold
  | AClosure
  | AKeep
  deriving ( Show, Eq )

data FMod
  = MNone
  | MMap
  | MKeepEnum
  | MKeep
  | MGen
  | MFold
  | MUnfold
  | MLet
  | MExport
  | MLazy
  deriving ( Show, Eq )

data Type
  = TInt
  | TFloat
  | TString
  | TBool
  | TList Type
  | TFunc Type Type
  | TAny
  deriving ( Show, Eq )

data Operator
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpExp
  | OpAnd
  | OpOr
  | OpMod
  | OpGt
  | OpGtEq
  | OpLt
  | OpLtEq
  | OpEq
  | OpNeq
  deriving ( Show, Eq )

data Arg = Arg String Type
  deriving (Show, Eq)

data FuncExp
  = Cond Exp Exp FuncExp
  | Single Exp
  | FNil
  deriving ( Show, Eq )

data IoExp
  = IoStdIn Type
  | IoStdOut Type
  | IoFileIn String Type
  | IoFileOut String Type
  deriving ( Show, Eq )

data Exp
  = Nil
  | BinOp Operator Exp Exp
  | LInt Int
  | LFloat Double
  | LBool Bool
  | LString String
  | LList [Exp]
  | LTuple [Exp]
  | Var String
  | Capture CType
  | Label String
  | Cell FMod Exp
  | Func (Maybe String) [Arg] Type FuncExp
  | Io IoExp
  | Flow Exp Exp
  | FRef String
  | Program Exp Exp
  | Anchor AnchorType Exp Exp
  | Casting Exp Type
  deriving ( Show, Eq )




{--------------------------------:
    Literals
:--------------------------------}

literal :: Parser Exp
literal = tuple <|> list <|> str <|> float <|> bool <|> int

int :: Parser Exp
int = LInt <$> num

float :: Parser Exp
float =
  LFloat
    <$> (do
          a <- num
          _ <- char '.'
          b <- natural <|> pure 0
          return (read (show a <> "." <> show b))
        )

bool :: Parser Exp
bool = LBool . read <$> (string "True" <|> string "False")

str :: Parser Exp
str = LString <$> enclosed (char '`') (char '`') (many beginning)
  where beginning = pre (\c -> isLatin1 c && c `notElem` "`")

list :: Parser Exp
list =
  LList
    <$> enclosed (token (char '['))
                 (token (char ']'))
                 (sepBy (token (char ',')) term)
    <|> token (char '[')
    *>  token (char ']')
    $>  LList []

tuple :: Parser Exp
tuple =
  LTuple
    <$> enclosed (token (char '('))
                 (token (char ')'))
                 (sepBy (token (char ';')) term)
    <|> token (char '(')
    *>  token (char ')')
    $>  LTuple []




{--------------------------------:
    Type annotations
:--------------------------------}

pType :: Parser Type
pType = _pFunc <|> simple
 where
  simple =
    (TInt <$ token (string "Int"))
      <|> (TFloat <$ token (string "Float"))
      <|> (TString <$ token (string "Str"))
      <|> (TBool <$ token (string "Bool"))
      <|> (TList <$> _pList)
      <|> (TAny <$ token (string "Any"))
  _pList =
    string "List"
      *>  enclosed (char '<') (char '>') (token pType)
      <|> string "List"
      $>  TAny
  _pFunc =
    do
      a <- token simple
      _ <- token (string "->")
      b <- token _pFunc
      return (TFunc a b)
    <|> simple




{--------------------------------:
    Expressions
:--------------------------------}

program :: Parser Exp
program = (Program <$> flow <*> program) <|> flow

expr :: Parser Exp
expr = term <|> flow

flow :: Parser Exp
flow =
  (Flow <$> token cell <* token (string "=>") <*> token flow)
    <|> (Flow <$> cell <*> pure Nil)

cell :: Parser Exp
cell =
  token fRef
    <|> (Cell <$> fMod <*> token fRef)
    <|> (Cell <$> (fMod <|> pure MNone) <*> enclosed
          (token (char '{'))
          (token (char '}'))
          innerExp
        )
  where innerExp = func <|> token fRef <|> expr

term :: Parser Exp
term =
  casting
    <|> enclosed (char '(') (char ')') (token term)
    <|> io
    <|> literal
    <|> binaryOp
    <|> token var

identifier :: Parser String
identifier =
  do
    a0 <- pre isLetter
    as <- some (pOr [isLetter, isDigit, (`elem` "_")])
    return (a0 : as)
  <|> some (pre isLetter)


casting :: Parser Exp
casting =
  Casting
    <$> token (enclosed (char '(') (char ')') (token subTerm) <|> subTerm)
    <*  token (string "::")
    <*> (enclosed (char '(') (char ')') (token pType) <|> token pType)
  where subTerm = io <|> literal <|> binaryOp <|> token var



{--------------------------------:
    Functions
:--------------------------------}

fMod :: Parser FMod
fMod =
        (MMap      <$ tString "map"   )
    <|> (MKeepEnum <$ tString "keep[]")
    <|> (MKeep     <$ tString "keep"  )
    <|> (MGen      <$ tString "gen"   )
    <|> (MFold     <$ tString "fold"  )
    <|> (MUnfold   <$ tString "unfold")
    <|> (MLet      <$ tString "let"   )
    <|> (MExport   <$ tString "export")
    <|> (MLazy     <$ tString "lazy")
    where tString = token . string

func :: Parser Exp
func =
  Func
    <$> optional (token label)
    <*> (token args <|> pure [])
    <*  token (char '.')
    <*> (token returnType <|> pure TAny)
    <*> token fBody

fRef :: Parser Exp
fRef = FRef <$> (char '~' *> token identifier)

label :: Parser String
label = char '~' *> identifier <* token (char ':')

arg :: Parser Arg
arg =
  (Arg <$> token identifier <*> maybeEnclosed (char '(') (char ')') (token pType))
    <|> (Arg <$> token identifier <*> pure TAny)

args :: Parser [Arg]
args = sepBy (token (char ',')) arg

returnType :: Parser Type
returnType = maybeEnclosed (char '(') (char ')') pType

fBody :: Parser FuncExp
fBody =
  (   Cond
    <$> token expr
    <*  token (char '|')
    <*> e
    <*> (fBody <|> pure FNil)
    )
    <|> (Single <$ token (char '|') <*> e)
    <|> (Single <$> e)
  where e = token (program <|> expr)




{--------------------------------:
    IO
:--------------------------------}

io :: Parser Exp
io = Io <$> (ioIn <|> ioOut)

ioIn :: Parser IoExp
ioIn =
  (IoFileIn <$> token path <* token (string "~>") <*> token pType)
    <|> (IoStdIn <$> (token (string "~>") *> pType))

ioOut :: Parser IoExp
ioOut =
  (IoFileOut <$> token path <* token (string "<~") <*> token pType)
    <|> (IoStdOut <$> (token (string "<~") *> pType))

path :: Parser String
path = do
  LString path <- str
  return path




{--------------------------------:
    Binary Ops
:--------------------------------}

operator :: Parser Operator
operator = foldl1 (<|>) $ aux <$> ops
 where
  aux (f, s) = f <$ string s
  ops =
    [ (OpAdd , "+")
    , (OpSub , "-")
    , (OpMul , "*")
    , (OpDiv , "/")
    , (OpExp , "^")
    , (OpAnd , "&&")
    , (OpOr  , "||")
    , (OpMod , "%")
    , (OpGtEq, ">=")
    , (OpGt  , ">")
    , (OpLtEq, "<=")
    , (OpLt  , "<")
    , (OpEq  , "==")
    , (OpNeq , "/=")
    ]

var :: Parser Exp
var =
  (   CSlice
    <$> (string "&" *> natural)
    <*  string ":"
    <*> optional natural
    <&> Capture
    )
    <|> (Capture . CSingle <$> (string "&" *> natural))
    <|> (Var <$> identifier)

binaryOp :: Parser Exp
binaryOp = BinOp <$> token operator <*> token term <*> token term




{--------------------------------:
    Helpers
:--------------------------------}

printAST :: ParseResult -> IO ()
printAST (ParseResult (Just a) _ _) = pPrint a

parseString :: String -> ParseResult
parseString input = parseResultOfRoot
  input
  (fixRootProgram $ runParser program (removeComments input))

parseFile :: String -> IO (Either String ParseResult)
parseFile path = second parseString <$> gatherFile path


fixRootProgram :: Maybe (Exp, String) -> Maybe (Exp, String)
fixRootProgram p@(Just (Program _ _, _)) = p
fixRootProgram (  Just (p          , r)) = Just (Program p Nil, r)
fixRootProgram a                         = a

parseResultOfRoot :: String -> Maybe (Exp, String) -> ParseResult
parseResultOfRoot src root
  | Just (ast, r) <- root = ParseResult (Just ast) (Just r) src
  | otherwise             = ParseResult Nothing Nothing src
