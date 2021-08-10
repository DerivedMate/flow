{-# LANGUAGE TupleSections #-}

module Syntax where
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Functor
import           Lexer
import           PreProcessor
import           System.IO
import           Text.Pretty.Simple

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

data Arg
  = Arg String Type
  deriving ( Show, Eq )

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
  | Func (Maybe String) [Arg] FuncExp
  | Io IoExp
  | Flow Exp Exp
  | FRef String
  | Program Exp Exp
  | Anchor AnchorType Exp Exp
  deriving ( Show, Eq )




{--------------------------------:
    Literals
:--------------------------------}

literal :: Parser Exp
literal = tuple <|> list <|> str <|> float <|> bool <|> int

int :: Parser Exp
int = LInt <$> num

float :: Parser Exp
float = LFloat <$>
  (  do
      a <- num
      _ <- char '.'
      b <- natural <|> pure 0
      return (read ( show a <> "." <> show b ))
  )

bool :: Parser Exp
bool = LBool . read <$> (string "True" <|> string "False")

str :: Parser Exp
str = LString <$> enclosed (char '`') (char '`') (many beginning)
  where
    beginning = pre (\c -> isLatin1 c && c `notElem` "`")

list :: Parser Exp
list = LList <$> enclosed
                  (token (char '['))
                  (token (char ']'))
                    (sepBy (token (char ','))
                      term
                    )
     <|> token (char '[') *> token (char ']') $> LList []

tuple :: Parser Exp
tuple = LTuple <$> enclosed
                    (token (char '('))
                    (token (char ')'))
                    (sepBy (token (char ';'))
                     term
                    )
      <|> token (char '(') *> token (char ')') $> LTuple []




{--------------------------------:
    Type annotations
:--------------------------------}

pType :: Parser Type
pType = _pFunc <|> simple
  where
    simple =  (TInt    <$  token (string "Int"  ))
          <|> (TFloat  <$  token (string "Float"))
          <|> (TString <$  token (string "Str"  ))
          <|> (TBool   <$  token (string "Bool" ))
          <|> (TList   <$> _pList                )
          <|> (TAny    <$  token (string "Any"  ))
    _pList =  string "List" *> enclosed (char '<') (char '>') (token pType)
          <|> string "List" $> TAny
    _pFunc =
          do
            a <- token simple
            _ <- token ( string "->" )
            b <- token _pFunc
            return ( TFunc a b )
          <|> simple




{--------------------------------:
    Expressions
:--------------------------------}

program :: Parser Exp
program = (Program <$> flow <*> program)
       <|> token flow

expr :: Parser Exp
expr = term <|> flow

flow :: Parser Exp
flow = (Flow <$> token cell <* token (string "=>") <*> token flow)
    <|> (Flow <$> cell <*> pure Nil)

cell :: Parser Exp
cell =
    token fRef
    <|> ( Cell <$> fMod <*> token fRef )
    <|> ( Cell <$> (fMod <|> pure MNone)
               <*> enclosed
                     (token (char '{'))
                     (token (char '}'))
                     (func <|> token fRef <|> expr)
        )

term :: Parser Exp
term = enclosed (char '(') (char ')') (token term)
     <|> io
     <|> literal
     <|> binaryOp
     <|> token var

identifier :: Parser String
identifier = do
   a0 <- pre isLetter
   as <- some (pOr [isLetter, isDigit, (`elem` "_")])
   return (a0 : as)
  <|> some (pre isLetter)




{--------------------------------:
    Functions
:--------------------------------}

fMod :: Parser FMod
fMod =  (MMap      <$ token ( string "map"    ))
    <|> (MKeepEnum <$ token ( string "keep[]" ))
    <|> (MKeep     <$ token ( string "keep"   ))
    <|> (MGen      <$ token ( string "gen"    ))
    <|> (MFold     <$ token ( string "fold"   ))
    <|> (MUnfold   <$ token ( string "unfold" ))

func :: Parser Exp
func =
  Func <$> optional (token label)
       <*> (token args <|> pure [])
       <*  token (char '=')
       <*> token fBody

fRef :: Parser Exp
fRef = FRef <$> (char '~' *> token identifier)

label :: Parser String
label = char '~' *> identifier <* token (char ':')

arg :: Parser Arg
arg = (Arg <$> token identifier
           <*> enclosed (char '(') (char ')') (token pType))
  <|> (Arg <$> token identifier <*> pure TAny)

args :: Parser [Arg]
args = sepBy (token (char ',')) arg

fBody :: Parser FuncExp
fBody =
  ( Cond   <$> token expr
           <*  token (char '|')
           <*> token expr
           <*> (fBody <|> pure FNil)
  ) <|> (
    Single <$  token (char '|')
           <*> token expr
  ) <|> (
    Single <$> token expr
  )




{--------------------------------:
    IO
:--------------------------------}

io :: Parser Exp
io = Io <$> (ioIn <|> ioOut)

ioIn :: Parser IoExp
ioIn = ( IoFileIn
         <$> token path
         <*  token (string "~>")
         <*> token pType
       ) <|> (IoStdIn <$> (token (string "~>") *> pType))

ioOut :: Parser IoExp
ioOut = ( IoFileOut
          <$> token path
          <*  token (string "<~")
          <*> token pType
        ) <|> (IoStdOut <$> (token (string "<~") *> pType))

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
    ops = [ ( OpAdd, "+")
          , ( OpSub, "-")
          , ( OpMul, "*")
          , ( OpDiv, "/")
          , ( OpExp, "^")
          , ( OpAnd, "&&")
          , ( OpOr, "||")
          , ( OpMod, "%")
          , ( OpGtEq, ">=")
          , ( OpGt, ">")
          , ( OpLtEq, "<=")
          , ( OpLt, "<")
          , ( OpEq, "==")
          , ( OpNeq, "/=")
          ]

var :: Parser Exp
var =
      (CSlice 
        <$> (string "&" *> natural) 
        <* string ":" 
        <*> optional natural
        <&> Capture
        )
  <|> (Capture . CSingle <$> (string "&" *> natural))
  <|> (Var <$> identifier)

binaryOp :: Parser Exp
binaryOp =
  BinOp <$> token operator
        <*> token term
        <*> token term




{--------------------------------:
    Helpers
:--------------------------------}

printAST :: Maybe (Exp, String) -> IO ()
printAST (Just (a, _)) = pPrint a

parseString :: String -> Maybe (Exp, String)
parseString input = fixRootProgram $ runParser program (removeComments input)

parseFile :: String -> IO (Maybe (Exp, String))
parseFile src = do
  f <- openFile src ReadMode
  parseString <$> hGetContents f


fixRootProgram :: Maybe (Exp, String) -> Maybe (Exp, String)
fixRootProgram p@(Just (Program _ _, _))
  = p
fixRootProgram (Just (p, r))
  = Just (Program p Nil, r)
fixRootProgram a
  = a
