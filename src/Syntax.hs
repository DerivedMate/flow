module Syntax where
import Lexer
import Control.Applicative
import Control.Monad
import Data.List
import Data.Char
import Text.Pretty.Simple (pPrint)

{--------------------------------:
    Examples
:--------------------------------}
{----:
  Factorial:

  {(~> Int; 1)} 
  => { ~fact: a (Int), b (Int) = 
        > a 1 | {(- a 1 ; * a b)} => ~fact 
              | { b } 
     } 
  => {<~ Int}
:----}

data FMod 
  = MNone
  | MMap
  | MKeep
  | MGen
  deriving Show

data Type 
  = TInt
  | TFloat
  | TString
  | TBool
  | TList Type
  | TFunc Type Type
  deriving Show

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
  deriving Show

data Arg 
  = Arg String Type
  deriving Show

data FuncExp 
  = Cond Exp Exp FuncExp
  | Single Exp 
  | FNil
  deriving Show

data IoExp
  = IoStdIn Type
  | IoStdOut Type
  | IoFileIn String Type
  | IoFileOut String Type
  deriving Show

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
  | Label String
  | Cell FMod Exp
  | Func (Maybe String) [Arg] FuncExp {- FuncBody -}
  | Io IoExp
  | Flow Exp Exp
  | FRef String
  | Program Exp Exp

  deriving Show




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
str = LString <$> enclosed (char '`') (char '`') (many (token beginning))
  where 
    beginning = space <|> letter 
            <|> nat   <|> mark
            <|> pre isPunctuation
    _str      = do
                  b   <- beginning
                  end <- _str
                  return (b : end)
            <|> (pure <$> beginning)

list :: Parser Exp
list = LList <$> enclosed (token (char '[')) (token (char ']')) (sepBy (token (char ',')) term )

tuple :: Parser Exp
tuple = LTuple 
    <$> enclosed 
          (token (char '(')) 
          (token (char ')')) 
          (sepBy (char ';') term )




{--------------------------------:
    Type annotations
:--------------------------------}

pType :: Parser Type
pType = _pFunc <|> simple
  where 
    simple =  (TInt    <$  token (string "Int"))
          <|> (TFloat  <$  token (string "Float"))
          <|> (TString <$  token (string "Str"))
          <|> (TBool   <$  token (string "Bool"))
          <|> (TList   <$> _pList)
    _pList = string "List" *> enclosed (char '<') (char '>') (token pType)
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
       <|> flow

expr :: Parser Exp
expr = binaryOp <|> flow <|> term <|> fRef

flow :: Parser Exp 
flow = (Flow <$> token cell <* token (string "=>") <*> token flow)
    <|> cell

cell :: Parser Exp
cell =  
    (Cell MNone <$> token fRef)
    <|> ( Cell <$> (fMod <|> pure MNone) 
               <*> enclosed 
                     (token (char '{')) 
                     (token (char '}')) 
                     (func <|> expr)
        )

term :: Parser Exp
term = enclosed (char '(') (char ')') (token term)
     <|> binaryOp 
     <|> token var
     <|> literal
     <|> io

identifier :: Parser String
identifier = do
   a0 <- pre isLetter 
   as <- some (pOr [isLetter, isDigit])
   return (a0 : as)
  <|> some (pre isLetter)




{--------------------------------:
    Functions
:--------------------------------}

fMod :: Parser FMod
fMod =  (MMap  <$ token ( string "map"  ))
    <|> (MKeep <$ token ( string "keep" ))
    <|> (MGen  <$ token ( string "gen"  ))

func :: Parser Exp
func = 
  Func <$> optional (token label)
       <*> token args
       <*  token (char '=')
       <*> token fBody

fRef :: Parser Exp
fRef = FRef <$> (char '~' *> token identifier)

label :: Parser String
label = char '~' *> identifier <* token (char ':')

arg :: Parser Arg
arg = 
  Arg <$> token identifier
      <*> enclosed (char '(') (char ')') (token pType)

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
         <$> path
         <*  token (string "~>")
         <*> pType
       ) <|> (IoStdIn <$> (token (string "~>") *> pType))

ioOut :: Parser IoExp
ioOut = ( IoFileIn 
          <$> path
          <*  token (string "<~")
          <*> pType
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
          , ( OpGt, ">")
          , ( OpGtEq, ">=")
          , ( OpLt, "<")
          , ( OpLtEq, "<=")
          , ( OpEq, "==")
          , ( OpNeq, "/=")
          ]

var :: Parser Exp
var = Var <$> identifier

binaryOp :: Parser Exp
binaryOp = 
  BinOp <$> token operator
        <*> token term
        <*> token term




{--------------------------------:
    Helpers
:--------------------------------}

printTree :: Maybe (Exp, String) -> IO ()
printTree (Just (a, _)) = aux 0 a
  where
    aux :: Int -> Exp -> IO ()
    aux indent (BinOp op a b) = 
      putStrLn (replicate (indent * 2) ' ' <> "Op: " <> show op)
      >> aux (indent + 1) a
      >> aux (indent + 1) b
    aux indent a = putStrLn $ replicate (indent * 2) ' ' <> show a

printAST :: Maybe (Exp, String) -> IO ()
printAST (Just (a, _)) = pPrint a
