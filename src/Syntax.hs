module Syntax where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Lexer
import           Module.Module
import           PreProcessor
import           System.IO
import           Text.Pretty.Simple
import Debug.Pretty.Simple

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
  | TTuple [Type]
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

flLiteral :: Parser Exp
flLiteral = flTuple <+> flList <+> flStr <+> flFloat <+> flBool <+> flInt

flInt :: Parser Exp
flInt = LInt <$> qcNum

flFloat :: Parser Exp
flFloat =
  LFloat
    <$> (do
          a <- qcNum
          _ <- qcChar '.'
          b <- qcNatural <+> pure 0
          return (read $ show a <> "." <> show b)
        )

flBool :: Parser Exp
flBool = LBool <$> ((qcStr "True" $> True) <+> (qcStr "False" $> False))

flStr :: Parser Exp
flStr =
  LString <$> qcEnclosedBy (qcChar '`') (qcChar '`') (qcMany $ qcProp (/= '`'))

flTuple :: Parser Exp
flTuple = (LTuple <$> qcEnclosedBy (qcChar '(')
                                  (qcChar ')')
                                  (qcSeparatedBy (qctChar ',') flTerm))
       <+> (qctChar '(' *> qctChar ')' $> Nil)

flList :: Parser Exp
flList = (LList <$> qcEnclosedBy (qcChar '[')
                                (qcChar ']')
                                (qcSeparatedBy (qctChar ',') flTerm))
      <+> (LList <$> (qctChar '[' *> qctChar ']' $> []) )



{--------------------------------:
    Types
:--------------------------------}

flType :: Parser Type
flType = _func <+> _simple <+> _list
 where
  _simple =
    (TInt <$ qctStr "Int")
      <@> (TFloat <$ qctStr "Float")
      <@> (TString <$ qctStr "Str")
      <@> (TString <$ qctStr "Bool")
      <@> (TAny <$ qctStr "Any")
  _list     = TList <$> qcEnclosedBy (qctChar '[') (qctChar ']') flType
  _func     = (TFunc <$> __notFunc <* qctStr "->" <*> _func) <@> _simple
  __notFunc = qcToken (_list <@> _simple)



{--------------------------------:
    Expressions
:--------------------------------}

flProgram :: Parser Exp
flProgram = (Program <$> flFlow <*> flProgram) <+> flFlow

flExpr :: Parser Exp
flExpr = flTerm <@> flFlow

flFlow :: Parser Exp
flFlow =
  (Flow <$> qcToken flCell <* qctStr "=>" <*> flFlow)
    <+> (Flow <$> flCell <*> pure Nil)

flCell :: Parser Exp
flCell =
  qcToken flFRef
    <+> (Cell <$> flFMod <*> qcToken flFRef)
    <+> (   Cell
        <$> (flFMod <+> pure MNone)
        <*> qcEnclosedBy (qctChar '{') (qctChar '}') _innerExp
        )
  where _innerExp = flFunc <+> flExpr

flTerm :: Parser Exp
flTerm =
  flCast
    <+> qcEnclosedBy (qcToken (qcChar '(')) (qcToken (qcChar ')')) flTerm
    <+> flIo
    <+> flLiteral
    <+> flBinaryOp
    <+> flVar

flId :: Parser String
flId = (:) <$> qcProp _fst <*> qcMany
  (qcProp (\c -> or [ f c | f <- [isLetter, isDigit, (`elem` "_'")] ]))
  where _fst c = isLetter c || c `elem` "_'"

flCast :: Parser Exp
flCast =
  Casting
    <$> qcMaybeEnclosedBy (qctChar '(') (qctChar ')') (qcToken _subTerm)
    <*  qctStr "::"
    <*> qcMaybeEnclosedBy (qctChar '(') (qctChar ')') (qcToken flType)
  where _subTerm = flIo <+> flLiteral <+> flBinaryOp <+> flVar




{--------------------------------:
    Functions
:--------------------------------}

flFMod :: Parser FMod
flFMod =
  (MMap <$ qctStr "map")
    <|> (MKeepEnum <$ qctStr "keep[]")
    <|> (MKeep <$ qctStr "keep")
    <|> (MGen <$ qctStr "gen")
    <|> (MFold <$ qctStr "fold")
    <|> (MUnfold <$ qctStr "unfold")
    <|> (MLet <$ qctStr "let")
    <|> (MExport <$ qctStr "export")
    <|> (MLazy <$ qctStr "lazy")

flFunc :: Parser Exp
flFunc =
  Func
    <$> ((Just <$> qcToken flLabel) <@> pure Nothing)
    <*> (qcToken flArgs <+> pure [])
    <*  qctChar '.'
    <*> (qcToken flReturnType <+> pure TAny)
    <*> qcToken flFBody

flFRef :: Parser Exp
flFRef = FRef <$> flLabel

flLabel :: Parser String
flLabel = qcChar '~' *> qcToken flId

flArg :: Parser Arg
flArg =
  (Arg <$> qcToken flId <*> qcMaybeEnclosedBy (qcChar '(')
                                              (qcChar ')')
                                              (qcToken flType)
    )
    <+> (Arg <$> qcToken flId <*> pure TAny)

flArgs :: Parser [Arg]
flArgs = qcSeparatedBy (qctChar ',') flArg

flReturnType :: Parser Type
flReturnType = qcMaybeEnclosedBy (qcChar '(') (qcChar ')') (qcToken flType)

flFBody :: Parser FuncExp
flFBody =
  (   Cond
    <$> qcToken flExpr
    <*  qctChar '|'
    <*> _innerExpr
    <*> (flFBody <@> pure FNil)
    )
    <+> (Single <$ qctChar '|' <*> _innerExpr)
    <+> (Single <$> _innerExpr)
  where _innerExpr = qcToken (flProgram <+> flExpr)



{--------------------------------:
    IO
:--------------------------------}

flIo :: Parser Exp
flIo = Io <$> (flIoIn <+> flIoOut)

flIoIn :: Parser IoExp
flIoIn =
  (IoFileIn <$> qcToken flPath <* qctStr "~>" <*> qcToken flType)
    <+> (IoStdIn <$> (qctStr "~>" *> qcToken flType))

flIoOut :: Parser IoExp
flIoOut =
  (IoFileOut <$> qcToken flPath <* qctStr "<~" <*> qcToken flType)
    <+> (IoStdOut <$> (qctStr "<~" *> qcToken flType))

flPath :: Parser String
flPath = do
  LString path <- flStr
  return path




{--------------------------------:
    Binary Ops
:--------------------------------}

flOperator :: Parser Operator
flOperator = foldl1 (<+>) $ aux <$> ops
 where
  aux (f, s) = f <$ qcStr s
  ops =
    [ (OpAdd , "+")
    , (OpSub , "-")
    , (OpMul , "*")
    , (OpNeq , "/=")
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
    ]

flBinaryOp :: Parser Exp
flBinaryOp = BinOp <$> qcToken flOperator <*> qcToken flTerm <*> qcToken flTerm

flVar :: Parser Exp
flVar = _var <+> _cSlice <+> _cSingle
 where
  _var = Var <$> flId
  _cSlice =
    CSlice
      <$> (qcChar '&' *> qcNatural)
      <*  qcChar ':'
      <*> (Just <$> qcNatural <+> pure Nothing)
      <&> Capture
  _cSingle = CSingle <$> (qcChar '&' *> qcNatural) <&> Capture

{--------------------------------:
    Helpers
:--------------------------------}

flPrintAST :: PReturn Exp -> IO ()
flPrintAST prs
  | Just pr <- r = pPrint (prResult pr)
  | Just pe <- e = pPrint pe
  | otherwise = error $ "[printAST]: unmatched guards; argument: " <> show prs
  where (e, r) = flDistillReturn prs

flParseString :: String -> PReturn Exp
flParseString = runParser flProgram . qcCtxOfString . removeComments

flParseFile :: FilePath -> IO (Either String (PReturn Exp))
flParseFile path = second flParseString <$> gatherFile path


flFixRootProgram :: PReturn Exp -> PReturn Exp
flFixRootProgram r
  | Left _ <- r   = r
  | Right rr <- r = Right $ rr { prResult = fixRoot . prResult $ rr }
 where
  fixRoot :: Exp -> Exp
  fixRoot e | Program{} <- e = e
            | otherwise      = Program e Nil

flDistillReturn :: PReturn Exp -> (Maybe ParseError, Maybe (ParseResult Exp))
flDistillReturn prs | Right r <- prs = (Nothing, Just r)
                    | Left e <- prs  = (Just e, Nothing)
