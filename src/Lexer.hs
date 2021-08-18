module Lexer where

import           Control.Applicative
import           Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.List
import           Text.Pretty.Simple
import           Text.Read

data ParseResult a = ParseResult
  { prResult     :: a
  , prNewContext :: ParseContext
  }
  deriving (Show, Eq)

instance Functor ParseResult where
  fmap f pr = pr { prResult = f . prResult $ pr }

data ParseError = ParseError
  { peContext  :: ParseContext
  , peExpected :: String
  , peGot      :: String
  }
  deriving (Show, Eq)

data ParseContext = ParseContext
  { ctxLine   :: Int
  , ctxColumn :: Int
  , ctxString :: String
  }
  deriving (Show, Eq)

qcCtxOfString :: String -> ParseContext
qcCtxOfString = ParseContext 1 1

type PReturn a = Either ParseError (ParseResult a)
newtype Parser a
  = Parser { runParser :: ParseContext -> PReturn a }

instance Functor Parser where
  fmap f p = Parser $ \c -> let rs = runParser p c in second (fmap f) rs

instance Applicative Parser where
  pure a = Parser $ \c -> Right $ ParseResult a c
  fp <*> qp = Parser $ \c -> case runParser fp c of
    Right f -> fmap (fmap (prResult f)) (runParser qp (prNewContext f))
    Left  e -> Left e


instance Monad Parser where
  qp >>= f = Parser $ \c -> case runParser qp c of
    Left  e  -> Left e
    Right qr -> runParser (f . prResult $ qr) (prNewContext qr)

instance MonadFail Parser where
  fail msg = Parser $ const
    (Left $ ParseError { peContext  = qcCtxOfString ""
                       , peExpected = ""
                       , peGot      = msg
                       }
    )

instance Alternative Parser where
  empty = fail ""
  p <|> q = Parser $ \c ->
    let rp = runParser p c
        rq = runParser q c
    in  case (rp, rq) of
          (Right _, _) -> rp
          _            -> rq


data InCtx a = InCtx
  { incV   :: a
  , incCtx :: ParseContext
  }
  deriving (Show, Eq)

qcChar :: Char -> Parser Char
qcChar k = Parser aux
 where
  aux :: ParseContext -> PReturn Char
  aux c
    | s@(k' : ks) <- ctxString c
    , k' == k
    = let dl = ((-) `on` (length . lines)) s ks
          dc | dl > 0    = -(ctxColumn c) + 1
             | otherwise = 1
      in  Right $ ParseResult
            { prResult     = k
            , prNewContext = c { ctxColumn = ctxColumn c + dc
                               , ctxLine   = ctxLine c + dl
                               , ctxString = ks
                               }
            }
    | (k' : _) <- ctxString c
    = Left $ ParseError { peContext = c, peExpected = [k], peGot = [k'] }
    | otherwise
    = Left $ ParseError { peContext = c, peExpected = [k], peGot = "" }

qctChar :: Char -> Parser Char
qctChar = qcToken . qcChar

qcStr :: String -> Parser String
qcStr = mapM qcChar

qctStr :: String -> Parser String
qctStr = qcToken . qcStr

qcProp :: (Char -> Bool) -> Parser Char
qcProp f = Parser aux
 where
  aux c
    | k : ks <- ctxString c, f k = runParser (qcChar k) c
    | otherwise = Left $ ParseError
      { peContext  = qcCtxOfString ""
      , peExpected = "[proposition]"
      , peGot      = "[unmatched: " <> take 1 (ctxString c) <> "]"
      }


qcNat :: Parser Char
qcNat = qcProp isDigit

qcNatural :: Parser Int
qcNatural = do
  r <- (readMaybe <$> many qcNat) :: Parser (Maybe Int)
  case r of
    Just n -> pure n
    Nothing -> fail "cannot read int"

qcNum :: Parser Int
qcNum = qcSigned qcNatural

qcSigned :: (Eq a, Num a) => Parser a -> Parser a
qcSigned p =
  (*) <$> ((qcChar '-' $> (-1)) <|> (qcChar '+' $> 1) <|> pure 1) <*> p

qcSS :: Parser String
qcSS = many (qcProp (`elem` " \t\r\n"))

qcToken :: Parser a -> Parser a
qcToken p = qcSS *> p <* qcSS

qcEnclosedBy :: Parser l -> Parser r -> Parser p -> Parser p
qcEnclosedBy l r p = l *> qcToken p <* r

qcMaybeEnclosedBy
  :: (Eq l, Eq r, Eq p) => Parser l -> Parser r -> Parser p -> Parser p
qcMaybeEnclosedBy l r p = qcEnclosedBy l r p <|> p

qcSeparatedBy :: (Eq s, Eq e) => Parser s -> Parser e -> Parser [e]
qcSeparatedBy s e = ((:) <$> (e <* s) <*> qcSeparatedBy s e) <|> (: []) <$> e

