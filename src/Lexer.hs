module Lexer where

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = 
  P { runParser :: String -> Maybe (a, String) } 

instance Functor Parser where
  fmap f (P p) = P $ \s -> do
    (x', s') <- p s
    return (f x', s')

instance Applicative Parser where
  pure a = P $ \s -> return (a, s)
  (P f) <*> (P p) = P $ \s -> do
    (f', s')  <- f s
    (p', s'') <- p s'
    return (f' p', s'')

instance Monad Parser where
  (P p) >>= f = P $ \s -> do
    (p', s') <- p s
    runParser (f p') s'

instance MonadFail Parser where
  fail _ = P (const Nothing)

instance Alternative Parser where
  empty = fail ""
  (P a) <|> (P b) = P $ \s -> a s <|> b s

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

char :: Char -> Parser Char
char c = P aux
  where 
    aux (c':s') | c' == c = return (c, s')
    aux _                 = empty

string :: String -> Parser String
string = mapM char

pre :: (Char -> Bool) -> Parser Char
pre f = P aux
  where 
    aux (c:s) | f c = return (c, s)
    aux _           = empty

pOr :: [( Char -> Bool )] -> Parser Char
pOr predicates = P aux
  where
    aux (c:s)
      | or [predicate c | predicate <- predicates] 
      = return ( c, s )
      | otherwise                                  
      = empty
    aux _ = empty


space :: Parser Char
space = pre isSeparator

ss :: Parser String
ss = many space

nat :: Parser Char
nat = pre isDigit

natural :: Parser Int
natural = read <$> some nat

num :: Parser Int
num = negate <$> (char '-' *> natural) 
             <|> (char '+' *> natural)
             <|>              natural

enclosed :: Parser l -> Parser r -> Parser a -> Parser a
enclosed l r p = l *> p <* r

token :: Parser a -> Parser a
token p = ss *> p <* ss

letter :: Parser Char
letter = pre isLetter

mark :: Parser Char
mark = pre isMark

sepBy :: Parser sep -> Parser el -> Parser [el]
sepBy sep el = do
              a    <- el
              _    <- token sep
              rest <- sepBy sep el
              return (a : rest)  
         <|> (pure <$> token el)
