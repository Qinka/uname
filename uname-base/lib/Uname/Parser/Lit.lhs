

\begin{code}
module Uname.Parser.Lit
       ( parsingLit
       , parsingRealL
       ) where

import Uname.Data.LanguageExtension
import Uname.Data.ParserState
import Uname.Data.Syntax
import Uname.Parser.Char

import Text.Parsec.Combinator
import Text.Parsec.Prim
\end{code}

\codesection{parsing real number}
for basic part
\begin{code}
fDtR :: Double -> Rational
fDtR = toRational
fItR :: Int -> Rational
fItR = toRational
intOL :: UnameParser s u m => ParsecT s u m Rational
intOL = do
  f <- (fItR.read) <$> many1 digit
  option '.' $ char '.'
  e <- option id expItem
  return $ e f
pointOL :: UnameParser s u m => ParsecT s u m Rational
pointOL = do
  f <- (fDtR.read.("0." ++)) <$> (char '.' *> many1 digit)
  e <- option id expItem
  return $ e f
bypartL :: UnameParser s u m
        => ParsecT s u m Rational
bypartL = do
  l <- many1 digit
  p <- char '.'
  r <- many1 digit
  let f = (fDtR.read) $ l++p:r
  e <- option id expItem
  return $ e f
\end{code}

for power
\begin{code}
expItem :: (UnameParser s u m,Fractional a,Eq a)
        => ParsecT s u m (a -> a)
expItem = do
  char 'E' <|> char 'e'
  g <- sign
  x <- read <$> (many1 digit)
  return . fixE $ g x
  where
    fixE 0 0 = 0
    fixE x y = y ^^ x
\end{code}

for signed
\begin{code}
sign :: (UnameParser s u m,Num a) => ParsecT s u m (a -> a)
sign = do
  x <- option '+' $ oneOf "+-"
  return $ \a -> case x of
    '+' -> a
    '-' -> -a
\end{code}

for parsing
\begin{code}
parsingRealL :: UnameParser s u m => ParsecT s u m Lit
parsingRealL = RealL <$> floatLit
  where floatLit = sign >>= \x -> x <$> choice [try bypartL,intOL,pointOL]
          <?> "a real number liking 1, .1, 1.1e2, etc"
parsingLit ::  UnameParser s u m => ParsecT s u m Lit
parsingLit = parsingRealL
\end{code}
