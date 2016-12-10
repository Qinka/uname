
for parsing Char and String for Uname

\begin{code}
{-# LANGUAGE FlexibleContexts #-}
\end{code}
\begin{code}
module Uname.Parser.Char
       ( space, spaces, newline, crlf, endOfLine, tab, upper, lower, alphaNum, letter, digit, hexDigit, octDigit
       , satisfy, string, oneOf, noneOf, letter', char, anyChar
       ) where

import Control.Applicative ((*>))
import Data.Char
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim

import Uname.Data.LanguageExtension

import Text.Parsec.Char ( space, spaces, newline, crlf, endOfLine, tab, upper, lower, alphaNum, letter, digit, hexDigit, octDigit)
\end{code}

\function{satisfy}
\begin{code}
satisfy :: (Stream s m Char,IsCaseSensitive u,CaseSensitive s) => (Char -> Bool) -> ParsecT s u m Char
satisfy f = isCaseSensitiveM >>= (\x -> tokenPrim (showChar x) nextPos (testChar x))
  where
    showChar b = if b then show
                   else (\c -> show (toLower c) ++ " or " ++ show (toUpper c))
    nextPos p c _cs = updatePosChar p c
    testChar b c = if | b && f c -> Just c
                      | not b && f `testWithoutCSChar` c -> Just (toLower c)
                      | otherwise -> Nothing
\end{code}

\begin{code}
string :: (Stream s m Char,IsCaseSensitive u,CaseSensitive s) => String -> ParsecT s u m String
string str = isCaseSensitiveM >>= (\x -> (tokens_ x) (showS x) updatePosString (transS x))
  where showS x = if x then show
                  else (\str -> "[case-insensitive]" ++ show (toLower <$> str))
        transS x = if x then str
                      else toLower <$> str
        tokens_ x = if x then tokens
                  else tokens'
\end{code}

\function{tokens'}
\begin{code}
tokens' :: (Stream s m Char)
        => (String -> String)      -- Pretty print a list of tokens
        -> (SourcePos -> String -> SourcePos)
        -> String                  -- List of tokens to parse
        -> ParsecT s u m String
tokens' _ _ []
    = mkPT $ \s ->
      return $ Empty $! return $ Error $ unknownError s
tokens' showTokens nextposs tts@(tok:toks)
    = mkPT $ \(State input pos u) -> -- cok cerr eok eerr -> 
    let errEof = return $ Error $ (setErrorMessage (Expect (showTokens tts))
                                   (newErrorMessage (SysUnExpect "") pos))
        errExpect x = return $ Error $ (setErrorMessage (Expect (showTokens tts))
                                        (newErrorMessage (SysUnExpect (showTokens [x])) pos))
        walk []     rs = ok rs
        walk (t:ts) rs = do
          sr <- uncons rs
          case sr of
            Nothing                                 -> errEof
            Just (x,xs) | toLower t == toLower x    -> walk ts xs
                        | otherwise                 -> errExpect x
        ok rs = let pos' = nextposs pos tts
                    s' = State rs pos' u
                in return $ Ok tts s' (newErrorUnknown pos')
    in do
        sr <- uncons input
        return $ case sr of
            Nothing                         -> Empty errEof
            Just (x,xs)
                | toLower tok == toLower x  -> Consumed $ walk toks xs
                | otherwise                 -> Empty $  errExpect x
\end{code}

\functions{about char}

\begin{code}
oneOf :: (IsCaseSensitive u,Stream s m Char,CaseSensitive s) => [Char] -> ParsecT s u m Char
oneOf cs = satisfy (`elem` cs)
noneOf :: (Stream s m Char,IsCaseSensitive u,CaseSensitive s) => [Char] -> ParsecT s u m Char
noneOf cs = satisfy $ not.(`elem` cs)
letter' :: (IsCaseSensitive u,CaseSensitive s,Stream s m Char) => ParsecT s u m Char
letter' = satisfy isAlpha <?> "case insensitive letter"
char :: (IsCaseSensitive u,CaseSensitive s,Stream s m Char) => Char ->  ParsecT s u m Char
char c = satisfy (==c) <?> (show [toLower c] ++ "or" ++  show [toUpper c])
anyChar :: (IsCaseSensitive u,CaseSensitive s,Stream s m Char) => ParsecT s u m Char
anyChar = satisfy $ const True
\end{code}
