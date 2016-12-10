\begin{code}
\end{code}

\begin{code}
module Uname.Parser.Statement
       ( parsingIsS
       , parsingFRDS
       , parsingLanguageExtension
       , parsingStmt
       , parsingStmtStep
       ) where


import Uname.Data.LanguageExtension
import Uname.Data.ParserState
import Uname.Data.SymbolTable
import Uname.Data.Syntax
import Uname.Parser.Char
import Uname.Parser.Lit
import Uname.Parser.Expr

import Data.Maybe
import Text.Parsec.Combinator
import Text.Parsec.Prim
import qualified Text.Parsec.Char as TPC
\end{code}

\begin{code}
parsingStmt :: UnameParser s u m => ParsecT s u m [Stmt]
parsingStmt = many parsingStmtStep <* eof

parsingStmtStep :: UnameParser s u m => ParsecT s u m Stmt
parsingStmtStep = do
  try spaces
  option "" parsingComment
  try spaces
  x <- try (parsingIsS <* char ';')
    <|> try (parsingFRDS <* char ';')
    <|> parsingLanguageExtension
  try spaces
  return x
\end{code}

for parsing IS statment
\begin{code}
parsingIsS :: UnameParser s u m => ParsecT s u m Stmt
parsingIsS = do
  st <- symbolTable <$> getState
  modifyState (\x -> modifyTable x $ \x -> x `union` fromList ["rotate","translate","scale"])
  et@(VarE vn) <- parsingVarE False
  try spaces
  string "is"
  try spaces
  var <- parsingExpr
  modifyState (\x -> modifyTable x $ \_ -> st)
  try spaces
  check vn var
  return $ IsS et var
  where
    check n (TupleE []) = unexpected $ "wrong type for " ++ n
    check "translate" (TupleE [x]) = unexpected "wrong type for translate"
    check "scale" (TupleE [x]) = unexpected "wrong type for scale"
    check _ _ = return ()
\end{code}

\begin{code}
parsingComment :: UnameParser s u m => ParsecT s u m String
parsingComment = between (string "--" <|> string "//") newline
  (many $ noneOf "\n\r")
\end{code}

for parsing draw
\begin{code}
parsingFRDS :: UnameParser s u m => ParsecT s u m Stmt
parsingFRDS = do
  string "for"
  try spaces
  st <- symbolTable <$> getState
  t@(VarE vn) <- parsingVarE True
  try spaces
  string "from"
  try spaces
  from <- parsingExpr
  try spaces
  string "to"
  try spaces
  to <- parsingExpr
  try spaces
  string "step"
  try spaces
  step <- parsingExpr
  modifyState (\x -> modifyTable x $ insert vn)
  try spaces
  string "draw"
  try spaces
  draw <- parsingExpr
  modifyState (\x -> modifyTable x $ \_ -> st)
  return $ FRDS t from to step draw
\end{code}


for language extension
\begin{code}
parsingLanguageExtension :: UnameParser s u m => ParsecT s u m Stmt
parsingLanguageExtension = do
  TPC.string "{-#"
  try spaces
  TPC.string "LANGUAGE"
  try spaces
  xN <- many1 letter
  try spaces
  TPC.string "#-}"
  try $ many $ oneOf " \t"
  enableLanguageExtension $ LanguageExtensionS xN
\end{code}

\begin{code}
enableLanguageExtension :: UnameParser s u m => Stmt -> ParsecT s u m Stmt
enableLanguageExtension i@(LanguageExtensionS "CaseSensitive")   = transCaseM True  >> return i
enableLanguageExtension i@(LanguageExtensionS "CaseInsensitive") = transCaseM False >> return i
enableLanguageExtension i = return i
\end{code}
