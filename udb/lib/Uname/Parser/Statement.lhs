\begin{code}
\end{code}

\begin{code}
module Uname.Parser.Statement
       ( parsingIsS
       , parsingFRDS
       ) where


import Uname.Data.LanguageExtension
import Uname.Data.Syntax
import Uname.Parser.Char
import Uname.Parser.Lit
import Uname.Parser.Expr

import Text.Parsec.Combinator
import Text.Parsec.Prim       
\end{code}

for parsing IS statment
\begin{code}
parsingIsS :: (Stream s m Char,IsCaseSensitive u,CaseSensitive s) => ParsecT s u m Stmt
parsingIsS = do
  et <- parsingVarE
  try spaces
  string "is"
  try spaces
  var <- parsingExpr
  return $ IsS et var
parsingFRDS :: (Stream s m Char,IsCaseSensitive u,CaseSensitive s) => ParsecT s u m Stmt
parsingFRDS = do
  string "for"
  try spaces
  t <- parsingVarE
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
  try spaces
  string "draw"
  try spaces
  draw <- parsingExpr
  return $ FRDS t from to step draw
\end{code}
