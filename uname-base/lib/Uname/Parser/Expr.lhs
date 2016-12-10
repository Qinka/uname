
\begin{code}
module Uname.Parser.Expr
       ( parsingLitE
       , parsingVarE
       , parsingTupleE
       , parsingExpr
       , parsingL1
       , parsingL2
       , parsingL3
       , parsingL4
       , parsingL0
       , parsingFuncE
       ) where

import Uname.Data.LanguageExtension
import Uname.Data.ParserState
import Uname.Data.SymbolTable
import Uname.Data.Syntax
import Uname.Parser.Char
import Uname.Parser.Lit

import Text.Parsec.Combinator
import Text.Parsec.Prim
\end{code}

\codesection{parsing expression}





For a normal expr with operations, there is the way about how to parsing:
$$Expr \rightarrow prefix\,Expr'|Expr'\,infix\,Expr'|Expr'\,postfix$$
And the "Expr'" means the next level of operations, and for the top level,the operation is `LitE',`VarE', and `TupleE',or the Expr.


for LitE
\begin{code}
parsingLitE :: UnameParser s u m => ParsecT s u m Exp
parsingLitE = LitE <$> parsingLit
\end{code}

for VarE
\begin{code}
getNameVE :: UnameParser s u m => Bool -> ParsecT s u m (Bool,String)
getNameVE b = do
  st <- symbolTable <$> getState
  f <- oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['\'','_']
  rst <- many.oneOf $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['\'','_']
  let b' =  b || ((f:rst) `member` st)
  return (b', f:rst)
  
parsingVarE :: UnameParser s u m => Bool -> ParsecT s u m Exp
parsingVarE b = do
  v <- getNameVE b
  case v of
    (_,"pi") -> return $ LitE $ RealL $ toRational   pi
    (_,"e" ) -> return $ LitE $ RealL $ toRational $ exp 1
    (True ,x) -> return (VarE x) <?> "a vailed name of variable"
    (False,x) -> unexpected $ "Unexpected symbol: " ++ x
\end{code}

for IsE
\begin{code}
parsingTupleE :: UnameParser s u m => ParsecT s u m Exp
parsingTupleE = let p = parsingExpr `sepBy` (spaces >>char ','>>spaces)
                    t = char '(' *> p <* char ')'
                in TupleE <$> t
\end{code}

for level 4 (about add and sub)
\begin{code}
parsingL4 :: UnameParser s u m => ParsecT s u m Exp
parsingL4 = try (parsingL3 >>= parsingL4') <|> parsingL3
parsingL4' :: UnameParser s u m => Exp -> ParsecT s u m Exp
parsingL4' e1 = do
  try spaces
  opt <- oneOf "+-"
  try spaces
  e2 <- parsingL3
  let e = case opt of
        '+' -> AddE e1 e2
        '-' -> SubE e1 e2
  option e $ parsingL4' e
\end{code}

for level 3 (about multiplication and division)
\begin{code}
parsingL3 :: UnameParser s u m => ParsecT s u m Exp
parsingL3 = try (parsingL2 >>= parsingL3') <|> parsingL2
parsingL3' ::  UnameParser s u m => Exp -> ParsecT s u m Exp
parsingL3' e1 = do
  try spaces
  opt <- oneOf "*/"
  try spaces
  e2 <- parsingL2
  let e = case opt of
        '*' -> MulE e1 e2
        '/' -> DivE e1 e2
  option e $ parsingL3' e
\end{code}

for level 2 (about positive adn negative)
\begin{code}
parsingL2 :: UnameParser s u m => ParsecT s u m Exp
parsingL2 = try parsingL2' <|> parsingL1
parsingL2' :: UnameParser s u m => ParsecT s u m Exp
parsingL2' = do
  try spaces
  opt <- oneOf "+-"
  try spaces
  e <- parsingL2
  return $ case opt of
    '+' -> PosE e
    '-' -> NegE e
\end{code}

for level 1 (about power)
\begin{code}
parsingL1 :: UnameParser s u m => ParsecT s u m Exp
parsingL1 = try parsingL1' <|> parsingL0
parsingL1' :: UnameParser s u m => ParsecT s u m Exp
parsingL1' = do
  e1 <- parsingL0
  try spaces
  char '^'
  try spaces
  e2 <- parsingL1
  return $ PwrE e1 e2
\end{code}

for level0 (about id, lit,comment, and func)
\begin{code}
parsingL0 :: UnameParser s u m => ParsecT s u m Exp
parsingL0 = try parsingFuncE <|> try parsingTupleE <|> try parsingLitE <|> parsingVarE False
parsingFuncE :: UnameParser s u m => ParsecT s u m Exp
parsingFuncE = do
  try spaces
  funName <- mkFunNames ["sin","cos","tan","asin","acos","atan","sinh","cosh","tanh","asinh","acosh","atanh","sqrt","ln","exp","log","abs"]
    --} try (string "sin") <|> try (string "cos") <|> (string "ln")
  try spaces
  e <- parsingExpr
  return $ FuncE funName e
  where mkFunNames [] = error "how ?!"
        mkFunNames (x:xs) = foldl (<|>) (string x) $ map (try.string) xs
\end{code}

\begin{code}
parsingExpr :: UnameParser s u m => ParsecT s u m Exp
parsingExpr = parsingL4
\end{code}
