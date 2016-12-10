\begin{code}
\end{code}

\begin{code}
module Uname.Evaluation
       ( evalStmt
       ) where

import Uname.Data.IntermediateCode
import Uname.Data.Syntax
import Uname.Error.CrashReport
import Uname.Parser

import Control.Applicative
import Control.Parallel.Strategies
import Data.Maybe
\end{code}

for evalutions
\begin{code}
evalStmtStep :: Stmt -> [Input]
evalStmtStep i@(IsS _ _) = [evalIsStatement i]
evalStmtStep i@(FRDS _ _ _ _ _) = evalFRDStatement i
evalStmtStep _ = [InvailedStmt]
evalStmt :: [Stmt] -> [Input]
evalStmt st = filter (\x -> x /= InvailedStmt) $ concat iss
 where iss = evalStmtStep <$> st `using` parList rseq
\end{code}

for evaluation IS statement
\begin{code}
evalIsStatement :: Stmt -> Input
evalIsStatement (IsS e1 e2) = case e1 of
  VarE "rotate"    -> SettingI $ Rotate    $ evalTupleLit  e2
  VarE "scale"     -> SettingI $ Scale     $ evalTuple2Lit e2
  VarE "translate" -> SettingI $ Translate $ evalTuple2Lit e2
  VarE x -> report $ "unexpected var id "++x++" are here, and this should not be there."
  x -> report $ "unexpected expr there("++show x++")."
\end{code}

for evluation For From To Step Draw
\begin{code}
evalFRDStatement :: Stmt -> [Input]
evalFRDStatement (FRDS (VarE x) eb ee es et) = ps
  where
    (xFunc,yFunc) = evalTuple2Lambda et
    tBegin = evalTupleLit eb 
    tEnd   = evalTupleLit ee
    tStep  = evalTupleLit es
    ts = [tBegin,tStep+tBegin..tEnd]
    xs = xFunc <$> ts `using` parList rseq
    ys = yFunc <$> ts `using` parList rseq
    ps = (map (\a -> PointI $ Point a "" []) $ zip xs ys) `using` parList rseq
\end{code}

for tuple ~ $\lambda$
\begin{code}
type LaDouble a = (->) Double a
evalTuple2Lambda :: Exp -> (LaDouble Double,LaDouble Double)
evalTuple2Lambda (TupleE (el:er:_)) =(evalTupleLambda el,evalTupleLambda er)
evalTupleLambda :: Exp -> LaDouble Double
evalTupleLambda (LitE (RealL x)) = \_ -> fromRational x
evalTupleLambda (VarE _) = \x -> x
evalTupleLambda (TupleE (x:_)) = evalTupleLambda x
evalTupleLambda (FuncE f e) = (evalFunc f).(evalTupleLambda e)
evalTupleLambda (PosE e) = evalTupleLambda e
evalTupleLambda (NegE e) = liftA negate (evalTupleLambda e)
evalTupleLambda (AddE e1 e2) = liftETla (+)  e1 e2
evalTupleLambda (SubE e1 e2) = liftETla (-)  e1 e2
evalTupleLambda (MulE e1 e2) = liftETla (*)  e1 e2
evalTupleLambda (DivE e1 e2) = liftETla (/)  e1 e2
evalTupleLambda (PwrE e1 e2) = liftETla (**) e1 e2
liftETla :: (Double -> Double -> Double) -> Exp -> Exp -> LaDouble Double
liftETla f e1 e2 = liftA2 f (evalTupleLambda e1) (evalTupleLambda e2)
\end{code}

for tuple ~ lit
\begin{code}
evalTupleLit :: Exp -> Double
evalTupleLit (LitE (RealL r)) = fromRational r
evalTupleLit (TupleE es) = evalTupleLit $ head es
evalTupleLit (FuncE str e) = evalFunc str $ evalTupleLit e
evalTupleLit (PosE e) = evalTupleLit e
evalTupleLit (NegE e) = - evalTupleLit e
evalTupleLit (AddE e1 e2) = evalTupleLit e1 +  evalTupleLit e2
evalTupleLit (SubE e1 e2) = evalTupleLit e1 -  evalTupleLit e2
evalTupleLit (MulE e1 e2) = evalTupleLit e1 *  evalTupleLit e2
evalTupleLit (DivE e1 e2) = evalTupleLit e1 /  evalTupleLit e2
evalTupleLit (PwrE e1 e2) = evalTupleLit e1 ** evalTupleLit e2
evalTuple2Lit :: Exp -> (Double,Double)
evalTuple2Lit (TupleE (e1:e2:_)) = (evalTupleLit e1,evalTupleLit e2)
\end{code}

for eval funcs
\begin{code}
evalFunc :: String -> LaDouble Double
evalFunc "sin" = sin
evalFunc "cos" = cos
evalFunc "tan" = tan
evalFunc "asin" = asin
evalFunc "acos" = acos
evalFunc "atan" = atan 
evalFunc "sinh" = sinh
evalFunc "cosh" = cosh
evalFunc "tanh" = tanh
evalFunc "asinh" = asinh
evalFunc "acosh" = acosh 
evalFunc "atanh" = atanh
evalFunc "sqrt" = sqrt
evalFunc "log" = logBase 10
evalFunc "ln"  = log
evalFunc "exp" = exp
evalFunc "abs" = abs 
\end{code}
