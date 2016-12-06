

\begin{code}
module Uname.Data.Syntax
       ( Lit(..)
       , Exp (..)
       , Stmt(..)
       , Prog(..)
       ) where

\end{code}

\codesection{data structs}

for literals
\begin{code}
data Lit = RealL Rational
           deriving (Eq,Show)
\end{code}

for expression
\begin{code}
data Exp = LitE Lit
         | VarE String
         | TupleE [Exp]
         | FuncE String Exp
         | PosE Exp
         | NegE Exp
         | AddE Exp Exp
         | SubE Exp Exp
         | MulE Exp Exp
         | DivE Exp Exp
         | PwrE Exp Exp
         deriving (Eq,Show)
\end{code}

for statment
\begin{code}
data Stmt = IsS Exp Exp
          | FRDS Exp Exp Exp Exp Exp
          deriving (Eq,Show)
\end{code}

for all
\begin{code}
data Prog = Prog [Stmt]
       deriving (Eq,Show)
\end{code}

