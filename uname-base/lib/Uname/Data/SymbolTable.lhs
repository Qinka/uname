
\begin{code}
module Uname.Data.SymbolTable
       ( SymbolTable(..)
       , Set(..)
       , insert
       , fromList
       , member
       , union
       ) where

import Data.HashSet 
\end{code}

\begin{code}
class SymbolTable a where
  symbolTable :: a -> Set String
  modifyTable :: a -> (Set String -> Set String) -> a
\end{code}

