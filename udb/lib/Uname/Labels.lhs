

\begin{code}
module Uname.Labels
       ( Label(..)
       , HasLabel(..)
       ) where

import Import.Labels
\end{code}


设置限制 label 的类型类与类型
\begin{code}
data Label (l::Symbol) = Get
class HasLabel a l b | a l -> b where
  from :: a -> Label l -> b
instance HasLabel a l b => IsLabel l (a -> b) where
  fromLabel tag x = from x (Get :: Label l)
\end{code}
