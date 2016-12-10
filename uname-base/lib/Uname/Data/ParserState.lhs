
\begin{code}
module Uname.Data.ParserState
       ( UnameParser(..)
       ) where

import Uname.Data.LanguageExtension
import Uname.Data.SymbolTable

import Text.Parsec.Prim
\end{code}

\begin{code}
class (Stream s m Char,CaseSensitive s,IsCaseSensitive u,SymbolTable u) => UnameParser s u m
\end{code}
