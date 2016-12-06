

This file is about the language extension in UMK, UNK, and UPK.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
\end{code}

\begin{code}
module Uname.Data.LanguageExtension
       ( IsCaseSensitive(..)
       , CaseSensitive(..)
       , testWithoutCSChar
       ) where

import Text.Parsec

import Data.Char (toLower,toUpper)
\end{code}

\codesection{Case Sensitive}

\typeclass{IsCaseSensitive}
Type class \codeinline|IsCaseSensitive| is about whether a state (for ParsecT) is case sensitive.
And \codeinline|isCaseSensitive| and \codeinline|isCaseSensitiveM| will return whether it is or not.
And at least \codeinline|isCaseSensitive| is necessary.
\begin{code}
class IsCaseSensitive u  where
  isCaseSensitive  :: u -> Bool
  isCaseSensitiveM :: (Monad m,CaseSensitive s) => ParsecT s u m Bool
  isCaseSensitiveM = isCaseSensitive <$> getState
  {-# MINIMAL isCaseSensitive #-}
\end{code}

\typeclass{CaseSensitive}
Type class \codeinline|CaseSensitive| is about making sure that a \codeinline|IsString|-instanced data type is case sensitive.
And \codeinline|eqCaseSensitive| is case sensitive equivalence, and \codeinline|eqNonCaseSensitive| is case insensitive.
\begin{code}
infixl 4 =-=, =.=
class Eq s =>  CaseSensitive s where
  eqCaseSensitive :: s -> s -> Bool
  eqCaseSensitive = (=-=)
  eqCaseInsensitive :: s -> s -> Bool
  eqCaseInsensitive = (=.=)
  (=-=) :: s -> s -> Bool
  (=-=) = eqCaseSensitive
  (=.=) :: s -> s -> Bool
  (=.=) = eqCaseInsensitive

instance CaseSensitive s => CaseSensitive [s] where
  [] =.= [] = True
  (x:xs) =.= (y:ys) = x =.= y && xs =.= ys
  _xs =.= _ys = False
  [] =-= [] = True
  (x:xs) =-= (y:ys) = x =-= y && xs =-= ys
  _xs =-= _ys = False
\end{code}

\instance{CaseSensitive}
For Char and String
\begin{code}
instance CaseSensitive Char where
  (=.=) = toL.flip $ toL (==)
    where toL = ((.toLower) <$>)
  (=-=) = (==)  
\end{code}

\function{testWithoutCS}
\begin{code}
testWithoutCSChar :: (Char -> Bool) -> Char -> Bool
testWithoutCSChar f b = f (toLower b) || f (toUpper b)
\end{code}
