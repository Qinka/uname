

\begin{code}
module Uname.EDSL
       ( rotate
       , trans
       , scale
       , for
       , interval
       , module X
       ) where

import Uname.EDSL.Internal as X

\end{code}


\begin{code}
rotate :: (Floating f,Monad m) => IS -> f -> UnameT f m ()
rotate _ r = modify $ modifyUnameState [SettingC $ Rotate r]
trans  :: (Floating f,Monad m) => IS -> (f,f) -> UnameT f m ()
trans _ t =  modify $ modifyUnameState [SettingC $ Translate t]
scale :: (Floating f,Monad m) => IS -> (f,f) -> UnameT f m ()
scale _ s =  modify $ modifyUnameState [SettingC $ Scale s]
\end{code}

\begin{code}
for :: (Floating f,Monad m,Enum f) =>  Val -> FROM -> f -> TO -> f -> DRAW -> (f -> f,f -> f) -> UnameT f m ()
for _ _ b _ e _ (fl,fr) = do
  (_,i) <- get
  let lins = [b,b+i..e]
      xs   = fl <$> lins
      ys   = fr <$> lins
      points = zipWith mkPoint xs ys
  modify $ modifyUnameState points
  where mkPoint x y = PointC $ Point (x,y) "" []
\end{code}

\begin{code}
interval :: (Floating f,Monad m) => IS -> f -> UnameT f m ()
interval _ i = modify $ modifyUnameStateInterval (\_ -> i)
\end{code}

\begin{code}
runUnameT :: (Floating f,Monad m) => f -> UnameT f m () -> m [Code f]
runUnameT iv ums = fst <$> execStateT ums ([],iv)
\end{code}
