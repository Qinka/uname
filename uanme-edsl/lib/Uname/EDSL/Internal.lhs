

\begin{code}
module Uname.EDSL.Internal
       ( UnameState(..)
       , UnameT(..)
       , IS, is
       , FROM, from
       , TO, to
       , DRAW, draw
       , Val, t
       , modifyUnameState
       , modifyUnameStateInterval
       , module X
       , Code(..)
       , Setting(..)
       , Point(..)
       ) where


import Control.Monad
import Control.Monad.State as X
\end{code}


Uname Monad
\begin{code}
type UnameState f = ([Code f],f)
type UnameT f m a = StateT (UnameState f) m a
\end{code}

\begin{code}
modifyUnameState :: [Code f] -> UnameState f -> UnameState f
modifyUnameState cs' (cs,i)  = (cs++cs',i)
modifyUnameStateInterval :: (f -> f) -> UnameState f -> UnameState f
modifyUnameStateInterval f (cs,i) = (cs,f i)
\end{code}


\begin{code}
data IS = IS
is :: IS
is = IS
data FROM = FROM
from :: FROM
from = FROM
data TO = TO
to :: TO
to = TO
data DRAW = DRAW
draw :: DRAW
draw = DRAW
data Val
t :: Val
t = undefined
\end{code}



\begin{code}
data Code f = SettingC (Setting f)
            | PointC (Point f)

data Setting f = Scale (f,f)
               | Rotate f
               | Translate (f,f)

data Point f = Point (f,f) String [f]              
\end{code}

instance Show
\begin{code}
instance Show f => Show (Code f) where
  show (SettingC s) = unlines.return $ show s
  show (PointC   p) = unlines.return $ show p

instance Show f => Show (Point f) where
  show (Point (x,y) color cls) = show x ++ " " ++ show y ++ cStr ++ clsStr
    where cStr = if null color then "" else " " ++ color
          clsStr = if null cls then "" else " " ++ unwords (show <$> cls)

instance Show f => Show (Setting f) where
  show (Rotate r) = "rotate " ++ show r
  show (Scale (x,y)) = "scale " ++ show x ++ " " ++ show y
  show (Translate (x,y)) = "translate " ++ show x ++ " " ++ show y
\end{code}
