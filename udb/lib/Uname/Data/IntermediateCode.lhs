

\begin{code}
module Uname.Data.IntermediateCode
       ( Input(..)
       , Setting(..)
       , Point(..)
       , Color(..)
       ) where

import Uname.Labels

import Import.Labels
\end{code}


\begin{code}
data Input = SettingI Setting
           | PointI Point
           | FrameSplit
           | InvailedStmt
           deriving (Show,Eq)

data Setting = Scale (Double,Double)
             | Rotate Double
             | Translate (Double,Double)
             deriving (Show,Eq)

data Point = Point (Double,Double) String [Double]
           deriving (Show,Eq)

data Color = RGB (Double,Double,Double)
           | RGBA (Double,Double,Double,Double)
           | HSV (Double,Double,Double)
           deriving (Show,Eq)                   
\end{code}

\begin{code}
instance HasLabel Point "color" (Maybe Color) where
  from (Point _ k vs) _ = case (k,vs) of
    ("RGB",r:g:b:_) -> Just $ RGB (r,g,b)
    ("RGBA",r:g:b:a:_) -> Just $ RGBA (r,g,b,a)
    ("HSV", h:s:v:_) -> Just $ HSV (h,s,v)
    ("", r:g:b:_) -> Just $ RGB (r,g,b)
    _ -> Nothing
instance HasLabel Point "getX" Double where
  from (Point x _ _) _ = fst x
instance HasLabel Point "getY" Double where
  from (Point y _ _) _ = snd y
instance HasLabel Point "getXY" (Double,Double) where
  from (Point x _ _) _ = x
instance HasLabel Point "colorKind" String where
  from (Point _ x _) _ = x
\end{code}
