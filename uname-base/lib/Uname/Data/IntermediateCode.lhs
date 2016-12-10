

\begin{code}
module Uname.Data.IntermediateCode
       ( Input(..)
       , Setting(..)
       , Point(..)
       , Color(..)
       , showInput
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
showInput :: Input -> String
showInput (SettingI s) = showSetting s
showInput (PointI p) = showPoint p
showInput FrameSplit = "frame\n"
showInput InvailedStmt = ""
showSetting :: Setting -> String
showSetting (Scale (x,y)) = "scale " ++ show x ++ " " ++ show y ++ "\n"
showSetting (Rotate x) = "rotate " ++ show x ++ "\n"
showSetting (Translate (x,y)) = "translate " ++ show x ++ " " ++ show y ++ "\n"
showPoint :: Point -> String
showPoint (Point (x,y) str ds) = show x ++ " " ++ show y ++ " " ++ str ++ " " ++ (unwords $ map show ds) ++ "\n"
\end{code}
showInputB :: Input -> ByteString
showInputB (SettingI s) = showSettingB s
showInputB (Point p) = showPointB p
showInputB FrameSplit = "frame\n"
showInputB InvailedStmt = ""
showSettingB :: Settings -> ByteString
showSettingB (Scale (x,y)) = concat ["scale ",  
\end{ce}


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
