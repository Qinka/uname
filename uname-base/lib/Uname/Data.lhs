
定义模块同时道路有关模块
\begin{code}
module Uname.Data
       ( TranStatus(..)
       ) where

import Data.Maybe
import Uname.Labels

import Import.Labels
\end{code}


定义相关的数据类型
\begin{code}
data TranStatus = TranStatus (Maybe (Double,Double)) (Maybe (Double,Double)) (Maybe Double)
                  deriving (Show,Eq)
\end{code}

设置标签
\begin{code}
instance HasLabel TranStatus "getScale" (Double,Double) where
  from (TranStatus x _ _) _ = fromMaybe (0,0) x
instance HasLabel TranStatus "getScaleX" Double where
  from (TranStatus x _ _) _ = fromMaybe 0 $ fst <$> x
instance HasLabel TranStatus "getScaleY" Double where
  from (TranStatus x _ _) _ = fromMaybe 0 $ snd <$> x
instance HasLabel TranStatus "getRotate" Double where
  from (TranStatus _ _ x) _ = fromMaybe 0 x
instance HasLabel TranStatus "getTrans" (Double,Double) where
  from (TranStatus _ x _) _ = fromMaybe (0,0) x
instance HasLabel TranStatus "getTransX" Double where
  from (TranStatus _ x _) _ = fromMaybe 0 $ fst <$> x
instance HasLabel TranStatus "getTransY" Double where
  from (TranStatus _ x _) _ = fromMaybe 0 $ snd <$> x
\end{code}
