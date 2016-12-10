
module Uname.Error.CrashReport
       ( report
       ) where

report :: String -> a
report str = error $ unlines
  [ "When you see this texts, that means somewhere crashed unexpexted."
  , "Such errors will happen when there are some things going wrongly."
  , "You need to report this error to https://github.com/qinka/uname/issuse"
  , "And the \"might-be\" useful infomations are the following:"
  , str
  ]
