

module Main
       ( main
       ) where

import Uname.IntermediateCode.Parsec
import System.IO

main :: IO ()
main = do
  print =<< runICIO stdin
