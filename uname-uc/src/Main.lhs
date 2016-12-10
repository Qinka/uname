
\begin{code}
module Main
       ( main
       ) where

import Uname.Data.IntermediateCode
import Uname.Data.LanguageExtension
import Uname.Data.ParserState
import Uname.Data.SymbolTable
import Uname.Data.Syntax
import Uname.Evaluation
import Uname.Parser

import Codec.Compression.Lzma
import Data.Functor.Identity
import System.Console.CmdArgs
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
\end{code}

\begin{code}

main :: IO ()
main = do
  u@UC{..} <- cmdArgs uc
  str <- readFile' compileFile
  let isCS  = case (xCaseSensitive,xCaseInsensitive) of
        (True,_) -> True
        (False,True) -> False
        _ -> error "how?"
  let toICode ss = do
        let ic = evalStmt ss
        let icStr = concat $ showInput <$> ic
        if stopAfterIC
          then outputIC output icStr
          else toUCC icStr
      toUCC icStr = do
        let cbs = BSLC.pack icStr
        let lbs = cbs :: BSL.ByteString
        let cp = defaultCompressParams 
              { compressIntegrityCheck = IntegrityCheckSha256
              , compressLevel = CompressionLevel9
              , compressLevelExtreme = True
              }
        let ucc = compressWith cp lbs
        writeFileBS output ucc
  let st = toSyntaxTree compileFile isCS str
  case st of
    Left e -> do
      print e
    Right ss -> if stopAfterAnalyzer
                then outputST output ss
                else toICode ss
  return ()
  where
    outputST :: FilePath -> [Stmt] -> IO ()
    outputST fp ss = writeFile' fp $ show ss
    outputIC :: FilePath -> String -> IO ()
    outputIC fp str = writeFile' fp str
\end{code}

\begin{code}
toSyntaxTree :: FilePath  -> Bool ->  String -> Either ParseError [Stmt]
toSyntaxTree fileName isCS str = runP parsingStmt (UPS (fromList []) isCS) fileName str
\end{code}

\begin{code}
writeFile' :: FilePath -> String -> IO ()
writeFile' fn str
  | fn == "stdout" = hPutStrLn stdout str
  | otherwise = writeFile fn str

writeFileBS :: FilePath -> BSL.ByteString -> IO ()
writeFileBS "stdout" str = BSLC.hPut stdout str
writeFileBS fn str = BSLC.writeFile fn str

readFile' :: FilePath -> IO String
readFile' "stdin" = getContents
readFile' fn = readFile fn
\end{code}


  
for compiler
\begin{code}
data UC = UC
          { stopAfterAnalyzer :: Bool
          , stopAfterIC       :: Bool
          , output            :: FilePath
          , compileFile       :: FilePath
          , xCaseSensitive    :: Bool
          , xCaseInsensitive  :: Bool
          }
          deriving (Eq,Show,Data)
uc :: UC
uc = UC { stopAfterAnalyzer = False
          &= help "stop and output after parsing"
          &= explicit
          &= name "parsing-stop"
          &= name "p"
        , stopAfterIC = False
          &= help "stop and oupput after creating intermediate code"
          &= explicit
          &= name "ic-stop"
          &= name "i"
        , output = "stdout"
          &= help "parsing and tranlate to final code -- uname compressed code(ucc)"
        , compileFile = "stdin"
          &= args
        , xCaseSensitive = False
          &= help "Case Sensitive"
          &= explicit
          &= name "XCaseSensitive"
        , xCaseInsensitive = True
          &= help "Case Insensitive"
          &= explicit
          &= name "XCaseInsensitive"
        }
\end{code}

\begin{code}
data UPS = UPS (Set String) Bool
instance IsCaseSensitive UPS where
  isCaseSensitive (UPS _ x) = x
  transCaseSensitive _ = Just $ \(UPS x _) -> UPS x True
  transCaseInsensitive _ = Just $ \(UPS x _) -> UPS x False
instance SymbolTable UPS where
  symbolTable (UPS x _) = x
  modifyTable (UPS x y) f = UPS (f x) y
instance UnameParser String UPS Identity
\end{code}
