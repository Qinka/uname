

Uname 模块中，处理中间代码解析的部分

\begin{code}
module Uname.IntermediateCode.Parsec
       ( Warning(..)
       , Parser
       , floatLit
       , point
       , input
       , end
       , parser
       , runIC
       , runICIO
       ) where


import Control.Monad
import Data.Char
import Data.Functor.Identity
import Data.Maybe
import System.IO as IO
import GHC.IO.Handle.Types(Handle(..))
import Text.Parsec
import Uname.Data.IntermediateCode
\end{code}

定义反馈的警告与错误。第一个 String 是错误类型，第二格式文本内容。
\begin{code}
data Warning = Warning String (Maybe String)
             deriving (Eq,Show)
\end{code}

定义解析器的
\begin{code}
type Parser = Parsec  String [Warning]
\end{code}

解析整数部分。
\begin{code}
intOL :: Parser Double
intOL = do
  f <- read <$>  many1 digit
  option '.' $ char '.'
  e <- option id expItem
  return $ e f
pointOL :: Parser Double
pointOL = do
  f <- (read.("0."++)) <$> (char '.' *> many1 digit)
  e <- option id expItem
  return $ e f
bypartL :: Parser Double
bypartL = do
  l <- many1 digit
  p <- char '.'
  r <- many1 digit
  let f = read $ l++p:r
  e <- option id expItem
  return $ e f

expItem :: Parser (Double -> Double)
expItem = do
  char 'E' <|> char 'e'
  g <- sign
  x <- read <$> (many digit)
  return $ f (g x)
  where
    f 0 0 = 0
    f x y = y ^^ x

floatLit' :: Parser Double
floatLit' = sign >>= \x -> x <$> choice [try bypartL, intOL, pointOL,floatFailed]

floatLit :: Parser Double
floatLit = do
  ls <- many1 $ oneOf $ ['0'..'9'] ++ "eE-+."
  return $ read ls

sign :: Num a => Parser (a -> a)
sign = do
  x <- option '+' $ oneOf "+-"
  return $ \a -> case x of
    '+' -> a
    '-' -> -a

floatFailed :: Parser Double
floatFailed = warning True "parsing float number failed" Nothing >> return 0
\end{code}

返回警告
\begin{code}
warning ::Bool ->  String -> Maybe String -> Parser ()
warning is reason text =  do
  pos <- getPosition
  let errStr = "In "
        ++ sourceName pos
        ++ ", line "
        ++ show (sourceLine pos)
        ++ ", column "
        ++ show (sourceColumn pos)
        ++ ": "
        ++ reason
  modifyState (Warning errStr text:)
  when is $ parserFail $ errStr ++ fromMaybe "" ((". with: "++) <$> text)
\end{code}

空白与结尾
\begin{code}
whiteSpace :: Parser ()
whiteSpace = skipMany $ oneOf " \t"

nline :: Parser Char
nline = try crlf <|> newline
end :: Parser ()
end = skipMany nline
\end{code}

获取点
\begin{code}
point :: Parser Point
point = do
  whiteSpace
  x <- floatLit
  whiteSpace
  y <- floatLit
  whiteSpace
  ck <- (map toUpper) <$> many letter
  whiteSpace
  ds <- (floatLit `sepBy` whiteSpace)
  return $ Point (x,y) ck ds
\end{code}

或许新的帧开始标记
\begin{code}
newFrame :: Parser Input
newFrame = do
  whiteSpace
  string "frame"
  whiteSpace
  return FrameSplit
\end{code}

获取设置标记
\begin{code}
setting :: Parser Setting
setting = do
  whiteSpace
  x <-  string "rotate"
    <|> string "scale"
    <|> string "translate"
  whiteSpace
  case x of
    "rotate" -> do
      x <- floatLit
      whiteSpace
      return $ Rotate x
    "scale" -> do
      x <- floatLit
      whiteSpace
      y <- floatLit
      whiteSpace
      return $ Scale (x,y)
    "translate" -> do
      x <- floatLit
      whiteSpace
      y <- floatLit
      whiteSpace
      return $ Translate (x,y)
\end{code}

一条语句失败了
\begin{code}
errorItem :: Parser Input
errorItem = do
  text <- many (noneOf "\n") <* char '\n'
  warning False "parsing point or command failed" $ Just text
  return InvailedStmt
\end{code}

获取一条语句
\begin{code}
input :: Parser Input
input = do
  x <- choice [PointI <$> try point, try newFrame, SettingI <$> try setting,try errorItem] <* end
  spaces
  return x
\end{code}

parser
\begin{code}
parser :: Parser ([Input],[Warning])
parser = do
  rt <- ignore <$> many input <* eof
  st <- getState
  return (rt,st)
  where
    ignore [] = []
    ignore (InvailedStmt:xs) = ignore xs
    ignore (x:xs) = x:ignore xs
\end{code}

parsing 
\begin{code}
runIC :: String -> String -> Either ParseError ([Input],[Warning])
runIC s str = runP parser [] s str

runICIO :: Handle -> IO  (Either ParseError ([Input],[Warning]))
runICIO h = hGetContents h >>= \str -> return $ runIC (filepath h) str
  where
    filepath (DuplexHandle i _ _) = i
    filepath (FileHandle i _ ) = i
\end{code}
