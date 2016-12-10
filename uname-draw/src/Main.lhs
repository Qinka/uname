
\begin{code}
module Main
       ( main
       ) where

import Uname.Data.IntermediateCode
import Uname.IntermediateCode.Parsec
import Uname.Parser

import ShowAll

import Control.Monad (when)
import Control.Monad.Trans.Reader (runReaderT)
import Codec.Compression.Lzma
import Data.Either
import Data.GI.Base
import Data.IORef
import Data.Maybe
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Foreign.Ptr (castPtr)
import System.Console.CmdArgs hiding ((:=))
import System.IO
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified GI.Cairo as GI.Cairo
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf as GP
import qualified GI.Gtk as Gtk
\end{code}

\begin{code}
renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))
\end{code}

\begin{code}
main :: IO ()
main = do
  u@UD{..} <- cmdArgs ud
  Gtk.init Nothing
  window <- new Gtk.Window
    [ #decorated       := True
    , #resizable       := True
    , #windowPosition   := Gtk.WindowPositionCenterAlways
    , #appPaintable    := True
    , #title           := "Uname Simple Draw"
    , #defaultWidth    := 1024
    , #defaultHeight   := 1024
    ]
  on window #keyPressEvent $ \event -> do
    name <- event `get` #keyval >>= Gdk.keyvalName
    when (name == Just "Escape") Gtk.mainQuit
    return False
  after window #destroy Gtk.mainQuit
  cs <- decompress <$> readFileBS input
  let rt = runIC input $ BSLC.unpack cs
  isRef <- newIORef []
  case rt of
    Left e -> print e >> Gtk.mainQuit
    Right (is,ws) -> do
      putStrLn $ unlines $
        (\(Warning str x) -> str  ++ fromMaybe "" ((" "++) <$> x)) <$> ws
      let is' = reverse.takeWhile (/= FrameSplit) $ reverse  is
      modifyIORef isRef (++is')
  on window #draw $ \context -> do{-
    width <- fromIntegral <$> #getAllocatedWidth window
    height <- fromIntegral <$> #getAllocatedWidth window-}
    is <- readIORef isRef
    renderWithContext context $ do
      save
      setOperator OperatorSource
      setSourceRGB 1 1 1
      paint
      restore
      rotate rr
      scale ssX ssY
      translate ttX ttY
      setSourceRGB 1 0 0
      setLineWidth $ 1/(ttX + ttY)
      let st = (0,(1,1),(0,0))
      drawPoints st.rights $ toPoints st is
      return ()
    return True
  showAllW window
  Gtk.main
\end{code}

\begin{code}
drawPoints st xs = mapM_ (drawPoint st) xs
drawPoint (_,(sX,sY),_) (x,y) = do
  newPath
  arc x y s 0 (2*pi)
  fill
  closePath
  where s = 0.1 /(sX+sY)
{-
drawPoints [] = stroke
drawPoints [x] = stroke
drawPoints ((x1,y1):(x2,y2):xs) = do
  newPath
  moveTo x1 y1
  lineTo x2 y2
  closePath
  drawPoints $ (x2,y2):xs
-}
{-
drawPoints [] = return (){-
drawPoints [(x,y)] = do
  rectangle x y 0.5 0.5
  fill-}
drawPoints [x] = return ()
drawPoints ((x,y):xs) = do
  moveTo x y
  mapM (uncurry lineTo) xs
  stroke
drawPoint xs st =
-}
\end{code}

\begin{code}
readFileBS :: FilePath -> IO BSLC.ByteString
readFileBS "stdin" = BSLC.getContents
readFileBS fn      = BSLC.readFile fn
\end{code}

\begin{code}
type St = (Double,(Double,Double),(Double,Double))
toPoints :: St -> [Input] -> [Either () (Double,Double)]
toPoints _ [] = []
toPoints st (FrameSplit:is) = Left ():toPoints st is
toPoints st (PointI p:is) = Right (transP st (#getXY p)):toPoints st is
toPoints (r,(sX,sY),(tX,tY)) (SettingI i:is) = toPoints st' is
  where st' = case i of
          Scale (x,y) -> (r,(sX*x,sY*y),(tX,tY))
          Translate (x,y) -> (r,(sX,sY),(x+tX,y+tY))
          Rotate rr -> (rr+r,(sX,sY),(tX,tY))
transP :: St -> (Double,Double) -> (Double,Double)
transP (r,(sX,sY),(tX,tY)) (x,y) = (x''+tX,y''+tY)
  where (x',y') = (sX*x,sY*y)
        (x'',y'') = (x' * cos r + y' * sin r, y' * cos r - x' * sin r)
\end{code}

\begin{code}
data UD = UD
          { rr      :: Double
          , ssX     :: Double
          , ssY     :: Double
          , ttX     :: Double
          , ttY     :: Double
          , input   :: String
          } deriving (Data,Eq,Show)

ud = UD
  { rr  = 0
  , ssX = 1
  , ssY = 1
  , ttX = 0
  , ttY = 0
  , input = "stdin"
  }
\end{code}

