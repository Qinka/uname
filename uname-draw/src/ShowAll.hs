module ShowAll where

import Data.GI.Base
import GI.Gtk

showAllW w = #showAll (w :: Window)
