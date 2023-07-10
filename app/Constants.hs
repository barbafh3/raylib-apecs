module Constants where

import Foreign.C.Types (CFloat (..))
import GHC.Float (int2Float)

tilesetPath, uiAtlasPath, mainFontPath :: String
tilesetPath = "assets/tileset.png"
uiAtlasPath = "assets/ui.png"
mainFontPath = "assets/prstartk.ttf"

screenWidth :: Int
screenWidth = 1280

screenHeight :: Int
screenHeight = 720

screenWidthF :: Float
screenWidthF = int2Float screenWidth

screenHeightF :: Float
screenHeightF = int2Float screenHeight

screenWidthCF :: CFloat
screenWidthCF = CFloat $ int2Float screenWidth

screenHeightCF :: CFloat
screenHeightCF = CFloat $ int2Float screenHeight