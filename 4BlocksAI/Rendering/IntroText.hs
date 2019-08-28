
module Rendering.IntroText ( renderIntroText )

where

import Core.Colour
import Core.ColouredPoint
import Core.Game

import Rendering.RGB
import Rendering.Point
import Rendering.Block
import Rendering.Text

import Graphics.Rendering.Cairo

renderIntroText :: Point -> Point -> Render ()
renderIntroText (startX,startY) (incX,incY)
  = do setSourceRGB 0.0 0.0 0.0
       selectFontFace "sans" FontSlantNormal FontWeightNormal
       
       renderText (startX + incX,startY + (incY * 2)) 40 2 (1,1,1) "Welcome to 4Blocks"
       renderText (startX + (incX * 15),startY + (incY * 3) + 5) 20 1 (1,1,1) "in"
       renderText (startX + (incX * 13),startY + (incY * 5)) 40 2 (1,1,1) "Haskell!"
       renderText (startX + incX, startY + (incY * 10)) 25 1 (1,1,1) "Press:"
       
       renderText (startX + incX, startY + (incY * 12)) 25 1 (1,1,1) "[1]"
       renderText (startX + incX, startY + (incY * 14)) 25 1 (1,1,1) "[2]"
       renderText (startX + incX, startY + (incY * 16)) 25 1 (1,1,1) "[3]"
       renderText (startX + incX, startY + (incY * 18)) 25 1 (1,1,1) "[Esc]"
       
       renderText (startX + (incX * 5), startY + (incY * 12)) 25 1 (1,1,1) "---"
       renderText (startX + (incX * 5), startY + (incY * 14)) 25 1 (1,1,1) "---"
       renderText (startX + (incX * 5), startY + (incY * 16)) 25 1 (1,1,1) "---"
       renderText (startX + (incX * 5), startY + (incY * 18)) 25 1 (1,1,1) "---"
       
       renderText (startX + (incX * 7), startY + (incY * 12)) 25 1 (1,1,1) "Human-Player Mode"
       renderText (startX + (incX * 7), startY + (incY * 14)) 25 1 (1,1,1) "Computer-Player Mode"
       renderText (startX + (incX * 7), startY + (incY * 16)) 25 1 (1,1,1) "Mixed-Player Mode"
       renderText (startX + (incX * 7), startY + (incY * 18)) 25 1 (1,1,1) "Exit Program"
       
-- Grey 0.4,0.4,0.4
-- Off White 0.8,0.8,0.8
-- White 1,1,1