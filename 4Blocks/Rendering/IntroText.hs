
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
       renderText (startX + incX, startY + (incY * 9.2)) 15 1 (1,1,1) "Press:"
       
       renderText (startX + incX, startY + (incY * 10.4)) 15 1 (1,1,1) "[1]"
       renderText (startX + incX, startY + (incY * 11.4)) 15 1 (1,1,1) "[Esc]"
       renderText (startX + incX, startY + (incY * 12.4)) 15 1 (1,1,1) "[P]"
       renderText (startX + incX, startY + (incY * 13.4)) 15 1 (1,1,1) "[A] / [Left]"
       renderText (startX + incX, startY + (incY * 14.4)) 15 1 (1,1,1) "[D] / [Right]"
       renderText (startX + incX, startY + (incY * 15.4)) 15 1 (1,1,1) "[S] / [Down]"
       renderText (startX + incX, startY + (incY * 16.4)) 15 1 (1,1,1) "[W] / [Up]"
       renderText (startX + incX, startY + (incY * 17.4)) 15 1 (1,1,1) "[Q] / [,]"
       renderText (startX + incX, startY + (incY * 18.4)) 15 1 (1,1,1) "[E] / [.]"
       
       renderText (startX + (incX * 9), startY + (incY * 10.4)) 15 1 (1,1,1) "---"
       renderText (startX + (incX * 9), startY + (incY * 11.4)) 15 1 (1,1,1) "---"
       renderText (startX + (incX * 9), startY + (incY * 12.4)) 15 1 (1,1,1) "---"
       renderText (startX + (incX * 9), startY + (incY * 13.4)) 15 1 (1,1,1) "---"
       renderText (startX + (incX * 9), startY + (incY * 14.4)) 15 1 (1,1,1) "---"
       renderText (startX + (incX * 9), startY + (incY * 15.4)) 15 1 (1,1,1) "---"
       renderText (startX + (incX * 9), startY + (incY * 16.4)) 15 1 (1,1,1) "---"
       renderText (startX + (incX * 9), startY + (incY * 17.4)) 15 1 (1,1,1) "---"
       renderText (startX + (incX * 9), startY + (incY * 18.4)) 15 1 (1,1,1) "---"
       
       renderText (startX + (incX * 12), startY + (incY * 10.4)) 15 1 (1,1,1) "Start Game"
       renderText (startX + (incX * 12), startY + (incY * 11.4)) 15 1 (1,1,1) "Exit Game"
       renderText (startX + (incX * 12), startY + (incY * 12.4)) 15 1 (1,1,1) "Pause/Resume Game"
       renderText (startX + (incX * 12), startY + (incY * 13.4)) 15 1 (1,1,1) "Shift Left"
       renderText (startX + (incX * 12), startY + (incY * 14.4)) 15 1 (1,1,1) "Shift Right"
       renderText (startX + (incX * 12), startY + (incY * 15.4)) 15 1 (1,1,1) "Soft Drop"
       renderText (startX + (incX * 12), startY + (incY * 16.4)) 15 1 (1,1,1) "Hard Drop"
       renderText (startX + (incX * 12), startY + (incY * 17.4)) 15 1 (1,1,1) "Rotate Left"
       renderText (startX + (incX * 12), startY + (incY * 18.4)) 15 1 (1,1,1) "Rotate Right"
       
       
-- Grey 0.4,0.4,0.4
-- Off White 0.8,0.8,0.8
-- White 1,1,1