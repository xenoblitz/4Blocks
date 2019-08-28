
module Rendering.StatusText ( renderStatusText )

where

import Core.Colour
import Core.ColouredPoint
import Core.Game

import Rendering.RGB
import Rendering.Point
import Rendering.Block
import Rendering.Text

import Graphics.Rendering.Cairo

renderStatusText :: Point -> Point -> Game -> Render ()
renderStatusText (startX,startY) (incX,incY) game
  = do setSourceRGB 0.0 0.0 0.0
       selectFontFace "sans" FontSlantNormal FontWeightNormal
       
       renderText (startX + incX,startY) 40 2 (1,1,1) "4Blocks"
       renderText (startX + (incX * 4),startY + (incY) + 5) 20 1 (1,1,1) "in"
       renderText (startX + (incX * 2),startY + (incY * 3)) 40 2 (1,1,1) "Haskell!"
       renderText (startX + incX, startY + (incY * 6)) 20 1 (1,1,1) ("Score: " ++ (show scoreValue))
       renderText (startX + incX, startY + (incY * 8)) 20 1 (1,1,1) ("Lines: " ++ (show linesValue))
       renderText (startX + incX, startY + (incY * 10)) 20 1 (1,1,1) ("Level: " ++ (show levelValue))
       renderText (startX + incX, startY + (incY * 12)) 20 1 (1,1,1) ("Goal: " ++ (show goalDiffValue))
       renderText (startX + incX, startY + (incY * 14)) 20 1 (1,1,1) ("Status: " ++ (show statusValue))
       renderText (startX + incX, startY + (incY * 16)) 20 1 (1,1,1) "Next: "
       where gravity = getGravityFromGame game
             goal = getGoalFromGame game
             scoreValue = getScoreFromGame game
             linesValue = getLinesFromGame game
             statusValue = getStatusFromGame game
             goalDiffValue = goal - linesValue
             levelValue = gravity - 1


