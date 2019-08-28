
module Rendering.Game ( renderGame )

where 

import Rendering.RGB
import Rendering.Point
import Rendering.Well
import Rendering.Brick
import Rendering.GhostBrick

import Core.Game

import Graphics.Rendering.Cairo

renderGame :: RGB -> RGB -> Point -> Point -> Game -> Render ()
renderGame shadowWellColour outlineWellColour (offX,offY) (blockSizeX,blockSizeY) game
  = do renderWell shadowWellColour outlineWellColour (offX,offY) (blockSizeX,blockSizeY) game
       renderGhostBrick shadowWellColour (offX,offY) (blockSizeX,blockSizeY) game
       renderBrick shadowWellColour outlineWellColour (offX,offY) (blockSizeX,blockSizeY) game