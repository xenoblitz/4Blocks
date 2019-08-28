
module Rendering.Brick ( renderBrick )

where

import Rendering.RGB
import Rendering.Block
import Rendering.Point

import Core.ColouredPoint
import Core.Brick
import Core.Well
import Core.Game

import Graphics.Rendering.Cairo

renderBrick :: RGB -> RGB -> Point -> Point -> Game -> Render ()
renderBrick shadowWellRGB outlineWellRGB offset blockSize game
  = do mapM_ (renderBrickBlock shadowWellRGB outlineWellRGB offset blockSize wellSize) brickBlocks
      where brick = getBrickFromGame game
            well = getWellFromGame game
            brickBlocks = getBlocksFromBrick brick
            wellSize = getSizeFromWell well
            
renderBrickBlock :: RGB -> RGB -> Point -> Point -> Point -> ColouredPoint -> Render ()      
renderBrickBlock shadowWellRGB outlineWellRGB (offsetX,offsetY) (blockSizeX,blockSizeY) (wellSizeX,wellSizeY) (x,y,c) 
  = if ((y >= 0) && (y <= (wellSizeY - 3)))
      then renderBlock (colourToRGB c) shadowWellRGB outlineWellRGB (actualX,actualY) (blockSizeX,blockSizeY)
      else return ()
    where actualX = renderingBrickX x blockSizeX offsetX
          actualY = renderingBrickY y blockSizeY offsetY wellSizeY
          
renderingBrickX ::  Double -> Double -> Double -> Double
renderingBrickX rawX blockSizeX offsetX = offsetX + (blockSizeX * rawX)

renderingBrickY ::  Double -> Double -> Double -> Double -> Double
renderingBrickY rawY blockSizeY offsetY wellSizeY = offsetY + (blockSizeY * (wellSizeY-3)) - (blockSizeY * rawY)

