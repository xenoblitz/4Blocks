
module Rendering.Well ( renderWell )

where

import Rendering.RGB
import Rendering.Block
import Rendering.Point

import Core.Well
import Core.ColouredPoint
import Core.Game

import Graphics.Rendering.Cairo

renderWell :: RGB -> RGB -> Point -> Point -> Game -> Render ()
renderWell shadowWellRGB outlineWellRGB offset blockSize game 
  = do mapM_ (renderWellBlock shadowWellRGB outlineWellRGB offset blockSize wellSize) wellBlocks
      where
        well = getWellFromGame game
        wellBlocks = getBlocksFromWell well
        wellSize = getSizeFromWell well
        
renderWellBlock :: RGB -> RGB -> Point -> Point -> Point -> ColouredPoint -> Render ()      
renderWellBlock shadowWellRGB outlineWellRGB (offsetX,offsetY) (blockSizeX,blockSizeY) (wellSizeX,wellSizeY) (x,y,c) 
  = if ((y >= 0) && (y <= (wellSizeY - 3)))
      then renderBlock (colourToRGB c) shadowWellRGB outlineWellRGB (actualX,actualY) (blockSizeX,blockSizeY)
      else return ()
    where actualX = renderingWellX x blockSizeX offsetX
          actualY = renderingWellY y blockSizeY offsetY wellSizeY

renderingWellX ::  Double -> Double -> Double -> Double
renderingWellX rawX blockSizeX offsetX = offsetX + (blockSizeX * rawX)

renderingWellY ::  Double -> Double -> Double -> Double -> Double
renderingWellY rawY blockSizeY offsetY wellSizeY = offsetY + (blockSizeY * (wellSizeY-3)) - (blockSizeY * rawY)
