
module Rendering.GhostBrick ( renderGhostBrick )

where

import Rendering.RGB
import Rendering.Block
import Rendering.Point

import Core.ColouredPoint
import Core.Colour
import Core.Brick
import Core.Well
import Core.Game

import Graphics.Rendering.Cairo

renderGhostBrick :: RGB -> Point -> Point -> Game -> Render ()
renderGhostBrick fillWellColour offset blockSize game
  = do mapM_ (renderGhostBrickBlock fillWellColour offset blockSize wellSize) tempBrickBlocks
      where (tempGame,_) = hardDrop game
            tempBrick = getBrickFromGame tempGame
            tempWell = getWellFromGame tempGame
            tempBrickBlocks = getBlocksFromBrick tempBrick
            wellSize = getSizeFromWell tempWell         
            
renderGhostBrickBlock :: RGB -> Point -> Point -> Point -> ColouredPoint -> Render ()      
renderGhostBrickBlock fillWellColour (offsetX,offsetY) (blockSizeX,blockSizeY) (wellSizeX,wellSizeY) (x,y,c) 
  = if ((y >= 0) && (y <= (wellSizeY - 3)))
      then renderBlock fillWellColour (actualC) (actualC) (actualX,actualY) (blockSizeX,blockSizeY)
      else return ()
    where actualX = renderingGhostBrickX x blockSizeX offsetX
          actualY = renderingGhostBrickY y blockSizeY offsetY wellSizeY
          actualC = renderingGhostBrickC c
          
renderingGhostBrickX ::  Double -> Double -> Double -> Double
renderingGhostBrickX rawX blockSizeX offsetX = offsetX + (blockSizeX * rawX)

renderingGhostBrickY ::  Double -> Double -> Double -> Double -> Double
renderingGhostBrickY rawY blockSizeY offsetY wellSizeY = offsetY + (blockSizeY * (wellSizeY-3)) - (blockSizeY * rawY)

renderingGhostBrickC :: Colour -> RGB
renderingGhostBrickC c = colourToRGB c

