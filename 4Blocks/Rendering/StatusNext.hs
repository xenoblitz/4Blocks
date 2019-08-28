
module Rendering.StatusNext ( renderStatusNext )

where

import Rendering.RGB
import Rendering.Point
import Rendering.Block

import Core.Shape
import Core.ColouredPoint
import Core.Brick
import Core.Game

import Graphics.Rendering.Cairo

renderStatusNext :: RGB -> RGB -> Point -> Point -> Game -> Render ()
renderStatusNext shadowWellRGB outlineWellRGB (nextOffsetX,nextOffsetY) (nextBlockSizeX,nextBlockSizeY) game
  = do renderStatusNextBrick shadowWellRGB outlineWellRGB (nextOffsetX,nextOffsetY) (nextBlockSizeX,nextBlockSizeY) brick1
       renderStatusNextBrick shadowWellRGB outlineWellRGB (nextOffsetX + (5  * nextBlockSizeX),nextOffsetY) (nextBlockSizeX,nextBlockSizeY) brick2
       renderStatusNextBrick shadowWellRGB outlineWellRGB (nextOffsetX + (10 * nextBlockSizeX),nextOffsetY) (nextBlockSizeX,nextBlockSizeY) brick3
       renderStatusNextBrick shadowWellRGB outlineWellRGB (nextOffsetX + (15 * nextBlockSizeX),nextOffsetY) (nextBlockSizeX,nextBlockSizeY) brick4
    where nextNums = getNextFromGame game
          brick1 = createBlocks $ toEnum $ nextNums !! 0
          brick2 = createBlocks $ toEnum $ nextNums !! 1
          brick3 = createBlocks $ toEnum $ nextNums !! 2
          brick4 = createBlocks $ toEnum $ nextNums !! 3
          
  
renderStatusNextBrick :: RGB -> RGB -> Point -> Point -> [ColouredPoint] -> Render ()
renderStatusNextBrick shadowWellRGB outlineWellRGB offset blockSize brickBlocks
  = mapM_ (renderStatusNextBlock shadowWellRGB outlineWellRGB offset blockSize) brickBlocks
  
renderStatusNextBlock :: RGB -> RGB -> Point -> Point -> ColouredPoint -> Render ()      
renderStatusNextBlock shadowWellRGB outlineWellRGB (offsetX,offsetY) (blockSizeX,blockSizeY) (x,y,c) 
  = renderBlock (colourToRGB c) shadowWellRGB outlineWellRGB (actualX,actualY) (blockSizeX,blockSizeY)
    where actualX = renderingStatusNextX x blockSizeX offsetX
          actualY = renderingStatusNextY y blockSizeY offsetY
          
renderingStatusNextX ::  Double -> Double -> Double -> Double
renderingStatusNextX rawX blockSizeX offsetX = offsetX + (blockSizeX * rawX)

renderingStatusNextY ::  Double -> Double -> Double -> Double
renderingStatusNextY rawY blockSizeY offsetY = offsetY + (blockSizeY * (4-rawY))




