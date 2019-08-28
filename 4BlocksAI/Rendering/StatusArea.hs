
module Rendering.StatusArea ( renderStatusArea )

where 

import Core.Colour
import Core.ColouredPoint
import Core.Game

import Rendering.RGB
import Rendering.Point
import Rendering.Block

import Graphics.Rendering.Cairo

renderStatusArea :: Colour -> RGB -> RGB -> Point -> Point -> Point -> Render ()
renderStatusArea fillWellColour shadowWellRGB outlineWellRGB offset blockSize (areaSizeX,areaSizeY)
  = do mapM_ (renderStatusAreaBlock shadowWellRGB outlineWellRGB offset blockSize (areaSizeX,areaSizeY)) statusAreaBlocks
    where statusAreaBlocksBottom = [(x,areaSizeY,fillWellColour) | x <- [0..areaSizeX]]
          statusAreaBlocksTop    = [(x,0,fillWellColour) | x <- [0..areaSizeX]]
          statusAreaBlocksRight  = [(areaSizeX,y,fillWellColour) | y <- [1..areaSizeY-1]]
          statusAreaBlocks = statusAreaBlocksBottom ++ 
                             statusAreaBlocksTop ++ 
                             statusAreaBlocksRight
  
renderStatusAreaBlock :: RGB -> RGB -> Point -> Point -> Point -> ColouredPoint -> Render ()
renderStatusAreaBlock shadowWellRGB outlineWellRGB (offsetX,offsetY) (blockSizeX,blockSizeY) (areaSizeX,areaSizeY) (x,y,c)
  = renderBlock (colourToRGB c) shadowWellRGB outlineWellRGB (actualX,actualY) (blockSizeX,blockSizeY)
    where actualX = renderingStatusAreaX x blockSizeX offsetX
          actualY = renderingStatusAreaY y blockSizeY offsetY
          
renderingStatusAreaX ::  Double -> Double -> Double -> Double
renderingStatusAreaX rawX blockSizeX offsetX = offsetX + (blockSizeX * rawX)

renderingStatusAreaY ::  Double -> Double -> Double -> Double
renderingStatusAreaY rawY blockSizeY offsetY = offsetY + (blockSizeY * rawY)
