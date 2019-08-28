
module Rendering.IntroArea ( renderIntroArea )

where

import Core.Colour
import Core.ColouredPoint

import Rendering.RGB
import Rendering.Point
import Rendering.Block
import Rendering.Text

import Graphics.Rendering.Cairo

renderIntroArea :: Colour -> RGB -> RGB -> Point -> Point -> Point -> Render ()
renderIntroArea fillWellColour shadowWellRGB outlineWellRGB offset blockSize (areaSizeX,areaSizeY)
  = do mapM_ (renderIntroAreaBlock shadowWellRGB outlineWellRGB offset blockSize (areaSizeX,areaSizeY)) introAreaBlocks
    where introAreaBlocksBottom = [(x,areaSizeY,fillWellColour) | x <- [0..areaSizeX]]
          introAreaBlocksTop    = [(x,0,fillWellColour) | x <- [0..areaSizeX]]
          introAreaBlocksLeft   = [(0,y,fillWellColour) | y <- [1..areaSizeY-1]]
          introAreaBlocksRight  = [(areaSizeX,y,fillWellColour) | y <- [1..areaSizeY-1]]
          introAreaBlocksMiddle = [(x,10,fillWellColour) | x <- [1..areaSizeX-1]]
          introAreaBlocks = introAreaBlocksBottom ++ 
                            introAreaBlocksTop ++ 
                            introAreaBlocksLeft ++ 
                            introAreaBlocksRight ++
                            introAreaBlocksMiddle
  
renderIntroAreaBlock :: RGB -> RGB -> Point -> Point -> Point -> ColouredPoint -> Render ()
renderIntroAreaBlock shadowWellRGB outlineWellRGB (offsetX,offsetY) (blockSizeX,blockSizeY) (areaSizeX,areaSizeY) (x,y,c)
  = renderBlock (colourToRGB c) shadowWellRGB outlineWellRGB (actualX,actualY) (blockSizeX,blockSizeY)
    where actualX = renderingIntroAreaX x blockSizeX offsetX
          actualY = renderingIntroAreaY y blockSizeY offsetY
          
renderingIntroAreaX ::  Double -> Double -> Double -> Double
renderingIntroAreaX rawX blockSizeX offsetX = offsetX + (blockSizeX * rawX)

renderingIntroAreaY ::  Double -> Double -> Double -> Double
renderingIntroAreaY rawY blockSizeY offsetY = offsetY + (blockSizeY * rawY)