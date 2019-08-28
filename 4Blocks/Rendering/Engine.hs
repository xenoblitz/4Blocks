
module Rendering.Engine ( renderOnePlayerScreen,
                          renderIntroScreen )
                         
where

import Core.Colour
import Core.Game
import Rendering.RGB
import Rendering.Game
import Rendering.Status
import Rendering.Intro
import Rendering.Background

import Graphics.Rendering.Cairo

-------------------------------------------------
-- Block Values
blockX :: Double
blockX = 20

blockY :: Double
blockY = 20

block :: (Double,Double)
block = (blockX,blockY)
-------------------------------------------------
-- Well Values
wellOffX :: Double
wellOffX = 20

wellOffY :: Double
wellOffY = 20

wellOff :: (Double,Double)
wellOff = (wellOffX,wellOffY)

wellBlockX :: Double
wellBlockX = 20

wellBlockY :: Double
wellBlockY = 20

wellBlock :: (Double,Double)
wellBlock = (wellBlockX,wellBlockY)
-------------------------------------------------
-- Status Values
statusAreaOffsetX :: Double
statusAreaOffsetX = wellOffX + (12 * blockX)

statusAreaOffsetY :: Double
statusAreaOffsetY = wellOffY

statusAreaOffset :: (Double,Double)
statusAreaOffset = (statusAreaOffsetX,statusAreaOffsetY)

statusAreaBlockX :: Double
statusAreaBlockX = wellBlockX

statusAreaBlockY :: Double
statusAreaBlockY = wellBlockY

statusAreaBlock :: (Double, Double)
statusAreaBlock = (statusAreaBlockX,statusAreaBlockY)

statusAreaSizeX :: Double
statusAreaSizeX = 10

statusAreaSizeY :: Double
statusAreaSizeY = 22

statusAreaSize :: (Double,Double)
statusAreaSize = (statusAreaSizeX,statusAreaSizeY)

statusNextOffsetX :: Double
statusNextOffsetX = wellOffX + (12.25 * blockX)

statusNextOffsetY :: Double
statusNextOffsetY = wellOffY + (19 * blockY)

statusNextOffset :: (Double,Double)
statusNextOffset = (statusNextOffsetX,statusNextOffsetY)

statusNextBlockX :: Double
statusNextBlockX = blockX / 2

statusNextBlockY :: Double
statusNextBlockY = blockY / 2

statusNextBlock :: (Double,Double)
statusNextBlock = (statusNextBlockX,statusNextBlockY)

statusTextOffsetX :: Double
statusTextOffsetX = wellOffX + (11.5 * blockX)

statusTextOffsetY :: Double
statusTextOffsetY = wellOffY + (3 * blockY)

statusTextOffset :: (Double,Double)
statusTextOffset = (statusTextOffsetX,statusTextOffsetY)

statusTextBlockX :: Double
statusTextBlockX = blockX

statusTextBlockY :: Double
statusTextBlockY = blockY

statusTextBlock :: (Double,Double)
statusTextBlock = (statusTextBlockX,statusTextBlockY)

-------------------------------------------------
-- Intro Values
introAreaOffsetX :: Double
introAreaOffsetX = wellOffX

introAreaOffsetY :: Double
introAreaOffsetY = wellOffY

introAreaOffset :: (Double,Double)
introAreaOffset = (introAreaOffsetX,introAreaOffsetY)

introAreaBlockX :: Double
introAreaBlockX = wellBlockX

introAreaBlockY :: Double
introAreaBlockY = wellBlockY

introAreaBlock :: (Double,Double)
introAreaBlock = (introAreaBlockX,introAreaBlockY)

introAreaSizeX :: Double
introAreaSizeX = 22

introAreaSizeY :: Double
introAreaSizeY = 22

introAreaSize :: (Double,Double)
introAreaSize = (introAreaSizeX,introAreaSizeY)

introTextOffsetX :: Double
introTextOffsetX = wellOffX + (1 * blockY)

introTextOffsetY :: Double
introTextOffsetY = wellOffY + (3 * blockY)

introTextOffset :: (Double,Double)
introTextOffset = (introTextOffsetX,introTextOffsetY)

introTextBlockX :: Double
introTextBlockX = blockX

introTextBlockY :: Double
introTextBlockY = blockY

introTextBlock :: (Double,Double)
introTextBlock = (introTextBlockX,introTextBlockY)
-------------------------------------------------
-- Tetris Colours
outlineWellRGB :: RGB
outlineWellRGB = (0.9, 0.9, 0.9)

shadowWellRGB :: RGB
shadowWellRGB = (0, 0, 0)

fillWellColour :: Colour
fillWellColour = Grey_Colour 
-------------------------------------------------
renderOnePlayerScreen :: Game -> Render ()
renderOnePlayerScreen game
  = do save
       renderBackground
       renderGame shadowWellRGB outlineWellRGB wellOff wellBlock game
       renderStatus fillWellColour shadowWellRGB outlineWellRGB statusAreaOffset statusAreaBlock statusAreaSize statusNextOffset statusNextBlock statusTextOffset statusTextBlock game
       restore
       
renderIntroScreen :: Render ()
renderIntroScreen
  = do save
       renderBackground
       renderIntro fillWellColour shadowWellRGB outlineWellRGB introAreaOffset introAreaBlock introAreaSize introTextOffset introTextBlock
       restore