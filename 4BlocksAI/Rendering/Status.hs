
module Rendering.Status ( renderStatus )

where

import Core.Colour
import Core.Game

import Rendering.RGB
import Rendering.Point
import Rendering.StatusText
import Rendering.StatusArea
import Rendering.StatusNext

import Graphics.Rendering.Cairo

renderStatus :: Colour -> RGB -> RGB -> Point -> Point -> Point -> Point -> Point -> Point -> Point -> Game -> Render ()
renderStatus fillWellColour shadowWellRGB outlineWellRGB areaOffset areaBlockSize areaSize nextOffset nextBlockSize textOffset textBlockSize game
  = do renderStatusArea fillWellColour shadowWellRGB outlineWellRGB areaOffset areaBlockSize areaSize
       renderStatusNext shadowWellRGB outlineWellRGB nextOffset nextBlockSize game
       renderStatusText textOffset textBlockSize game