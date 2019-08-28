
module Rendering.Intro ( renderIntro )

where

import Core.Colour
import Core.Game

import Rendering.RGB
import Rendering.Point
import Rendering.IntroText
import Rendering.IntroArea

import Graphics.Rendering.Cairo

renderIntro :: Colour -> RGB -> RGB -> Point -> Point -> Point -> Point -> Point -> Render ()
renderIntro fillWellColour shadowWellRGB outlineWellRGB areaOffset areaBlockSize areaSize textStart textInc
  = do renderIntroArea fillWellColour shadowWellRGB outlineWellRGB areaOffset areaBlockSize areaSize
       renderIntroText textStart textInc