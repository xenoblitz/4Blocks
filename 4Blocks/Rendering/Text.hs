
module Rendering.Text ( renderText )

where

import Rendering.Point
import Rendering.RGB

import Graphics.Rendering.Cairo

renderText :: Point -> Double -> Double -> RGB -> String -> Render ()
renderText (x,y) s w (r,g,b) t
  = do setFontSize s
       setLineWidth w 
       save
       stroke
       moveTo x y
       textPath t
       setSourceRGBA r g b 0.85
       stroke
       restore