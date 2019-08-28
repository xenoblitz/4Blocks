
module Rendering.Square ( renderSquare )

where

import Rendering.RGB
import Rendering.Point

import Graphics.Rendering.Cairo

renderSquare :: RGB -> Point -> Point -> Render ()
renderSquare (r,g,b) (x1,y1) (x2,y2)
  = do rectangle x1 y1 x2 y2
       setSourceRGB r g b
       fill