
module Rendering.Block ( renderBlock )

where

import Rendering.RGB
import Rendering.Point
import Rendering.Square

import Graphics.Rendering.Cairo

renderBlock :: RGB -> RGB -> RGB -> Point -> Point -> Render ()
renderBlock fillRGB shadowRGB outlineRGB (startx,starty) (sizex, sizey)
  = do renderSquare outlineRGB (startx,starty) (sizex,sizey)
       renderSquare shadowRGB (startx+1,starty+1) (sizex-1,sizey-1)
       renderSquare fillRGB (startx+1,starty+1) (sizex-2,sizey-2)