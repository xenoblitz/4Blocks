
module Rendering.Background ( renderBackground )

where

import Graphics.Rendering.Cairo

renderBackground :: Render ()
renderBackground 
  = do setSourceRGB 0 0 0
       paint    
