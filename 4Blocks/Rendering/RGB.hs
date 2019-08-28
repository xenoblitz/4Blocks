
module Rendering.RGB ( RGB,
                       colourToRGB )

where

import Core.Colour

type RGB = (Double, Double, Double)

colourToRGB :: Colour -> RGB
colourToRGB Yellow_Colour    = (1, 1, 0)
colourToRGB Orange_Colour    = (1, 0.65, 0)
colourToRGB Blue_Colour      = (0, 0, 1)
colourToRGB Purple_Colour    = (0.5, 0, 0.5)
colourToRGB Cyan_Colour      = (0, 1, 1)
colourToRGB Green_Colour     = (0, 1, 0)
colourToRGB Red_Colour       = (1, 0, 0)
colourToRGB Grey_Colour      = (0.75, 0.75, 0.75)
colourToRGB Dark_Grey_Colour = (0.10, 0.10, 0.10)