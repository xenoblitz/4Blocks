
module Core.Colour ( Colour ( Cyan_Colour,
                              Yellow_Colour,
                              Purple_Colour,
                              Green_Colour,
                              Red_Colour,
                              Blue_Colour,
                              Orange_Colour,
                              Grey_Colour,
                              Dark_Grey_Colour
                            ) 
                   )

where

data Colour = Cyan_Colour
            | Yellow_Colour
            | Purple_Colour
            | Green_Colour
            | Red_Colour
            | Blue_Colour
            | Orange_Colour
            | Grey_Colour
            | Dark_Grey_Colour
              deriving (Eq, Ord, Enum, Show)