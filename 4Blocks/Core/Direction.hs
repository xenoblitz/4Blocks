
module Core.Direction ( Direction ( Up_Direction,
                                    Right_Direction,
                                    Down_Direction,
                                    Left_Direction 
                                  )
                      )

where

data Direction
  = Up_Direction
  | Right_Direction
  | Down_Direction
  | Left_Direction
    deriving (Eq,Enum)
