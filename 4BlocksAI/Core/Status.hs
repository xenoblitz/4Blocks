
module Core.Status ( Status ( Start, 
                              Line_1,
                              Line_2,
                              Line_3,
                              Line_4,
                              Playing,
                              GameOver,
                              Paused
                            )
                   )

where

data Status = Start
            | Line_1
            | Line_2
            | Line_3
            | Line_4
            | Playing
            | GameOver
            | Paused
            deriving Eq
            
instance Show Status where
  show Start = "Good Luck!"
  show Line_1 = "One Line!"
  show Line_2 = "Two Lines!"
  show Line_3 = "Three Lines!"
  show Line_4 = "Four Lines !!!"
  show Playing = "Playing..."
  show GameOver = "Game Over!"
  show Paused = "...Paused"
