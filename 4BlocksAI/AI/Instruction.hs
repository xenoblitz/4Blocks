
module AI.Instruction ( Instruction (ShiftLeft,
                                     ShiftRight,
                                     NoAction,
                                     RotateLeft,
                                     RotateRight,
                                     SoftDrop,
                                     HardDrop
                                    ),
                        shiftLeft, 
                        shiftRight,
                        noAction,
                        rotateLeft,
                        rotateRight,
                        softDrop,
                        hardDrop
                      )

where

data Instruction
  = ShiftLeft
  | ShiftRight
  | NoAction
  | RotateLeft
  | RotateRight
  | SoftDrop
  | HardDrop
    deriving (Eq,Show)
    
shiftLeft :: Instruction
shiftLeft = ShiftLeft

shiftRight :: Instruction
shiftRight = ShiftRight

rotateLeft :: Instruction
rotateLeft = RotateLeft

rotateRight :: Instruction
rotateRight = RotateRight

noAction :: Instruction
noAction = NoAction

softDrop :: Instruction
softDrop = SoftDrop

hardDrop :: Instruction
hardDrop = HardDrop

