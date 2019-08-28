
module AI.Program ( Program (Skip,
                             (:>),
                             Perform,
                             IfThenElse,
                             While,
                             ForThisPiece),
                    skip, 
                    perform,           
                    ifThenElse,   
                    ifThen,
                    forever,
                    while,
                    forThisPiece
                  )                            

where

import Core.Game
import AI.Instruction

type Pred = Game -> Bool

data Program a = Skip                             
               | Program a :> Program a
               | Perform a 
               | IfThenElse Pred (Program a) (Program a) 
               | While Pred (Program a)
               | ForThisPiece (Program a)
               | Fit Int
               
perform :: Instruction -> Program Instruction
perform inst = Perform inst

skip :: Program Instruction
skip = Skip

ifThenElse :: Pred -> Program Instruction -> Program Instruction -> Program Instruction
ifThenElse pred prog1 prog2 = IfThenElse pred prog1 prog2

ifThen :: Pred -> Program Instruction -> Program Instruction
ifThen pred prog = ifThenElse pred prog skip

while :: Pred -> Program Instruction -> Program Instruction
while pred prog = While pred prog

forever :: Program Instruction -> Program Instruction
forever prog = while (\_ -> True) prog

forThisPiece :: Program Instruction -> Program Instruction
forThisPiece prog = ForThisPiece prog

               
instance Show a => Show (Program a) where
  show Skip               = "Skip"
  show (a :> b)           = show a ++ " :> " ++ show b
  show (Perform a)        = "(Perform " ++ show a ++ ")"
  show (IfThenElse _ a b) = "(If (cond) Then " ++ show a  ++ "Else " ++ show b ++ ")"
  show (While _ a)        = "(While (cond) " ++ show a ++ ")"
  show (ForThisPiece a)   = "(ForThisPiece " ++ show a ++ ")"
