{-# LANGUAGE FlexibleInstances #-}

module AI.Effector( AI,
                    initAI,
                    stepAI )

where

import AI.Instruction
import AI.Program
import AI.Interface
import AI.Thinker

---------------------------------------------------------------------------------------------------
-- Interface

type AI = Maybe (Program Instruction)

initAI :: AI
initAI = Nothing


stepAI :: (Game, AI) -> (Instruction, AI)
stepAI (game, Nothing) = thinkAI game
stepAI (game, Just prog)
  = if (not (isGameOver game))
      then case (step (game, prog)) of
             (maybeProg, Nothing)   -> stepAI (game, maybeProg)
             (maybeProg, Just inst) -> (inst, maybeProg)
      else (noAction, Just skip)

thinkAI :: Game -> (Instruction, AI)
thinkAI game = (noAction, Just (think game))
      
step :: (Game, Program Instruction) -> (Maybe (Program Instruction), Maybe Instruction)
-- Rule: SKIP -------------------------------------------------------------------------
step (_, Skip) = (Nothing, Nothing)                                                       
-- Rule: PERFORM ----------------------------------------------------------------------
step (_ ,Perform inst) = (Nothing, Just inst)                                             
step (game,prog1 :> prog2) = 
  case (step (game,prog1)) of
    -- Rule: SEQ. COMP. 1 -------------------------------------------------------------
    (Nothing, maybeInst) -> (Just prog2, maybeInst)                        
    -- Rule: SEQ. COMP. 2 -------------------------------------------------------------
    (Just prog1b, maybeInst) -> (Just (prog1b :> prog2), maybeInst)                            
step (game,IfThenElse pred prog1 prog2) = 
  if (pred game)
    -- Rule: IF-THEN-ELSE 1 -----------------------------------------------------------
    then step (game,prog1)
    -- Rule: IF-THEN-ELSE 2 -----------------------------------------------------------
    else step (game,prog2)																																
step (game,While pred prog) = 
  if (pred game)
    then 
      case (step (game,prog)) of
        -- Rule: WHILE 2 --------------------------------------------------------------
        (Nothing, maybeInst) -> (Just (while pred prog), maybeInst)                 
        -- Rule: WHILE 1 --------------------------------------------------------------
        (Just progb, maybeInst) -> (Just (progb :> while pred prog), maybeInst)  
    -- Rule: WHILE 3 ------------------------------------------------------------------             
    else (Nothing,Nothing)            
step (game,ForThisPiece prog) =
  if (isNewBrick game)
    -- Rule: FOR-THIS-PIECE 3 ---------------------------------------------------------
    then (Nothing, Nothing)                                                               
    else
      case (step (game,prog)) of                 
        -- Rule: FOR-THIS-PIECE 2 -----------------------------------------------------
        (Nothing, maybeInst) -> (Just (forThisPiece (perform noAction)), maybeInst) 
        -- Rule: FOR-THIS-PIECE 1 -----------------------------------------------------
        (Just progb, maybeInst) -> (Just (forThisPiece progb), maybeInst)   
---------------------------------------------------------------------------------------------------


