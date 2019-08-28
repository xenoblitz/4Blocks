
module Core.Commands ( Game,
                       standardGame,
                       shift_brick_down, 
                       shift_brick_up, 
                       shift_brick_left, 
                       shift_brick_right, 
                       rotate_brick_left, 
                       rotate_brick_right,
                       next_brick,
                       clear_brick_lines,
                       lock_brick,
                       increment_speed,
                       decrement_speed,
                       increment_score,
                       reset_score,
                       increment_lines,
                       reset_lines,
                       soft_drop_brick,
                       hard_drop_brick,
                       lock_clear_next_brick,
                       drop_lock_clear_next_brick,
                       pause_resume )

where 

import Core.Game

import Control.Monad.State


next_brick :: State Game Bool
next_brick 
  = do game <- get
       let (newGame, newResult) = nextBrick game
       put newGame
       return newResult
  
  
shift_brick_down :: State Game Bool
shift_brick_down
  = do game <- get
       let (newGame, newResult) = shiftDown game
       put newGame
       return newResult
       
shift_brick_up :: State Game Bool
shift_brick_up
  = do game <- get
       let (newGame, newResult) = shiftUp game
       put newGame
       return newResult

shift_brick_left :: State Game Bool
shift_brick_left
  = do game <- get
       let (newGame, newResult) = shiftLeft game
       put newGame
       return newResult

shift_brick_right :: State Game Bool 
shift_brick_right
  = do game <- get
       let (newGame, newResult) = shiftRight game
       put newGame
       return newResult

rotate_brick_left :: State Game Bool 
rotate_brick_left
  = do game <- get
       let (newGame, newResult) = rotateLeft game
       put newGame
       return newResult

rotate_brick_right :: State Game Bool 
rotate_brick_right
  = do game <- get
       let (newGame, newResult) = rotateRight game
       put newGame
       return newResult

clear_brick_lines :: State Game Bool
clear_brick_lines
  = do game <- get
       let (newGame, newResult) = clearLines game
       put newGame
       return newResult

lock_brick :: State Game Bool
lock_brick
  = do game <- get
       let (newGame, newResult) = lockBrick game
       put newGame
       return newResult
      
increment_speed :: State Game Bool
increment_speed
  = do game <- get
       let (newGame, newResult) = incrementSpeed game
       put newGame
       return newResult

decrement_speed :: State Game Bool
decrement_speed
  = do game <- get
       let (newGame, newResult) = decrementSpeed game
       put newGame
       return newResult
       
increment_score :: Int -> State Game Bool
increment_score n
  = do game <- get
       let (newGame, newResult) = incrementScore n game
       put newGame
       return newResult
       
reset_score :: State Game Bool
reset_score
  = do game <- get
       let (newGame, newResult) = resetScore game
       put newGame
       return newResult
     
increment_lines :: Int -> State Game Bool
increment_lines n
  = do game <- get
       let (newGame, newResult) = incrementLines n game
       put newGame
       return newResult
       
reset_lines :: State Game Bool
reset_lines
  = do game <- get
       let (newGame, newResult) = resetLines game
       put newGame
       return newResult
       
soft_drop_brick :: State Game Bool
soft_drop_brick
  = do game <- get
       let (newGame, newResult) = softDrop game
       put newGame
       return newResult
       
hard_drop_brick :: State Game Bool
hard_drop_brick
  = do game <- get
       let (newGame, newResult) = hardDrop game
       put newGame
       return newResult
       
lock_clear_next_brick :: State Game Bool
lock_clear_next_brick
  = do lock_brick
       clear_brick_lines
       next_brick
       
drop_lock_clear_next_brick :: State Game Bool
drop_lock_clear_next_brick
  = do hard_drop_brick
       lock_clear_next_brick
       
pause_resume :: State Game Bool
pause_resume
  = do game <- get
       let (newGame, newResult) = pause game
       put newGame
       return newResult