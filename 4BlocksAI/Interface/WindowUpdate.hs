
module Interface.WindowUpdate ( updateWindowWithPlayerAction,
                                updateWindowWithComputerAction,
                                updateWindowWithGravityDelay )

where

import Core.Game
import Core.Commands
import AI.Effector
import AI.Instruction
import AI.Interface -- remove when ready from debugging, for getGameStatus

import Control.Monad.State
import Control.Concurrent
import Graphics.UI.Gtk
import Data.IORef

import Debug.Trace

-------------------------------------------------------------------------------------------------------------------------
-- Updating of the Game Window with an action performed by the player, by a computer and with an action performed periodically       
updateWindowWithPlayerAction :: DrawingArea -> MVar Game -> State Game Bool -> IO ()
updateWindowWithPlayerAction drawArea gameMVar move
  = do oldGame <- takeMVar gameMVar
       let newGame = execState move oldGame
       widgetQueueDraw drawArea
       putMVar gameMVar newGame
       return ()

updateWindowWithComputerAction :: DrawingArea -> MVar Game -> MVar AI -> IORef Bool -> IORef Bool -> IO ()
updateWindowWithComputerAction drawArea gameMVar aiProgramMVar aiToggleIORef aiPauseIORef
  = do -- retrieve game state
       oldGame <- takeMVar gameMVar
       -- retrieve moves outstanding
       currentMoves <- takeMVar aiProgramMVar
       -- retrieve AI on/off
       toggleFlag <- readIORef aiToggleIORef
       -- retrieve AI pause/resume
       pauseFlag <- readIORef aiPauseIORef       
              
           -- step game with requested next move if AI toggle flag is on         
       let (move, newCurrentMoves) = case (toggleFlag, pauseFlag) of
                                       (True, False) -> stepAI (oldGame, currentMoves)
                                       (True, True ) -> (NoAction, currentMoves)
                                       otherwise     -> (NoAction, Nothing)
           -- get new game state after applying move
           newGame = case (move) of
                       ShiftLeft   -> execState shift_brick_left oldGame
                       ShiftRight  -> execState shift_brick_right oldGame
                       RotateLeft  -> execState rotate_brick_left oldGame
                       RotateRight -> execState rotate_brick_right oldGame
                       SoftDrop    -> execState soft_drop_brick oldGame
                       HardDrop    -> execState drop_lock_clear_next_brick oldGame
                       NoAction    -> oldGame
                       
       -- display current program and latest move
       {-
       unless (getGameStatus oldGame == "...Paused")
         (do putStrLn ("----------------------------------------------------------------------------------------------------------------")
             putStrLn ("Current Move: " ++ show move)
             putStrLn ("Program: " ++ show newCurrentMoves)
         )
       -}
       -- save moves still outstanding
       putMVar aiProgramMVar newCurrentMoves
       -- save new game state
       putMVar gameMVar newGame
       -- create a new one-time update for the screen by calling oneself 
       
       timeoutAdd (updateWindowWithComputerAction drawArea gameMVar aiProgramMVar aiToggleIORef aiPauseIORef >> return False) calculateComputerPlayDelay
       
       return ()   
       
updateWindowWithGravityDelay :: DrawingArea -> MVar Game -> IO ()
updateWindowWithGravityDelay drawArea gameMVar
  = do -- retrieve game state
  
       oldGame <- takeMVar gameMVar

       let -- attempt to move brick down
           (result,moveDownGame) = runState shift_brick_down oldGame
           -- if it is not possible to move brick down lock it in place and...
           newGame = if result
                       then moveDownGame
                       else execState lock_clear_next_brick moveDownGame
           -- calculate new delay
           newDelay = (calculateGravityDelay.getGravityFromGame) newGame
            
       -- save new game state
       putMVar gameMVar newGame
       -- create a new one-time update for the screen by calling oneself
       timeoutAdd (updateWindowWithGravityDelay drawArea gameMVar >> return False) newDelay
       return ()
-------------------------------------------------------------------------------------------------------------------------
-- Delay Calculations
calculateGravityDelay :: Int -> Int
calculateGravityDelay g
  | g > 0     = 1000 `div` g
  | otherwise = ((abs g) + 1) * 1000
  
calculateComputerPlayDelay :: Int
calculateComputerPlayDelay = 50