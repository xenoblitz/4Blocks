
module Interface.WindowUpdate ( updateWindowWithPlayerAction,
                                updateWindowWithGravityDelay )

where

import Core.Game
import Core.Commands

import Control.Monad.State
import Control.Concurrent
import Graphics.UI.Gtk
import Data.IORef

-------------------------------------------------------------------------------------------------------------------------
-- Updating of the Game Window with an action performed by the player, by a computer and with an action performed periodically       
updateWindowWithPlayerAction :: DrawingArea -> MVar Game -> State Game Bool -> IO ()
updateWindowWithPlayerAction drawArea gameMVar move
  = do oldGame <- takeMVar gameMVar
       let newGame = execState move oldGame
       widgetQueueDraw drawArea
       putMVar gameMVar newGame
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