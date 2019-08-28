module Interface.MainWindow ( mainWindow )

where

import Interface.WindowUpdate

import Rendering.Engine

import Core.Commands
import Core.Game

import Random
import Control.Monad.State
import Control.Concurrent
import Graphics.UI.Gtk
import qualified Data.Set as Set; import Data.Set (Set)
import Data.IORef;

mainWindow :: IO ()
mainWindow
  = do initGUI
       -------------------------------------------------------------------------------------------
       -- setup window
       window <- windowNew
       set window [
                    windowTitle := "4Blocks in Haskell!",
                    windowDefaultWidth := 500, 
                    windowDefaultHeight := 500                    
                  ]
       frame <- frameNew
       containerAdd window frame
       canvas <- drawingAreaNew
       containerAdd frame canvas
       widgetModifyBg canvas StateNormal (Color 0 0 0)
       widgetShowAll window 
      
       -------------------------------------------------------------------------------------------
       -- Boolean value variable used to check if mode key has already been pressed
       modeSelectedIORef <- newIORef False
       
       -- ioref for set of keys pressed
       keysIORef <- newIORef (Set.empty)
       
       -- get global random seed
       seed <- newStdGen
       
       -- mutable variable for game state
       gameMVar <- newMVar $ standardRandomGame seed
       
       -------------------------------------------------------------------------------------------
       -- canvas drawing event
       drawin <- widgetGetDrawWindow canvas
       canvas `on` exposeEvent $ tryEvent $ do 
         liftIO $ do
           selected <- readIORef modeSelectedIORef
           if (not selected)
             then renderWithDrawable drawin renderIntroScreen
             else do game <- readMVar gameMVar
                     renderWithDrawable drawin $ renderOnePlayerScreen game
                          
       -------------------------------------------------------------------------------------------
       -- key press event       
       window `on` keyPressEvent $ tryEvent $ do
         key <- eventKeyName
         liftIO $ 
           if (key == "Escape") 
             then mainQuit
             else 
               do selected <- readIORef modeSelectedIORef
                  when (not selected) $
                    case key of
                      "1" -> do writeIORef modeSelectedIORef True
                                -- gravity delay loop
                                updateWindowWithGravityDelay canvas gameMVar
                      _   -> return ()
                  when selected $ 
                    case key of
                      "Left"  -> do updateWindowWithPlayerAction canvas gameMVar shift_brick_left        
                      "A"     -> do updateWindowWithPlayerAction canvas gameMVar shift_brick_left
                      "a"     -> do updateWindowWithPlayerAction canvas gameMVar shift_brick_left
                      "Right" -> do updateWindowWithPlayerAction canvas gameMVar shift_brick_right
                      "D"     -> do updateWindowWithPlayerAction canvas gameMVar shift_brick_right
                      "d"     -> do updateWindowWithPlayerAction canvas gameMVar shift_brick_right
                      "Down"  -> do updateWindowWithPlayerAction canvas gameMVar soft_drop_brick
                      "S"     -> do updateWindowWithPlayerAction canvas gameMVar soft_drop_brick
                      "s"     -> do updateWindowWithPlayerAction canvas gameMVar soft_drop_brick
                      "Up"    -> do updateWindowWithPlayerAction canvas gameMVar drop_lock_clear_next_brick
                      "W"     -> do updateWindowWithPlayerAction canvas gameMVar drop_lock_clear_next_brick
                      "w"     -> do updateWindowWithPlayerAction canvas gameMVar drop_lock_clear_next_brick
                      "q"     -> do keys <- readIORef keysIORef
                                    unless (key `Set.member `keys) $ 
                                      do writeIORef keysIORef (Set.insert key keys) 
                                         updateWindowWithPlayerAction canvas gameMVar rotate_brick_left
                      "Q"     -> do keys <- readIORef keysIORef
                                    unless (key `Set.member `keys) $ 
                                      do writeIORef keysIORef (Set.insert key keys) 
                                         updateWindowWithPlayerAction canvas gameMVar rotate_brick_left
                      "comma" -> do keys <- readIORef keysIORef
                                    unless (key `Set.member `keys) $ 
                                      do writeIORef keysIORef (Set.insert key keys) 
                                         updateWindowWithPlayerAction canvas gameMVar rotate_brick_left
                      "e"     -> do keys <- readIORef keysIORef
                                    unless (key `Set.member `keys) $ 
                                      do writeIORef keysIORef (Set.insert key keys) 
                                         updateWindowWithPlayerAction canvas gameMVar rotate_brick_right
                      "E"     -> do keys <- readIORef keysIORef
                                    unless (key `Set.member `keys) $ 
                                      do writeIORef keysIORef (Set.insert key keys) 
                                         updateWindowWithPlayerAction canvas gameMVar rotate_brick_right
                      "period" -> do keys <- readIORef keysIORef
                                     unless (key `Set.member `keys) $ 
                                       do writeIORef keysIORef (Set.insert key keys) 
                                          updateWindowWithPlayerAction canvas gameMVar rotate_brick_right
                      "p"      -> do keys <- readIORef keysIORef
                                     unless (key `Set.member `keys) $ 
                                       do writeIORef keysIORef (Set.insert key keys) 
                                          updateWindowWithPlayerAction canvas gameMVar pause_resume 
                      "P"      -> do keys <- readIORef keysIORef
                                     unless (key `Set.member `keys) $ 
                                       do writeIORef keysIORef (Set.insert key keys) 
                                          updateWindowWithPlayerAction canvas gameMVar pause_resume            
                      _        -> return ()
               
       -------------------------------------------------------------------------------------------               
       -- key release event
       window `on` keyReleaseEvent $ tryEvent $ do
         key <- eventKeyName
         liftIO $ do
           selected <- readIORef modeSelectedIORef
           when selected $ 
             case key of
               "q"      -> do keys <- readIORef keysIORef
                              writeIORef keysIORef (Set.delete key keys)
               "Q"      -> do keys <- readIORef keysIORef
                              writeIORef keysIORef (Set.delete key keys)
               "comma"  -> do keys <- readIORef keysIORef
                              writeIORef keysIORef (Set.delete key keys)
               "e"      -> do keys <- readIORef keysIORef
                              writeIORef keysIORef (Set.delete key keys)
               "E"      -> do keys <- readIORef keysIORef
                              writeIORef keysIORef (Set.delete key keys)
               "period" -> do keys <- readIORef keysIORef
                              writeIORef keysIORef (Set.delete key keys)
               "p"      -> do keys <- readIORef keysIORef
                              writeIORef keysIORef (Set.delete key keys)
               "P"      -> do keys <- readIORef keysIORef
                              writeIORef keysIORef (Set.delete key keys)
               _        -> return ()
                              
       -------------------------------------------------------------------------------------------
       -- window close event
       window `on` deleteEvent $ tryEvent $ liftIO mainQuit
       
       -- redraw window periodically
       timeoutAdd (widgetQueueDraw window >> return True) 20
       
   
       mainGUI