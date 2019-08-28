
module Core.Game ( Game,
                   createGameFixed,
                   createGameRandom,
                   nullGame,
                   standardGame,
                   standardRandomGame,
                   getWellFromGame,
                   getBrickFromGame,
                   getSeedFromGame,
                   getGravityFromGame,
                   getGoalFromGame,
                   getLevelNumFromGame,
                   getScoreFromGame,
                   getLinesFromGame,
                   getNextFromGame,
                   getStatusFromGame,
                   nextBrick,
                   shiftUp,
                   shiftDown,
                   shiftLeft,
                   shiftRight,
                   rotateLeft,
                   rotateRight,
                   incrementSpeed,
                   decrementSpeed,
                   incrementScore,
                   resetScore,
                   incrementLines,
                   resetLines,
                   clearLines,
                   lockBrick,
                   softDrop,
                   hardDrop,
                   pause)

where

import Random
import Data.List

import Core.Brick
import Core.Well
import Core.Level
import Core.Shape
import Core.ColouredPoint
import Core.Status

data Game = Game {
              well :: Well,
              brick :: Brick,
              seed :: StdGen,
              level :: Level,
              score :: Integer,
              completeLines :: Integer,
              nextBrickNums :: [Int],
              status :: Status
            }
            
--------------------------------------------------------------------------------
-- Game Creation
createGameFixed :: Double -> Double -> Int -> Int -> Integer -> Integer -> Game
createGameFixed wellX wellY seedNum levelNum score lines = Game newWell newBrick newSeed newLevel newScore newLines newBrickNums newStatus
  where 
    newWell = generateWell (wellX,wellY)
    initSeed = mkStdGen seedNum
    (headBrickNum,newBrickNums,newSeed) = getNextBrickNum [] initSeed
    newShape = toEnum headBrickNum
    newBrick = createBrickInWell newShape newWell
    newLevel = initLevel levelNum
    newScore = score
    newLines = lines
    newStatus = Start
    
createGameRandom :: Double -> Double -> StdGen -> Int -> Integer -> Integer -> Game
createGameRandom wellX wellY seed levelNum score lines = Game newWell newBrick newSeed newLevel newScore newLines newBrickNums newStatus
  where 
    newWell = generateWell (wellX,wellY)
    (headBrickNum,newBrickNums,newSeed) = getNextBrickNum [] seed
    newShape = toEnum headBrickNum
    newBrick = createBrickInWell newShape newWell
    newLevel = initLevel levelNum
    newScore = score
    newLines = lines
    newStatus = Start
    
nullGame :: Game
nullGame = createGameFixed 0 0 0 0 0 0

standardGame :: Game
standardGame = createGameFixed 10 22 1 0 0 0

standardRandomGame :: StdGen -> Game
standardRandomGame seed = createGameRandom 10 22 seed 0 0 0

--------------------------------------------------------------------------------
-- Interface
getWellFromGame :: Game -> Well
getWellFromGame = well

getBrickFromGame :: Game -> Brick
getBrickFromGame = brick

getSeedFromGame :: Game -> StdGen
getSeedFromGame = seed

getGravityFromGame :: Game -> Int
getGravityFromGame = getGravityFromLevel.level

getGoalFromGame :: Game -> Integer
getGoalFromGame = getGoalFromLevel.level

getLevelNumFromGame :: Game -> Int
getLevelNumFromGame = getLevelNumFromLevel.level

getScoreFromGame :: Game -> Integer
getScoreFromGame = score

getLinesFromGame :: Game -> Integer
getLinesFromGame = completeLines

getNextFromGame :: Game -> [Int]
getNextFromGame = nextBrickNums

getStatusFromGame :: Game -> Status
getStatusFromGame = status
--------------------------------------------------------------------------------
-- Brick-in-Well operations

-- Add Brick points to Well points
addBrickInWell :: Well -> Brick -> Well
addBrickInWell well brick = newWell
  where 
    brickBlocks = getBlocksFromBrick brick
    wellBlocks = getBlocksFromWell well
    wellSize = getSizeFromWell well
    newBlocks = wellBlocks ++ brickBlocks
    newWell = createWell newBlocks wellSize

-- Check if Brick points can be added to Well points    
checkBrickInWell :: Well -> Brick -> Bool
checkBrickInWell well brick = ((length intersectionBlocks) == 0)
  where 
    intersectionBlocks = intersect simpleBrickBlocks simpleWellBlocks
    simpleBrickBlocks = convertColouredToSimplePoints $ getBlocksFromBrick brick
    simpleWellBlocks = convertColouredToSimplePoints $ getBlocksFromWell well
    
-- Lock brick in well if valid brick position
lockBrickInWell :: Well -> Brick -> (Well,Bool)
lockBrickInWell well brick
  = if (checkBrickInWell well brick)
      then (addBrickInWell well brick, True)
      else (well, False)
   
-- Create brick related to Well
createBrickInWell :: Shape -> Well -> Brick
createBrickInWell shape well = newBrick
  where 
    newBrick = createBrick shape (newX,newY)
    (sizeX,sizeY) = getSizeFromWell well
    (newX,newY) = if (shape == O_Shape) 
                    then ((sizeX/2)-1,sizeY-3) 
                    else ((sizeX/2)-2,sizeY-3)
--------------------------------------------------------------------------------
-- Actions on Brick

-- One action on Brick
actionOnBrick :: (Brick -> Brick) -> Game -> (Game,Bool) 
actionOnBrick action (Game well brick seed level score lines next status) = (newGame, newResult)
  where 
    newGame = if newResult 
                then Game well newBrick seed level score lines next status
                else Game well brick seed level score lines next status
    newResult = checkBrickInWell well newBrick
    newBrick = action brick
    
-- Repeated action on Brick 
repeatOnBrick :: (Brick -> Brick) -> Int -> Game -> (Game,Int)
repeatOnBrick action currentTimes oldGame = (newGame,repeatedTimes)
  where Game well brick seed level score lines next status = oldGame 
        tempBrick = action brick
        tempResult = checkBrickInWell well (tempBrick)
        tempGame = Game well tempBrick seed level score lines next status
        (newGame,repeatedTimes)
           = if (tempResult)
               then repeatOnBrick action (currentTimes + 1) tempGame 
               else (Game well brick seed level score lines next status,currentTimes)
--------------------------------------------------------------------------------
-- Actions on Level

incrementLevel :: Game -> (Game,Bool)
incrementLevel (Game well brick seed level score lines next status) = (newGame,newResult)
  where goal = getGoalFromLevel level
        (newLevel,newResult) = if (lines >= goal) 
                                 then (incLevel level,True)
                                 else (level,False)
        newGame = Game well brick seed newLevel score lines next status
        
decrementLevel :: Game -> (Game,Bool)
decrementLevel (Game well brick seed level score lines next status) 
  = (Game well brick seed (decLevel level) score lines next status,True)

--------------------------------------------------------------------------------
-- Actions on Score

-- Increment Score 
incScore :: Int -> Game -> (Game,Bool)
incScore valueScore (Game well brick seed level score lines next status) 
  = (Game well brick seed level (score + toInteger(valueScore)) lines next status,True)

-- Reset Score
zeroScore :: Game -> (Game, Bool)
zeroScore (Game well brick seed level _ lines next status)
  = (Game well brick seed level 0 lines next status,True)

--------------------------------------------------------------------------------
-- Actions on Lines

-- Increment Lines
incLines :: Int -> Game -> (Game, Bool)
incLines valueLines (Game well brick seed level score lines next status) 
  = (Game well brick seed level score (lines + toInteger(valueLines)) next status,True)

-- Reset Level
zeroLines :: Game -> (Game, Bool)
zeroLines (Game well brick seed level score _ next status)
  = (Game well brick seed level score 0 next status,True)

--------------------------------------------------------------------------------
-- Actions on Well

-- Clear Full Lines From Well
clearNFullLines :: Game -> (Game,Int)
clearNFullLines (Game well brick seed level score lines next status) = (Game newWell brick seed level score lines next status,repeatedTimes)
  where (newWell,repeatedTimes) = clearFullLinesFromWell well 0

clearFullLines :: Game -> (Game,Bool)
clearFullLines game = (newGame,newResult)
  where level = getLevelNumFromGame game
        (tempGame,repeatedTimes) = clearNFullLines game
        (newGame,newResult) = case repeatedTimes of
                                0 -> (setStatus Playing tempGame,False)
                                1 -> (updateGameValuesOnClearLines (100 * level) 1 Line_1 tempGame,True)
                                2 -> (updateGameValuesOnClearLines (300 * level) 3 Line_2 tempGame,True)
                                3 -> (updateGameValuesOnClearLines (500 * level) 5 Line_3 tempGame,True)
                                4 -> (updateGameValuesOnClearLines (800 * level) 8 Line_4 tempGame,True)

updateGameValuesOnClearLines :: Int -> Int -> Status -> Game -> Game
updateGameValuesOnClearLines score lines status = fst.incrementSpeed.fst.incLines lines.fst.incScore score.setStatus status
        
-- Lock Brick in Well of Game
lockBrickInPlace :: Game -> (Game,Bool)
lockBrickInPlace (Game well brick seed level score lines next status) = (Game newWell brick seed level score lines next status, newResult)
  where (newWell, newResult) = lockBrickInWell well brick
     
--------------------------------------------------------------------------------
-- Actions on Next
generateNewNext :: StdGen -> ([Int], StdGen)
generateNewNext seed = (newResult,newSeed)
  where (index, newSeed) = randomR (0,5039) seed
        newResult = bagOfBricksPermutations !! index
        
getNextBrickNum :: [Int] -> StdGen -> (Int,[Int],StdGen)
getNextBrickNum [] seed = (headBrickNum, newBrickNums, newSeed)
  where (headBrickNum:newBrickNums, newSeed) = generateNewNext seed
getNextBrickNum brickNums seed = (newHeadBrickNum,newTailBrickNums,newSeed)
  where (headBrickNum:tailBrickNums) = brickNums
        (newBrickNums,newSeed) = if ((length brickNums) < 5)
                                         then (brickNums ++ genBrickNums,genSeed)
                                         else (brickNums, seed)
        (genBrickNums,genSeed) = generateNewNext seed
        (newHeadBrickNum:newTailBrickNums) = newBrickNums
        
bagOfBricksPermutations :: [[Int]]
bagOfBricksPermutations = permutations [0..6]

-- Get next Brick
getNextBrick :: Game -> (Game,Bool)
getNextBrick (Game well brick seed level score lines next status) = (newGame,newResult)
  where
    (nextHead,nextTail,newSeed) = getNextBrickNum next seed
    newShape = toEnum nextHead
    newBrick = createBrickInWell newShape well
    newResult = checkBrickInWell well newBrick
    newGame = if newResult 
                then Game well newBrick newSeed level score lines nextTail status
                else Game endWell endBrick seed level score lines next GameOver -- this means end of game!
    endWell = gameEndWell well
    endBrick = gameEndBrick brick
--------------------------------------------------------------------------------
-- Actions on Status
setStatus :: Status -> Game -> Game
setStatus newStatus (Game well brick seed level score lines next status) = (Game well brick seed level score lines next newStatus) 

getStatus :: Game -> Status
getStatus (Game well brick seed level score lines next status) = status 

checkStatus :: (Game -> (Game,Bool)) -> Game -> (Game,Bool)
checkStatus move game =
  if (getStatus game == Paused)
    then (game,False)
    else move game
--------------------------------------------------------------------------------
-- Randomization
makeRandomValue :: StdGen -> (Int, StdGen)
makeRandomValue seed = randomR (0,6) seed

--------------------------------------------------------------------------------
-- Brick Drops
softBrickDrop :: Game -> (Game,Bool)
softBrickDrop game = (finalGame, finalResult)
  where
    (tempGame, tempResult) = actionOnBrick shiftBrickDown game
    (finalGame, finalResult) 
      = if (tempResult) 
          then incScore 1 tempGame
          else (tempGame, tempResult)

hardBrickDrop :: Game -> (Game,Bool)
hardBrickDrop game = (finalGame,finalResult)
  where
    (tempGame, tempTimes) = repeatOnBrick shiftBrickDown 0 game
    (finalGame, finalResult) 
      = if (tempTimes > 0) 
          then (incScore (tempTimes*2) tempGame)
          else (game, False)

--------------------------------------------------------------------------------
-- Pause Game
pauseGame :: Game -> (Game,Bool)
pauseGame game = 
  case status of 
    Paused    -> (setStatus Playing game, True)
    otherwise -> (setStatus Paused game,True)
  where (Game well brick seed level score lines next status) = game
--------------------------------------------------------------------------------
-- Commands/Module Interface

-- Generate a new brick
nextBrick :: Game -> (Game,Bool)
nextBrick = checkStatus getNextBrick
    
-- Move Down
shiftDown :: Game -> (Game,Bool)
shiftDown = checkStatus $ actionOnBrick shiftBrickDown

-- Move Up
shiftUp :: Game -> (Game,Bool)
shiftUp = checkStatus $ actionOnBrick shiftBrickUp

-- Move Left
shiftLeft :: Game -> (Game,Bool)
shiftLeft = checkStatus $ actionOnBrick shiftBrickLeft

-- Move Right
shiftRight :: Game -> (Game,Bool)
shiftRight = checkStatus $ actionOnBrick shiftBrickRight

-- Rotate Right
rotateRight :: Game -> (Game,Bool)
rotateRight = checkStatus $ actionOnBrick rotateBrickRight 

-- Rotate Left
rotateLeft :: Game -> (Game,Bool)
rotateLeft = checkStatus $ actionOnBrick rotateBrickLeft 

-- Increase Speed
incrementSpeed :: Game -> (Game,Bool)
incrementSpeed = checkStatus $ incrementLevel

-- Decrease Speed
decrementSpeed :: Game -> (Game,Bool)
decrementSpeed = checkStatus $ decrementLevel

-- Increment Score
incrementScore :: Int -> Game -> (Game,Bool)
incrementScore n = checkStatus $ incScore n

-- Reset Score
resetScore :: Game -> (Game,Bool)
resetScore = checkStatus $ zeroScore

-- Increment Lines
incrementLines :: Int -> Game -> (Game,Bool)
incrementLines n = checkStatus $ incLines n

-- Reset Lines
resetLines :: Game -> (Game,Bool)
resetLines = checkStatus $ zeroLines

-- Clear lines
clearLines :: Game -> (Game,Bool)
clearLines = checkStatus $ clearFullLines

-- Lock Brick
lockBrick :: Game -> (Game,Bool)
lockBrick = checkStatus $ lockBrickInPlace

-- Soft Drop Brick
softDrop :: Game -> (Game,Bool)
softDrop = checkStatus $ softBrickDrop

-- Hard Drop Brick
hardDrop :: Game -> (Game,Bool)
hardDrop = checkStatus $ hardBrickDrop

-- Pause Game
pause :: Game -> (Game,Bool)
pause = pauseGame
