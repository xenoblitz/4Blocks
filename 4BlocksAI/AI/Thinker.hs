
module AI.Thinker (think,
                   fit)

where

import AI.Program
import AI.Instruction
import AI.Interface
import AI.Util

import Data.List
import Data.Function

import Debug.Trace

-------------------------------------------------------------------------------------------------------------------------
-- Interface
-------------------------------------------------------------------------------------------------------------------------
think :: Game -> Program Instruction
think game 
  | detectNewGame game                          = newGameStrategy game
  | detectCompleteFourLines game                = completeFourLinesStrategy game
  | detectCompleteAnyLinesToMinimizeHeight game = completeAnyLinesToMinimizeHeightStrategy game
  | detectCompleteLinesMindingHoles game        = completeLinesMindingHolesStrategy game
  | detectEqualizeHeights game                  = equalizeHeightsStrategy game
  | detectSideFit game                          = sideFitStrategy game
  | otherwise                                   = minHeightLowestBestFitStrategy game

  --  | detectBrickTSpin          = trace "brick T-spin" $ brickTSpinStrategy game
  
-- TO DO:
-- Implement TSpin
-- Implement Next Brick
-- Perhaps improve Side Fit Fitness
            
fit :: Int -> Program Instruction
fit 0 = skip
fit 1 = perform rotateRight
fit 2 = perform rotateRight :> perform rotateRight
fit 3 = perform rotateLeft

-------------------------------------------------------------------------------------------------------------------------
-- New Game Strategy
-------------------------------------------------------------------------------------------------------------------------    

-- detect new game strategy
detectNewGame :: Game -> Bool
detectNewGame game = isLineEmpty game 1

-- generates script for a new game
newGameStrategy :: Game -> Program Instruction 
newGameStrategy game
  | isBrickOShaped game = moveNormalProgram game 1
  | isBrickLShaped game = moveNormalProgram game 8
  | isBrickJShaped game = moveNormalProgram game 1
  | isBrickTShaped game = moveNormalProgram game 1
  | isBrickIShaped game = moveNormalProgram game 1
  | isBrickSShaped game = moveNormalProgram game 1
  | isBrickZShaped game = moveNormalProgram game 8
  
-------------------------------------------------------------------------------------------------------------------------
-- Complete Four Lines Strategy
-------------------------------------------------------------------------------------------------------------------------        

-- detects when it is possible to complete 4 lines
detectCompleteFourLines :: Game -> Bool
detectCompleteFourLines game = fourLinesDetected
  where (_, _, _, fourLines) = getLinePossibilities game    
        fourLinesDetected = (length fourLines > 0)

-- generates script to complete 4 lines        
completeFourLinesStrategy :: Game -> Program Instruction
completeFourLinesStrategy game = generateNormalFitProgram game fLocX orient
  where (_, _, _, fourLines) = getLinePossibilities game
        ((fLocX,fLocY),_,orient) = head $ sortBy (compare `on` (snd.trip1)) fourLines
        
-------------------------------------------------------------------------------------------------------------------------
-- Complete Any Lines To Minimize Height Strategy
-------------------------------------------------------------------------------------------------------------------------

-- attempts to complete 1-3 lines in order to minimize height
detectCompleteAnyLinesToMinimizeHeight :: Game -> Bool
detectCompleteAnyLinesToMinimizeHeight game = (oneLineDetected || twoLinesDetected || threeLinesDetected) && growingHeight
  where (oneLine,twoLines,threeLines,_) = getLinePossibilities game
        oneLineDetected = (length oneLine > 0)
        twoLinesDetected = (length twoLines > 0)
        threeLinesDetected = (length threeLines > 0)
        growingHeight = isLineMixed game heightLimit
        heightLimit = if (odd.truncate) wellY then (wellY-1) / 2 else wellY / 2
        (_,wellY) = getWellSize game


-- generates script to complete 1-3 lines in order to minimize height
completeAnyLinesToMinimizeHeightStrategy :: Game -> Program Instruction
completeAnyLinesToMinimizeHeightStrategy game = generateNormalFitProgram game fLocX orient
  where (oneLine,twoLines,threeLines,_) = getLinePossibilities game 
        orderedOneLine = sortBy (compare `on` trip3) $ map (\(loc,bitmap,orient) -> (loc,orient,minimum $ map snd $ getBitmapAtLocation bitmap loc)) oneLine
        orderedTwoLines = sortBy (compare `on` trip3) $ map (\(loc,bitmap,orient) -> (loc,orient,minimum $ map snd $ getBitmapAtLocation bitmap loc)) twoLines
        orderedThreeLines = sortBy (compare `on` trip3) $ map (\(loc,bitmap,orient) -> (loc,orient,minimum $ map snd $ getBitmapAtLocation bitmap loc)) threeLines
        orderedLines = orderedThreeLines ++ orderedTwoLines ++ orderedOneLine
        ((fLocX,fLocY),orient,fitness) = head orderedLines

-------------------------------------------------------------------------------------------------------------------------
-- Complete Lines Minding Holes Strategy
-------------------------------------------------------------------------------------------------------------------------        

-- detects when it is possible to capture 1-3 lines
detectCompleteLinesMindingHoles :: Game -> Bool
detectCompleteLinesMindingHoles game = (length filteredLinesFitness) > 0
  where (oneLine,twoLines,threeLines,_) = getLinePossibilities game
        lines = oneLine ++ twoLines ++ threeLines       
        linesFitness = map (\(l,b,o) -> (l,b,o,lineCompletionMindingHolesFitnessCalculation game l b)) lines
        filteredLinesFitness = filter (\(l,b,o,f) -> f <= 0 ) linesFitness

-- generates script to complete 1-3 lines        
completeLinesMindingHolesStrategy :: Game -> Program Instruction
completeLinesMindingHolesStrategy game = generateNormalFitProgram game fLocX orient
  where (oneLine,twoLines,threeLines,_) = getLinePossibilities game
        lines = threeLines ++ twoLines ++ oneLine
        linesFitness = map (\(l,b,o) -> (l,b,o,lineCompletionMindingHolesFitnessCalculation game l b)) lines
        linesSortedByFitness = sortBy (compare `on` quad4) linesFitness
        ((fLocX,_),_,orient,_) = head linesSortedByFitness

-------------------------------------------------------------------------------------------------------------------------
-- Side Fit Strategy
-------------------------------------------------------------------------------------------------------------------------

-- detects when it is possible to do a side fit
detectSideFit :: Game -> Bool
detectSideFit game = foundAdequateFits
  where  (_, sideFits) = getCategorizedFits game
         fitnessFits = map (\(l,t,b,o) -> (l,t,o,sideFitFitnessCalculation l t b emptyLocs)) sideFits
         adequateFitnessLocs = filter (\(l,t,o,f) -> f >= 0.5) $ reverse $ sortBy (compare `on` quad4) fitnessFits
         foundAdequateFits = (length adequateFitnessLocs) > 0
         emptyLocs = getEmptyLocations game
         
sideFitStrategy :: Game -> Program Instruction
sideFitStrategy game = generateSideProgram game tLoc fLoc orient
  where (_, sideFits) = getCategorizedFits game
        fitnessFits = map (\(l,t,b,o) -> (l,t,o,sideFitFitnessCalculation l t b emptyLocs)) sideFits
        (fLoc,tLoc,orient,fitness) = head $ reverse $ sortBy (compare `on` quad4) fitnessFits
        emptyLocs = getEmptyLocations game
        
-------------------------------------------------------------------------------------------------------------------------
-- Minimum Height Lowest Best Fit Strategy
-------------------------------------------------------------------------------------------------------------------------

-- generates default script which attempts to find the lowest best fit
minHeightLowestBestFitStrategy :: Game -> Program Instruction
minHeightLowestBestFitStrategy game = generateNormalFitProgram game destX orient
    where (normalFits, _) = getCategorizedFits game
          fitnessFits = map (\(l,b,o) -> (l,b,o,normalFitFitnessCalculation l b emptyLocs)) normalFits
          bestFits = head $ groupBy (\(_,_,_,f1) (_,_,_,f2) -> f1==f2 ) $  reverse $ sortBy (compare `on` quad4) fitnessFits
          minimaBestFits = map (\(l,b,o,f) -> (l,b,o,f, minimum $ map snd $ getBitmapAtLocation b l)) bestFits          
          lowestBestFits = head $ groupBy (\(_,_,_,_,m1) (_,_,_,_,m2) -> m1==m2) $ sortBy (compare `on` (quin5)) minimaBestFits 
          maximaLowestBestFits = map (\(l,b,o,f,m) -> (l,o,f, maximum $ map snd $ getBitmapAtLocation b l)) lowestBestFits
          minHeightLowestBestFit = head $ sortBy (compare `on` (quad4)) $ maximaLowestBestFits     
          ((destX,destY),orient,fitness,_) = minHeightLowestBestFit
          emptyLocs = getEmptyLocations game 

-------------------------------------------------------------------------------------------------------------------------
-- Equalize Heights Strategy
-------------------------------------------------------------------------------------------------------------------------

detectEqualizeHeights :: Game -> Bool
detectEqualizeHeights game = (length regionsWithMeanHeights > 1)
  where regionsWithMeanHeights = getColumnRegionsAndAverageHeights game 8
        
equalizeHeightsStrategy :: Game -> Program Instruction
equalizeHeightsStrategy game = generateNormalFitProgram game destX orient
  where (normalFits, _) = getCategorizedFits game
        fitnessFits = map (\(l,b,o) -> (l,b,o,normalFitFitnessWithHeightFitnessCalculation l b emptyLocs wellSize)) normalFits
        ((destX,_),_,orient,_) = head $ reverse $ sortBy (compare `on` quad4) fitnessFits
        wellSize = getWellSize game
        emptyLocs = getEmptyLocations game

-------------------------------------------------------------------------------------------------------------------------
-- Fitness Calculations
-------------------------------------------------------------------------------------------------------------------------

lineCompletionMindingHolesFitnessCalculation game fit bitmap = fitness
  where locations = getOccupiedLocations game
        holesBefore = getNumberOfHolesInWell game
        positionedBitmap = getBitmapAtLocation bitmap fit
        newWellLocations = positionedBitmap ++ locations
        (wellX,wellY) = getWellSize game
        rowLocs = groupBy (\(_,y1) (_,y2) -> y1 == y2) $ sortBy (compare `on` snd) newWellLocations
        fullRowLocs = concat $ filter (\row -> (fromInteger.toInteger.length) row == wellX) rowLocs
        allLocations = [(x,y) | x <- [1..wellX-2], y <- [1..wellY-3]]
        holesAfter = length [(x,y) | (x,y) <- allLocations, (x,y) `notElem` newWellLocations && or ([(ux,uy) `elem` newWellLocations && (ux,uy) `notElem` fullRowLocs| (ux,uy) <- zip (repeat x) [y+1..wellY-3]])]
        fitness = holesAfter-holesBefore
        
sideFitFitnessCalculation fit temp bitmap emptyLocs = fitness
  where bitmapMinX = minimum (map fst bitmap)
        bitmapMaxX = maximum (map fst bitmap)
        surroundBitmap = (nub $ concat $ map (\(x,y) -> [(x+1,y),(x+1,y+1),(x,y+1),(x-1,y+1),(x-1,y),(x-1,y-1),(x,y-1),(x+1,y-1)]) bitmap) \\ bitmap
        partSurroundBitmap = if (fst fit > fst temp) 
                              then [(x,y) | (x,y) <- surroundBitmap, x >= bitmapMinX]
                              else [(x,y) | (x,y) <- surroundBitmap, x <= bitmapMinX]
        locsSurroundingBrick = map (\(x,y) -> (x + fst fit,y + snd fit)) partSurroundBitmap      
        locsFoundSurroundingBrick = [loc | loc <- locsSurroundingBrick, loc `notElem` emptyLocs]
        bestLocsFound = (fromInteger.toInteger.length) locsSurroundingBrick
        actualLocsFound = (fromInteger.toInteger.length) locsFoundSurroundingBrick
        fitness = (actualLocsFound / bestLocsFound)
        
normalFitFitnessCalculation fit bitmap emptyLocs = fitness
  where bitmapMinX = minimum (map fst bitmap) 
        bitmapMaxX = maximum (map fst bitmap)
        bitmapMaxY = maximum (map snd bitmap)
        surroundBitmap = (nub $ concat $ map (\(x,y) -> [(x+1,y),(x+1,y+1),(x,y+1),(x-1,y+1),(x-1,y),(x-1,y-1),(x,y-1),(x+1,y-1)]) bitmap) \\ bitmap
        underSurroundBitmap = [(x,y) | (x,y) <- surroundBitmap, bitmapMinX <= x, x <= bitmapMaxX, y < bitmapMaxY]       
        locsUnderBrick = map (\(x,y) -> (x + fst fit,y + snd fit)) underSurroundBitmap      
        locsFoundUnderBrick = [loc | loc <- locsUnderBrick, loc `notElem` emptyLocs]
        bestLocsFound = (fromInteger.toInteger.length) underSurroundBitmap
        actualLocsFound = (fromInteger.toInteger.length) locsFoundUnderBrick
        fitness = (actualLocsFound / bestLocsFound)
        
normalFitFitnessWithHeightFitnessCalculation fit bitmap emptyLocs (wellX, wellY) = fitness
  where bitmapMinX = minimum (map fst bitmap) 
        bitmapMaxX = maximum (map fst bitmap)
        bitmapMaxY = maximum (map snd bitmap)
        surroundBitmap = (nub $ concat $ map (\(x,y) -> [(x+1,y),(x+1,y+1),(x,y+1),(x-1,y+1),(x-1,y),(x-1,y-1),(x,y-1),(x+1,y-1)]) bitmap) \\ bitmap
        underSurroundBitmap = [(x,y) | (x,y) <- surroundBitmap, bitmapMinX <= x, x <= bitmapMaxX, y < bitmapMaxY]       
        locsUnderBrick = map (\(x,y) -> (x + fst fit,y + snd fit)) underSurroundBitmap      
        locsFoundUnderBrick = [loc | loc <- locsUnderBrick, loc `notElem` emptyLocs]
        bestLocsFound = (fromInteger.toInteger.length) underSurroundBitmap
        actualLocsFound = (fromInteger.toInteger.length) locsFoundUnderBrick
        bitmapAtLoc = getBitmapAtLocation bitmap fit
        fitBottom = minimum $ map snd bitmapAtLoc
        fitness = (actualLocsFound / bestLocsFound) * (wellY - fitBottom)/wellY
        
-------------------------------------------------------------------------------------------------------------------------
-- DSEL Program Generators
-------------------------------------------------------------------------------------------------------------------------

-- compose program 
composeProgram :: [Program Instruction] -> Program Instruction
composeProgram xs = foldl1 (:>) xs

forThisPieceProgram :: Program Instruction -> Program Instruction
forThisPieceProgram prog = forThisPiece prog

-- generate program to go from an initial X to a destination X
shiftNHardDropProgram :: Double -> Double -> Program Instruction
shiftNHardDropProgram initX destX = composeProgram $ [shiftNProgram initX destX] ++ [perform hardDrop]
        
shiftNProgram :: Double -> Double -> Program Instruction
shiftNProgram initX destX
  | diff > 0  = composeProgram $ replicate step (perform shiftLeft)
  | diff < 0  = composeProgram $ replicate step (perform shiftRight)
  | otherwise = skip
  where diff = round (initX - destX)
        step = abs diff
        
whileCondSoftDropProgram :: (Game -> Bool) -> Program Instruction
whileCondSoftDropProgram f = while f (perform softDrop)

-----------------------------

-- generate program to move a brick in position for the zeroth orientation        
moveNormalProgram :: Game -> Double -> Program Instruction
moveNormalProgram game destX = shiftNHardDropProgram initX destX
  where initX = getBrickXOffset game
 
-- generate program to rotate right and move a brick in position for the first orientation  
rotateRightAndMoveNormalProgram :: Game -> Double -> Program Instruction
rotateRightAndMoveNormalProgram game destX
  | isBrickOShaped game = shiftNHardDropProgram initX destX
  | otherwise           = let gotoPart = shiftNHardDropProgram (initX+1) destX 
                              rotateRightPart = perform rotateRight
                          in composeProgram [rotateRightPart, gotoPart]
  where initX = getBrickXOffset game
 
-- generate program to rotate right twice and move a brick in position for the second orientation  
rotateRightTwiceAndMoveNormalProgram :: Game -> Double -> Program Instruction
rotateRightTwiceAndMoveNormalProgram game destX
  | isBrickOShaped game = shiftNHardDropProgram initX destX
  | otherwise           = let gotoPart = shiftNHardDropProgram initX destX
                              rotateRightTwicePart = perform rotateRight :> perform rotateRight 
                          in composeProgram [rotateRightTwicePart, gotoPart]
  where initX = getBrickXOffset game

-- generate program to rotate left and move a brick in position for the third orientation         
rotateLeftAndMoveNormalProgram :: Game -> Double -> Program Instruction
rotateLeftAndMoveNormalProgram game destX
  | isBrickOShaped game = shiftNHardDropProgram initX destX
  | isBrickIShaped game = let gotoPart = shiftNHardDropProgram (initX+2) destX 
                          in composeProgram [rotateLeftPart, gotoPart]
  | otherwise           = let gotoPart = shiftNHardDropProgram  initX    destX 
                          in composeProgram [rotateLeftPart, gotoPart]
  where rotateLeftPart = perform rotateLeft
        initX = getBrickXOffset game

-----------------------------
        
-- generate program to move a brick in a temporary position for the zeroth orientation then move into the final position      
moveSideProgram :: Game -> (Double,Double) -> (Double,Double) -> Program Instruction
moveSideProgram game (tempX,tempY) (finalX, finalY) = moveSide
  where initX = getBrickXOffset game
        shiftSidePart = shiftNProgram initX tempX
        shiftDownPart = whileCondSoftDropProgram (\gameIn -> getBrickYOffset gameIn > finalY)
        shiftFinalPart = shiftNHardDropProgram tempX finalX
        moveSide = composeProgram [shiftSidePart,shiftDownPart,shiftFinalPart]

-- generate program to rotate right and move a brick in a temporary position for the first orientation then move into the final position  
rotateRightAndMoveSideProgram :: Game -> (Double,Double) -> (Double,Double) -> Program Instruction
rotateRightAndMoveSideProgram game (tempX,tempY) (finalX, finalY)
  | isBrickOShaped game = moveSideProgram game (tempX,tempY) (finalX, finalY)
  | otherwise           = let shiftSidePart = shiftNProgram (initX+1) tempX 
                              shiftDownPart = whileCondSoftDropProgram (\gameIn -> getBrickYOffset gameIn > finalY)
                              shiftFinalPart = shiftNHardDropProgram tempX finalX
                          in composeProgram [rotateRightPart, shiftSidePart, shiftDownPart, shiftFinalPart]
  where rotateRightPart = perform rotateRight
        initX = getBrickXOffset game
 
-- generate program to rotate right twice and move a brick in a temporary position for the second orientation then move into the final position 
rotateRightTwiceAndMoveSideProgram :: Game -> (Double,Double) -> (Double,Double) -> Program Instruction
rotateRightTwiceAndMoveSideProgram game (tempX,tempY) (finalX, finalY)
  | isBrickOShaped game = moveSideProgram game (tempX,tempY) (finalX, finalY)
  | otherwise           = let rotateRightTwicePart = perform rotateRight :> perform rotateRight
                              shiftSidePart = shiftNProgram initX tempX
                              shiftDownPart = whileCondSoftDropProgram (\gameIn -> getBrickYOffset gameIn > finalY)
                              shiftFinalPart = shiftNHardDropProgram tempX finalX
                          in composeProgram [rotateRightTwicePart,shiftSidePart,shiftDownPart,shiftFinalPart]
  where initX = getBrickXOffset game

-- generate program to rotate left and move a brick in a temporary position for the third orientation then move into the final position         
rotateLeftAndMoveSideProgram :: Game -> (Double,Double) -> (Double,Double) -> Program Instruction
rotateLeftAndMoveSideProgram game (tempX,tempY) (finalX, finalY)
  | isBrickOShaped game = moveSideProgram game (tempX,tempY) (finalX, finalY)
  | isBrickIShaped game = let shiftSidePart = shiftNProgram (initX+2) tempX 
                              shiftDownPart = whileCondSoftDropProgram (\gameIn -> getBrickYOffset gameIn > finalY)
                              shiftFinalPart = shiftNHardDropProgram tempX finalX
                          in composeProgram [rotateLeftPart, shiftSidePart, shiftDownPart, shiftFinalPart]
  | otherwise           = let shiftSidePart = shiftNProgram initX tempX 
                              shiftDownPart = whileCondSoftDropProgram (\gameIn -> getBrickYOffset gameIn > finalY)
                              shiftFinalPart = shiftNHardDropProgram tempX finalX
                          in composeProgram [rotateLeftPart, shiftSidePart, shiftDownPart, shiftFinalPart]
  where rotateLeftPart = perform rotateLeft
        initX = getBrickXOffset game

-----------------------------
        
generateNormalProgram :: Game -> Double -> Double -> Program Instruction
generateNormalProgram game destX orient = forThisPieceProgram $ 
  case (orient) of
    0         -> moveNormalProgram game destX
    1         -> rotateRightAndMoveNormalProgram game destX
    2         -> rotateRightTwiceAndMoveNormalProgram game destX
    otherwise -> rotateLeftAndMoveNormalProgram game destX
      
generateSideProgram :: Game -> (Double,Double) -> (Double,Double) -> Double -> Program Instruction
generateSideProgram game tempX destX orient = forThisPieceProgram $ 
  case (orient) of
    0         -> moveSideProgram game tempX destX
    1         -> rotateRightAndMoveSideProgram game tempX destX
    2         -> rotateRightTwiceAndMoveSideProgram game tempX destX
    otherwise -> rotateLeftAndMoveSideProgram game tempX destX

-----------------------------

moveNormalAndFitProgram :: Game -> Double -> Double -> Program Instruction
moveNormalAndFitProgram game destX orient = composeProgram finalProgram
  where finalProgram = [fitPart,movePart,hardDropPart]
        hardDropPart = perform hardDrop
        fitPart = fit (truncate orient)
        movePart = case (orient) of
                     0         -> shiftNHardDropProgram initX destX
                     1         -> if (isBrickOShaped game) then shiftNProgram initX destX else shiftNProgram (initX+1) destX 
                     2         -> shiftNHardDropProgram initX destX
                     otherwise -> if (isBrickIShaped game) then shiftNProgram (initX+2) destX else shiftNProgram initX destX 
        initX = getBrickXOffset game
    
generateNormalFitProgram :: Game -> Double -> Double -> Program Instruction
generateNormalFitProgram game destX orient = forThisPieceProgram $ moveNormalAndFitProgram game destX orient