module AI.Interface ( Game,

                      isNewBrick,
                      isGameOver,
                      
                      isBrickOShaped,
                      isBrickLShaped,
                      isBrickJShaped,
                      isBrickTShaped,
                      isBrickIShaped,
                      isBrickSShaped,
                      isBrickZShaped,
                      getUnderBitmap,
                      getBitmapAtLocation,
                      getAllPossibleFits,
                      getCategorizedFits,
                      getBrickXOffset,
                      getBrickYOffset,
                      getLinePossibilities,
                      
                      getWellSize,
                      isAtOccupied,
                      isAtEmpty,
                      isRangeOccupied,
                      isRangeMixed,
                      isRangeEmpty,
                      isLineOccupied,
                      isLineMixed,
                      isLineEmpty,
                      isColumnOccupied,
                      isColumnMixed,
                      isColumnEmpty,
                      getEmptyLocations,
                      getOccupiedLocations,
                      getHolesInWell,
                      getNumberOfHolesInWell,
                      getColumnRegionsAndAverageHeights,
                      
                      getGameStatus -- remove when ready!!!                   
                    )

where

import Core.Game
import Core.Brick
import Core.Well
import Core.ColouredPoint
import Core.SimplePoint
import Core.Status
import Core.Shape

import AI.Program
import AI.Instruction

import Data.List
import Data.Function

-------------------------------------------------------------------------------------------------------------------------
-- Used by Effector
-------------------------------------------------------------------------------------------------------------------------
isNewBrick :: Game -> Bool
isNewBrick game = isNew
  where (_,offy) = (getOffsetFromBrick.getBrickFromGame) game
        (_,welly) = (getSizeFromWell.getWellFromGame) game
        isNew = (offy >= (welly-4))
        
isGameOver :: Game -> Bool
isGameOver game = (status == GameOver)
  where status = getStatusFromGame game

-------------------------------------------------------------------------------------------------------------------------
-- Used by Thinker
-------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------
---- Information about the game state's brick
--------------------------------------------------------------------------------------

-- is brick O-Shaped
isBrickOShaped :: Game -> Bool
isBrickOShaped game = (getBrickShape game == O_Shape)

-- is brick L-Shaped
isBrickLShaped :: Game -> Bool
isBrickLShaped game = (getBrickShape game == L_Shape)

-- is brick J-Shaped
isBrickJShaped :: Game -> Bool
isBrickJShaped game = (getBrickShape game == J_Shape)

-- is brick T-Shaped
isBrickTShaped :: Game -> Bool
isBrickTShaped game = (getBrickShape game == T_Shape)

-- is brick I-Shaped
isBrickIShaped :: Game -> Bool
isBrickIShaped game = (getBrickShape game == I_Shape)

-- is brick S-Shaped
isBrickSShaped :: Game -> Bool
isBrickSShaped game = (getBrickShape game == S_Shape)

-- is brick Z-Shaped
isBrickZShaped :: Game -> Bool
isBrickZShaped game = (getBrickShape game == Z_Shape)                         

-- get current brick's shape
getBrickShape :: Game -> Shape
getBrickShape game = (getShapeFromBrick.getBrickFromGame) game

-----------------------------

-- brick shape bitmaps

bitmapOShapeBrickOrient0 = [(0,0),(1,0),(0,1),(1,1)]
bitmapIShapeBrickOrient0 = [(0,0),(1,0),(2,0),(3,0)]
bitmapIShapeBrickOrient1 = [(0,0),(0,1),(0,2),(0,3)]
bitmapSShapeBrickOrient0 = [(0,0),(1,0),(1,1),(2,1)]
bitmapSShapeBrickOrient1 = [(0,1),(0,0),(1,0),(1,-1)]
bitmapZShapeBrickOrient0 = [(0,0),(1,0),(1,-1),(2,-1)]
bitmapZShapeBrickOrient1 = [(0,0),(0,1),(1,1),(1,2)]
bitmapJShapeBrickOrient0 = [(0,0),(1,0),(2,0),(0,1)]
bitmapJShapeBrickOrient1 = [(0,0),(0,1),(0,2),(1,2)]
bitmapJShapeBrickOrient2 = [(0,0),(1,0),(2,0),(2,-1)]
bitmapJShapeBrickOrient3 = [(0,0),(1,0),(1,1),(1,2)]
bitmapLShapeBrickOrient0 = [(0,0),(1,0),(2,0),(2,1)]
bitmapLShapeBrickOrient1 = [(0,0),(0,1),(0,2),(1,0)]
bitmapLShapeBrickOrient2 = [(0,0),(0,1),(1,1),(2,1)]
bitmapLShapeBrickOrient3 = [(0,0),(1,0),(1,-1),(1,-2)]
bitmapTShapeBrickOrient0 = [(0,0),(1,0),(2,0),(1,1)]
bitmapTShapeBrickOrient1 = [(0,0),(0,1),(0,2),(1,1)]
bitmapTShapeBrickOrient2 = [(0,0),(1,0),(2,0),(1,-1)]
bitmapTShapeBrickOrient3 = [(0,0),(1,0),(1,-1),(1,1)]

-- get current brick's bitmaps
getBrickBitmapOrientationPairs :: Game -> [([SimplePoint],Double)]
getBrickBitmapOrientationPairs game =
  case brickShape of
    O_Shape -> [(bitmapOShapeBrickOrient0,0)]
    I_Shape -> [(bitmapIShapeBrickOrient0,0),(bitmapIShapeBrickOrient1,1)]
    S_Shape -> [(bitmapSShapeBrickOrient0,0),(bitmapSShapeBrickOrient1,1)]
    Z_Shape -> [(bitmapZShapeBrickOrient0,0),(bitmapZShapeBrickOrient1,1)]
    J_Shape -> [(bitmapJShapeBrickOrient0,0),(bitmapJShapeBrickOrient1,1),(bitmapJShapeBrickOrient2,2),(bitmapJShapeBrickOrient3,3)]
    L_Shape -> [(bitmapLShapeBrickOrient0,0),(bitmapLShapeBrickOrient1,1),(bitmapLShapeBrickOrient2,2),(bitmapLShapeBrickOrient3,3)]
    T_Shape -> [(bitmapTShapeBrickOrient0,0),(bitmapTShapeBrickOrient1,1),(bitmapTShapeBrickOrient2,2),(bitmapTShapeBrickOrient3,3)]
  where brickShape = getBrickShape game

-- get a brick shape's bitmap's under-bitmap
getUnderBitmap :: [SimplePoint] -> [SimplePoint]
getUnderBitmap bitmap = underBitmap
  where sortedXBitmap = groupBy (\(x1,_) (x2,_) -> x1 == x2) $ sortBy (compare `on` fst) bitmap
        lastOfColumns = map (head.sortBy (compare `on` snd)) sortedXBitmap
        underBitmap = map (\(x,y) -> (x,y-1)) lastOfColumns

getBitmapAtLocation :: [SimplePoint] -> SimplePoint -> [SimplePoint]
getBitmapAtLocation bitmap (x,y) = map (\(bx,by) ->(bx+x,by+y)) bitmap

-- used to create a filter to get the fits for a brick
locsFilterAnd :: [SimplePoint] -> [SimplePoint] -> (SimplePoint -> [SimplePoint] -> Bool) -> SimplePoint -> Bool
locsFilterAnd [(xa,ya)]    locs filterF (x,y) = (x+xa,y+ya) `filterF` locs
locsFilterAnd ((xa,ya):xs) locs filterF (x,y) = (x+xa,y+ya) `filterF` locs && locsFilterAnd xs locs filterF (x,y)

locsFilterOr :: [SimplePoint] -> [SimplePoint] -> (SimplePoint -> [SimplePoint] -> Bool) -> SimplePoint -> Bool
locsFilterOr [(xa,ya)]    locs filterF (x,y) = (x+xa,y+ya) `filterF` locs
locsFilterOr ((xa,ya):xs) locs filterF (x,y) = (x+xa,y+ya) `filterF` locs || locsFilterOr xs locs filterF (x,y) 

-- filter which acts over a range
locFilterAndRange :: [SimplePoint] -> [SimplePoint] -> (SimplePoint -> [SimplePoint] -> Bool) -> [SimplePoint]  -> Bool
locFilterAndRange xs locs filterF ys = and $ map (locsFilterAnd xs locs filterF) ys
        
-- get all the possible fits of a brick
getAllPossibleFits :: Game -> [(SimplePoint,[SimplePoint],Double)]
getAllPossibleFits game = allFits
  where allFits = [(loc,shapeBitmap,orient) | loc <- emptyLocs, (shapeBitmap,underBitmap,orient) <- bitmapUnderbitmapOrientationTriples,
                         locsFilterAnd shapeBitmap emptyLocs elem loc &&
                         locsFilterOr underBitmap emptyLocs notElem loc]
        emptyLocs = getEmptyLocations game
        bitmapOrientationPairs = getBrickBitmapOrientationPairs game
        bitmapUnderbitmapOrientationTriples = map (\(bitmap,orient) -> (bitmap, getUnderBitmap bitmap, orient)) bitmapOrientationPairs
        
getCategorizedFits :: Game -> ([(SimplePoint,[SimplePoint],Double)],[(SimplePoint,SimplePoint,[SimplePoint],Double)])
getCategorizedFits game = (possibleFits,sideFits)
  where emptyLocs = getEmptyLocations game
        allFits = getAllPossibleFits game
        (sizeX,sizeY) = getWellSize game
        blockedFits = filter (\((locX,locY),shapeBitmap,orient) -> 
                             (not (locFilterAndRange shapeBitmap emptyLocs elem (zip [locX,locX..] [locY+1 .. sizeY-4])))) allFits
        blockedXFits = map (\((blocX, blocY),shapeBitmap,orient) -> 
                                               ((blocX,blocY),shapeBitmap,orient
                                               ,([(tempX,tempY) | (tempX,tempY) <- (zip [1..blocX-1] [blocY,blocY..]), 
                                                  locFilterAndRange shapeBitmap emptyLocs elem (zip [tempX..blocX-1] [blocY,blocY..])]
                                                ,[(tempX,tempY) | (tempX,tempY) <- (zip [blocX+1..sizeX-2] [blocY,blocY..]), 
                                                  locFilterAndRange shapeBitmap emptyLocs elem (zip [blocX+1..tempX] [blocY,blocY..])])
                                               )) blockedFits
        finalBlockedXFits = [(loc,shapeBitmap,orient,right) | (loc,shapeBitmap,orient,(left, right)) <- blockedXFits, (null left) && (not (null right && null left))] ++
                            [(loc,shapeBitmap,orient,left) | (loc,shapeBitmap,orient,(left, right)) <- blockedXFits, (null right) && (not (null right && null left))]
        blockedXYFits = map (\(finalLoc,shapeBitmap,orient,tempLocs) -> 
                               (finalLoc,shapeBitmap,orient,
                                filter (\(tempX,tempY) -> locFilterAndRange shapeBitmap emptyLocs elem (zip [tempX,tempX..] [tempY+1..sizeY-3])) tempLocs
                               )
                            ) finalBlockedXFits                
        remFullyBlockedXYFits = filter (\(finalLoc,shapeBitmap,orient,tempLocs) -> not (null tempLocs)) blockedXYFits
        sideFits = map (\((fLocX,fLocY),shapeBitmap,orient,tempLocs) -> if (((fst.head) tempLocs) < fLocX)
                                                       then ((fLocX,fLocY),last tempLocs,shapeBitmap,orient)
                                                       else ((fLocX,fLocY),head tempLocs,shapeBitmap,orient)) remFullyBlockedXYFits
        possibleFits = allFits \\ blockedFits

-----------------------------
        
-- get current brick's true X offset
getBrickXOffset :: Game -> Double
getBrickXOffset game = (minimum.map fst.convertColouredToSimplePoints.getBlocksFromBrick.getBrickFromGame) game

-- get current brick's true Y offset
getBrickYOffset :: Game -> Double
getBrickYOffset game = (minimum.map snd.convertColouredToSimplePoints.getBlocksFromBrick.getBrickFromGame) game

-----------------------------

numberOfLines :: Double -> (SimplePoint,[SimplePoint],Double) -> [SimplePoint] -> Int
numberOfLines rowSize ((locX,locY),bitmap,orient) locs = numLines
  where shapeLocs = map (\(bitX,bitY) -> (bitX + locX, bitY + locY)) bitmap
        allLocs = shapeLocs ++ locs
        rowLocs = groupBy (\(_,y1) (_,y2) -> y1 == y2) $ sortBy (compare `on` snd) allLocs
        fullRows = filter (rowSize ==) (map (fromInteger.toInteger.length) rowLocs)
        numLines = length fullRows

getOShapeBrickLinePossibilities :: Game -> ([(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)])
getOShapeBrickLinePossibilities game = (oneLine, twoLines, [], [])
  where 
    (possFitsOrient, _) = getCategorizedFits game
    (sizeX,_) = getWellSize game
    locs = getOccupiedLocations game
    oneLine = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 2]
    twoLines = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 3]
        
getIShapeBrickLinePossibilities :: Game -> ([(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)])
getIShapeBrickLinePossibilities game = (oneLine, twoLines, threeLines, fourLines)
  where 
    (possFitsOrient, _) = getCategorizedFits game
    (sizeX,_) = getWellSize game
    locs = getOccupiedLocations game
    oneLine = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 2]
    twoLines = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 3]
    threeLines = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 4]
    fourLines = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 5]

getSShapeBrickLinePossibilities :: Game -> ([(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)])
getSShapeBrickLinePossibilities game = (oneLine, twoLines, [], [])
  where 
    (possFitsOrient, _) = getCategorizedFits game
    (sizeX,_) = getWellSize game
    locs = getOccupiedLocations game
    oneLine = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 2]
    twoLines = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 3]

getZShapeBrickLinePossibilities :: Game -> ([(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)])
getZShapeBrickLinePossibilities game = (oneLine, twoLines, [], [])
  where 
    (possFitsOrient, _) = getCategorizedFits game
    (sizeX,_) = getWellSize game
    locs = getOccupiedLocations game
    oneLine = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 2]
    twoLines = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 3]
    
getLShapeBrickLinePossibilities :: Game -> ([(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)])
getLShapeBrickLinePossibilities game = (oneLine, twoLines, threeLines, [])
  where 
    (possFitsOrient, _) = getCategorizedFits game
    (sizeX,_) = getWellSize game
    locs = getOccupiedLocations game
    oneLine = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 2]
    twoLines = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 3]
    threeLines = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 4]  
    
getJShapeBrickLinePossibilities :: Game -> ([(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)])
getJShapeBrickLinePossibilities game = (oneLine, twoLines, threeLines, [])
  where 
    (possFitsOrient, _) = getCategorizedFits game
    (sizeX,_) = getWellSize game
    locs = getOccupiedLocations game   
    oneLine = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 2]
    twoLines = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 3]
    threeLines = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 4]   

getTShapeBrickLinePossibilities :: Game -> ([(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)])
getTShapeBrickLinePossibilities game = (oneLine, twoLines, [], [])
  where 
    (possFitsOrient, _ )= getCategorizedFits game
    (sizeX,_) = getWellSize game
    locs = getOccupiedLocations game
    oneLine = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 2]
    twoLines = [fit | fit <- possFitsOrient, numberOfLines sizeX fit locs == 3] 
    
getLinePossibilities :: Game -> ([(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)],[(SimplePoint,[SimplePoint],Double)])
getLinePossibilities game = 
  case brickShape of
    O_Shape -> getOShapeBrickLinePossibilities game
    I_Shape -> getIShapeBrickLinePossibilities game
    S_Shape -> getSShapeBrickLinePossibilities game
    Z_Shape -> getZShapeBrickLinePossibilities game
    J_Shape -> getJShapeBrickLinePossibilities game
    L_Shape -> getLShapeBrickLinePossibilities game
    T_Shape -> getTShapeBrickLinePossibilities game
  where brickShape = getBrickShape game


--------------------------------------------------------------------------------------
---- Information about the game state's well
--------------------------------------------------------------------------------------

-- get size of well
getWellSize :: Game -> (Double, Double)
getWellSize game = (getSizeFromWell.getWellFromGame) game

-----------------------------

-- checks if a point in the well is occupied
isAtOccupied :: Game -> SimplePoint -> Bool
isAtOccupied game (x,y) = answer
  where blocks = (convertColouredToSimplePoints.getBlocksFromWell.getWellFromGame) game
        answer = (x,y) `elem` blocks
        
-- checks if a point in the well is empty
isAtEmpty :: Game -> SimplePoint -> Bool
isAtEmpty game (x,y) = not $ isAtOccupied game (x,y)

-- checks if a range of points in the well is occupied        
isRangeOccupied :: Game -> SimplePoint -> SimplePoint -> Bool
isRangeOccupied game (x1,y1) (x2,y2) = answer 
  where range = [isAtOccupied game (xa,ya) | xa <- [x1..x2], ya <- [y1..y2]]
        answer = and range
        
-- checks if a range of points in the well is mixed
isRangeMixed :: Game -> SimplePoint -> SimplePoint -> Bool
isRangeMixed game (x1,y1) (x2,y2) = answer
  where range = [isAtOccupied game (xa,ya) | xa <- [x1..x2], ya <- [y1..y2]]
        answer = or range
        
-- checks if a range of points in the well is empty        
isRangeEmpty :: Game -> SimplePoint -> SimplePoint -> Bool
isRangeEmpty game (x1,y1) (x2, y2) = answer
  where emptyRange = [isAtEmpty game (xa,ya) | xa <- [x1..x2], ya <- [y1..y2]]
        answer = and emptyRange
        
-- checks if a line of points in the well is occupied        
isLineOccupied :: Game -> Double -> Bool
isLineOccupied game y = answer 
  where (maxX, _) = (getSizeFromWell.getWellFromGame) game
        answer    = isRangeOccupied game (1,y) (maxX-2,y)
        
-- checks if a line of points in the well is mixed        
isLineMixed :: Game -> Double -> Bool
isLineMixed game y = answer
   where (maxX, _) = (getSizeFromWell.getWellFromGame) game
         answer    = isRangeMixed game (1,y) (maxX-2,y)
  
-- checks if a line of points in the well is empty        
isLineEmpty :: Game -> Double -> Bool
isLineEmpty game y = answer 
  where (maxX, _) = (getSizeFromWell.getWellFromGame) game 
        answer    = isRangeEmpty game (1,y) (maxX-2,y)

-- checks if a column of points in the well is occupied        
isColumnOccupied :: Game -> Double -> Bool
isColumnOccupied game x = answer 
  where (_, maxY) = (getSizeFromWell.getWellFromGame) game
        answer    = isRangeOccupied game (x,1) (x,maxY-1)

-- checks if a column of points in the well is mixed        
isColumnMixed :: Game -> Double -> Bool
isColumnMixed game x = answer 
  where (_, maxY) = (getSizeFromWell.getWellFromGame) game
        answer    = isRangeMixed game (x,1) (x,maxY-1)

-- checks if a column of points in the well is empty        
isColumnEmpty :: Game -> Double -> Bool
isColumnEmpty game x = answer 
  where (_, maxY) = (getSizeFromWell.getWellFromGame) game
        answer    = isRangeEmpty game (x,1) (x,maxY-1)
        
-----------------------------

-- get empty locations in well
getEmptyLocations :: Game -> [SimplePoint]
getEmptyLocations game = emptyLocs
  where occupiedLocs = getOccupiedLocations game
        (maxX, maxY) = getWellSize game
        allLocs = [(xa,ya) | ya <- [0..maxY-1], xa <- [0..maxX-1]]
        emptyLocs = allLocs \\ occupiedLocs
        
-- get occupied locations in well
getOccupiedLocations :: Game -> [SimplePoint]
getOccupiedLocations game = occupiedLocs
  where occupiedLocs = (convertColouredToSimplePoints.getBlocksFromWell.getWellFromGame) game

-----------------------------  

isLocationBlocked :: Game -> SimplePoint -> Bool
isLocationBlocked game (x,y) = emptyLoc && upBlocked
  where locs = getOccupiedLocations game
        emptyLoc = (x,y) `notElem` locs
        (_,wellY) = getWellSize game
        upBlocked = or [(ux,uy) `elem` locs | (ux,uy) <- zip (repeat x) [y+1..wellY-3]]
        
getHolesInWell :: Game -> [SimplePoint]
getHolesInWell game = holes
  where (wellX, wellY) = getWellSize game
        allLocations = [(x,y) | x <- [1..wellX-2], y <- [1..wellY-3]]
        holes = filter (isLocationBlocked game) allLocations
        
getNumberOfHolesInWell :: Game -> Int
getNumberOfHolesInWell = length.getHolesInWell
        
-----------------------------
getColumnRegionsAndAverageHeights :: Game -> Double -> [([SimplePoint],Double)]
getColumnRegionsAndAverageHeights game height = (calculateRegionsAverageHeight.calculateColumnRegionsAverageHeight.groupColumnRegions height [].getColumnHeights) game
        
getColumnHeights :: Game -> [([SimplePoint],Double)]
getColumnHeights game = columnHeightPairs
  where (wellX, _) = getWellSize game
        wellContents = filter (\(x,y) -> (x > 0) && (x < wellX-1)) (getOccupiedLocations game)
        wellColumns = groupBy (\(x1,_) (x2,_) -> x1 == x2) $ sortBy (compare `on` fst) wellContents
        columnHeightPairs = map (\col -> (col,maximum $ map snd col)) wellColumns

groupColumnRegions :: Double -> [([SimplePoint],Double)] -> [([SimplePoint],Double)] -> [[([SimplePoint],Double)]]
groupColumnRegions _ currXs [x1] = [currXs ++ [x1]]
groupColumnRegions height currXs (x1:x2:xs)
  = if (diffHeight <= height)
      then groupColumnRegions height (currXs ++ [x1]) (x2:xs)
      else ((currXs ++ [x1]) : (groupColumnRegions height [] (x2:xs)))
    where diffHeight = abs $ snd x1 - snd x2

calculateColumnRegionsAverageHeight :: [[([SimplePoint],Double)]] -> [([[SimplePoint]],Double)]
calculateColumnRegionsAverageHeight xs = map (\ys -> (map fst ys,(sum $ map snd ys) / (fromInteger.toInteger.length) ys)) xs

calculateRegionsAverageHeight :: [([[SimplePoint]],Double)] -> [([SimplePoint],Double)]
calculateRegionsAverageHeight xs = map (\(ys,z) -> (concat ys, z)) xs
-----------------------------
  
--------------------------------------------------------------------------------------
---- Information about the game state's status
--------------------------------------------------------------------------------------

-- REMOVE when done testing --
getGameStatus :: Game -> String
getGameStatus game = (show.getStatusFromGame) game

-----------------------------


