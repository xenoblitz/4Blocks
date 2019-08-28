
module Core.Well ( Well,
                   createWell,
                   generateWell,
                   getBlocksFromWell,
                   getSizeFromWell,
                   clearFullLinesFromWell,
                   gameEndWell
                 )

where

import Core.ColouredPoint
import Core.SimplePoint
import Core.Colour

data Well = Well {
              blocks :: [ColouredPoint],
              size :: SimplePoint
            }
            
--------------------------------------------------------------
createWell :: [ColouredPoint] -> SimplePoint -> Well
createWell blocks size = Well blocks size

generateWell :: SimplePoint -> Well
generateWell (x,y) = Well blocks size
  where 
    size = (x+2,y+3)
    blocks = addBorderBlocks size
         
addBorderBlocks :: SimplePoint -> [ColouredPoint]
addBorderBlocks (x,y) = leftBorderBlocks ++ bottomBorderBlocks ++ rightBorderBlocks
  where 
    leftBorderBlocks   = [(0,j,Grey_Colour) | j <- [1..(y-1)]]
    bottomBorderBlocks = [(i,0,Grey_Colour) | i <- [0..(x-1)]]
    rightBorderBlocks  = [(x-1,j,Grey_Colour) | j <- [1..(y-1)]]
    
-------------------------------------------------------------- 
    
getBlocksFromWell :: Well -> [ColouredPoint]
getBlocksFromWell = blocks

getSizeFromWell :: Well -> SimplePoint
getSizeFromWell = size

--------------------------------------------------------------
    
splitLinesOfBlocksAtFullLine :: Int -> [[ColouredPoint]] -> ([[ColouredPoint]],[ColouredPoint],[[ColouredPoint]])
splitLinesOfBlocksAtFullLine fullLineSize wellLines = (beforeFullLine,fullLine,afterFullLine)
  where 
    (beforeFullLine,fullLineAndRest) = span ((< fullLineSize).length) wellLines
    (fullLine,afterFullLine) = if (fullLineAndRest == []) then ([],[]) else (head fullLineAndRest, tail fullLineAndRest)
    
moveLinesOfBlocksDown :: [ColouredPoint] -> [[ColouredPoint]] -> [[ColouredPoint]]
moveLinesOfBlocksDown fullContentsLine beforeContentsLines
  = if (fullContentsLine /= []) 
      then map (map moveColouredPointDown) beforeContentsLines 
      else beforeContentsLines

clearFullLineFromWell :: Well -> Well
clearFullLineFromWell (Well blocks (sizeX,sizeY)) = Well newBlocks (sizeX,sizeY)
  where 
    -- extract and structure blocks for use
    (wellBorderBlocks,wellContentsBlocks) = extractColouredPoints Grey_Colour blocks
    wellContentsLines = getLinesOfColouredPoints wellContentsBlocks
    -- split lines at full line and move lines before full line downwards
    (beforeContentsLines,fullContentsLine,afterContentsLines) = splitLinesOfBlocksAtFullLine (truncate (sizeX-2)) wellContentsLines      
    updatedBeforeContentsLines = moveLinesOfBlocksDown fullContentsLine beforeContentsLines
    -- convert lines back to blocks and create one list
    updatedBeforeContentsBlocks = getColouredPointsFromLines updatedBeforeContentsLines
    afterContentsBlocks = getColouredPointsFromLines afterContentsLines       
    newBlocks = updatedBeforeContentsBlocks ++ afterContentsBlocks ++ wellBorderBlocks
    
--------------------------------------------------------------

clearFullLinesFromWell :: Well -> Int -> (Well,Int)
clearFullLinesFromWell well currentTimes = (newWell,newTimes)
  where tempWell = clearFullLineFromWell well
        hasClearedLines = (length (blocks well) /= length (blocks tempWell))
        (newWell,newTimes) = if (hasClearedLines)
                               then clearFullLinesFromWell tempWell (currentTimes+1)
                               else (well,currentTimes)
    
--------------------------------------------------------------
gameEndWell :: Well -> Well
gameEndWell (Well blocks size) = Well (convertAllColouredPointsToColour Dark_Grey_Colour blocks) size


    
    
