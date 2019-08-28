
module Core.ColouredPoint ( ColouredPoint,
                            getColouredPointX,
                            getColouredPointY,
                            getColouredPointColour,
                            convertColouredToSimplePoints,
                            moveColouredPointDown,
                            moveColouredPointUp,
                            moveColouredPointLeft,
                            moveColouredPointRight,
                            removeColouredPoints,
                            filterColouredPoints,
                            extractColouredPoints,
                            getLinesOfColouredPoints,
                            getSortedLinesOfColouredPoints,
                            getColumnsOfColouredPoints,
                            getSortedColumnsOfColouredPoints,
                            getColouredPointsFromLines,
                            convertAllColouredPointsToColour ) 

where

import Core.Colour
import Core.SimplePoint

import Data.Function
import Data.List

type ColouredPoint = (Double,Double,Colour)
  
getColouredPointX :: ColouredPoint -> Double
getColouredPointX (x,_,_) = x

getColouredPointY :: ColouredPoint -> Double
getColouredPointY (_,y,_) = y

getColouredPointColour :: ColouredPoint -> Colour
getColouredPointColour (_,_,c) = c 

convertColouredToSimplePoint :: ColouredPoint -> SimplePoint
convertColouredToSimplePoint (x,y,c) = (x,y)

convertColouredToSimplePoints :: [ColouredPoint] -> [SimplePoint]
convertColouredToSimplePoints xs = map (convertColouredToSimplePoint) xs

moveColouredPointDown :: ColouredPoint -> ColouredPoint
moveColouredPointDown (x,y,c) = (x,y-1,c)

moveColouredPointUp :: ColouredPoint -> ColouredPoint
moveColouredPointUp (x,y,c) = (x,y+1,c)

moveColouredPointLeft :: ColouredPoint -> ColouredPoint
moveColouredPointLeft (x,y,c) = (x-1,y,c)

moveColouredPointRight :: ColouredPoint -> ColouredPoint
moveColouredPointRight (x,y,c) = (x+1,y,c)

removeColouredPoints :: Colour -> [ColouredPoint] -> [ColouredPoint]
removeColouredPoints colour = filter ((/= colour).getColouredPointColour)

filterColouredPoints :: Colour -> [ColouredPoint] -> [ColouredPoint]
filterColouredPoints colour = filter ((== colour).getColouredPointColour)

extractColouredPoints :: Colour -> [ColouredPoint] -> ([ColouredPoint],[ColouredPoint])
extractColouredPoints colour colouredPoints = (filterColouredPoints colour colouredPoints,removeColouredPoints colour colouredPoints)

getLinesOfColouredPoints :: [ColouredPoint] -> [[ColouredPoint]]
getLinesOfColouredPoints colouredPoints = linesOfColouredPoints
  where 
    sortedByYColouredPoints = sortBy (compare `on` getColouredPointY) colouredPoints
    groupedColouredPoints = groupBy (\(x1,y1,c1) (x2,y2,c2) -> (y1==y2)) sortedByYColouredPoints
    reversedGroupedColouredPoints = reverse groupedColouredPoints
    linesOfColouredPoints = reversedGroupedColouredPoints

getSortedLinesOfColouredPoints :: [ColouredPoint] -> [[ColouredPoint]]
getSortedLinesOfColouredPoints colouredPoints = linesOfColouredPoints
  where 
    sortedByYColouredPoints = sortBy (compare `on` getColouredPointY) colouredPoints
    groupedColouredPoints = groupBy (\(x1,y1,c1) (x2,y2,c2) -> (y1==y2)) sortedByYColouredPoints
    sortedByXYColouredPointsLines = map (sortBy (compare `on` getColouredPointX)) groupedColouredPoints
    reversedGroupedColouredPoints = reverse sortedByXYColouredPointsLines
    linesOfColouredPoints = reversedGroupedColouredPoints
    
getColumnsOfColouredPoints :: [ColouredPoint] -> [[ColouredPoint]]
getColumnsOfColouredPoints colouredPoints = columnsOfColouredPoints
  where
    sortedByXColouredPoints = sortBy (compare `on` getColouredPointX) colouredPoints
    groupedColouredPoints = groupBy (\(x1,y1,c1) (x2,y2,c2) -> (x1==x2)) sortedByXColouredPoints    
    columnsOfColouredPoints = groupedColouredPoints
    
getSortedColumnsOfColouredPoints :: [ColouredPoint] -> [[ColouredPoint]]
getSortedColumnsOfColouredPoints colouredPoints = columnsOfColouredPoints
  where
    sortedByXColouredPoints = sortBy (compare `on` getColouredPointX) colouredPoints
    groupedColouredPoints = groupBy (\(x1,y1,c1) (x2,y2,c2) -> (x1==x2)) sortedByXColouredPoints
    sortedByXYColouredPointsLines = map (sortBy (compare `on` getColouredPointY)) groupedColouredPoints
    columnsOfColouredPoints = sortedByXYColouredPointsLines
    
getColouredPointsFromLines :: [[ColouredPoint]] -> [ColouredPoint]
getColouredPointsFromLines = concat

convertAllColouredPointsToColour :: Colour -> [ColouredPoint] -> [ColouredPoint]
convertAllColouredPointsToColour c colouredPoints = unColouredPoints ++ filteredColouredPoints
  where (filteredColouredPoints,removedColouredPoints) = extractColouredPoints Grey_Colour colouredPoints 
        unColouredPoints = map (convertAllColouredPointToColour c) removedColouredPoints

convertAllColouredPointToColour :: Colour -> ColouredPoint -> ColouredPoint
convertAllColouredPointToColour c (x,y,_) = (x,y,c)
