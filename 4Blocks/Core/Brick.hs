
module Core.Brick ( Brick,
                    createBlocks,
                    createBrick,
                    getBlocksFromBrick,
                    getOffsetFromBrick,
                    getShapeFromBrick,
                    rotateBrickRight,
                    rotateBrickLeft,
                    shiftBrickLeft,
                    shiftBrickRight,
                    shiftBrickDown,
                    shiftBrickUp,
                    gameEndBrick
                 )

where

import Core.SimplePoint
import Core.ColouredPoint
import Core.Direction
import Core.Shape
import Core.Colour

data Brick = Brick {
               blocks    :: [ColouredPoint],
               shape     :: Shape,
               offset    :: SimplePoint,
               direction :: Direction              
             }

--------------------------------------------------------------------------------
-- Create Brick
             
createBrick :: Shape -> SimplePoint -> Brick
createBrick shape offset = Brick (createBlocks shape) shape offset Up_Direction

createBlocks :: Shape -> [ColouredPoint]
createBlocks O_Shape = [(0,1,c),(0,2,c),(1,1,c),(1,2,c)]
  where c = Blue_Colour
createBlocks L_Shape = [(0,1,c),(1,1,c),(2,1,c),(2,2,c)]
  where c = Green_Colour
createBlocks J_Shape = [(0,1,c),(1,1,c),(2,1,c),(0,2,c)]
  where c = Orange_Colour
createBlocks T_Shape = [(0,1,c),(1,1,c),(2,1,c),(1,2,c)]
  where c = Cyan_Colour
createBlocks I_Shape = [(0,1,c),(1,1,c),(2,1,c),(3,1,c)]
  where c = Red_Colour
createBlocks S_Shape = [(0,1,c),(1,1,c),(1,2,c),(2,2,c)]
  where c = Purple_Colour
createBlocks Z_Shape = [(0,2,c),(1,2,c),(1,1,c),(2,1,c)]
  where c = Yellow_Colour

--------------------------------------------------------------------------------
-- Update Translation

shiftBrickLeft :: Brick -> Brick
shiftBrickLeft (Brick blocks shape (x,y) direction) = Brick blocks shape (x-1,y) direction

shiftBrickRight :: Brick -> Brick
shiftBrickRight (Brick blocks shape (x,y) direction) = Brick blocks shape (x+1,y) direction

shiftBrickUp :: Brick -> Brick
shiftBrickUp (Brick blocks shape (x,y) direction) = Brick blocks shape (x,y+1) direction

shiftBrickDown :: Brick -> Brick
shiftBrickDown (Brick blocks shape (x,y) direction) = Brick blocks shape (x,y-1) direction

--------------------------------------------------------------------------------
-- Update Rotation

rotateBrickLeft :: Brick -> Brick
rotateBrickLeft (Brick blocks shape offset direction) = Brick blocks shape offset newDirection
  where 
    newDirection = 
      case direction of
        Up_Direction    -> Right_Direction
        Right_Direction -> Down_Direction
        Down_Direction  -> Left_Direction
        Left_Direction  -> Up_Direction

rotateBrickRight :: Brick -> Brick
rotateBrickRight (Brick blocks shape offset direction) = Brick blocks shape offset newDirection
  where 
    newDirection = 
      case direction of
        Up_Direction    -> Left_Direction
        Left_Direction  -> Down_Direction
        Down_Direction  -> Right_Direction
        Right_Direction -> Up_Direction

    
--------------------------------------------------------------------------------
-- Apply Translation

applyTranslationBlocks :: SimplePoint -> [ColouredPoint] -> [ColouredPoint]
applyTranslationBlocks offset blocks = map (applyTranslationBlock offset) blocks

applyTranslationBlock :: SimplePoint -> ColouredPoint -> ColouredPoint
applyTranslationBlock (offx,offy) (x,y,c) = (x+offx,y+offy,c)

--------------------------------------------------------------------------------
-- Apply Rotation

applyRotationBlocks :: Shape -> Direction -> [ColouredPoint] -> [ColouredPoint]
applyRotationBlocks shape direction blocks = map (applyRotationBlock shape direction) blocks

applyRotationBlock :: Shape -> Direction -> ColouredPoint -> ColouredPoint
applyRotationBlock O_Shape _ block = block
applyRotationBlock I_Shape direction (x,y,c) = (newx,newy,c)
  where
    (newx,newy) = 
      case direction of
        Up_Direction    -> (x,y)
        Left_Direction  -> rotateBy90DegreesCWAboutXY (1.5,1.5) (x,y)
        Down_Direction  -> rotateBy180DegreesCWAboutXY (1.5,1.5) (x,y) 
        Right_Direction -> rotateBy270DegreesCWAboutXY (1.5,1.5) (x,y)  
applyRotationBlock _ direction (x,y,c) = (newx,newy,c)
  where 
    (newx,newy) = 
      case direction of
        Up_Direction    -> (x,y)
        Left_Direction  -> rotateBy90DegreesCWAboutXY (1.0,1.0) (x,y)
        Down_Direction  -> rotateBy180DegreesCWAboutXY (1.0,1.0) (x,y) 
        Right_Direction -> rotateBy270DegreesCWAboutXY (1.0,1.0) (x,y)    


rotateBy90DegreesCWAboutOrigin :: SimplePoint -> SimplePoint
rotateBy90DegreesCWAboutOrigin (x,y) = (y,-x)

rotateBy90DegreesCWAboutXY :: SimplePoint -> SimplePoint -> SimplePoint
rotateBy90DegreesCWAboutXY (centrex, centrey) (x,y) = (rotatedx + centrex, rotatedy + centrey)
  where (rotatedx, rotatedy) = rotateBy90DegreesCWAboutOrigin (x-centrex,y-centrey)
  
rotateBy180DegreesCWAboutXY :: SimplePoint -> SimplePoint -> SimplePoint
rotateBy180DegreesCWAboutXY (centrex, centrey) = rotateBy90DegreesCWAboutXY (centrex, centrey) . rotateBy90DegreesCWAboutXY (centrex, centrey) 

rotateBy270DegreesCWAboutXY :: SimplePoint -> SimplePoint -> SimplePoint
rotateBy270DegreesCWAboutXY (centrex, centrey) = rotateBy90DegreesCWAboutXY (centrex, centrey) . rotateBy180DegreesCWAboutXY (centrex, centrey) 

--------------------------------------------------------------------------------

getBlocksFromBrick :: Brick -> [ColouredPoint]
getBlocksFromBrick (Brick blocks shape offset direction) = translatedRotatedBlocks
  where rotatedBlocks = applyRotationBlocks shape direction blocks
        translatedRotatedBlocks = applyTranslationBlocks offset rotatedBlocks
        
getOffsetFromBrick :: Brick -> SimplePoint
getOffsetFromBrick = offset

getShapeFromBrick :: Brick -> Shape
getShapeFromBrick = shape

--------------------------------------------------------------------------------
        
gameEndBrick :: Brick -> Brick
gameEndBrick (Brick blocks shape offset direction) = Brick (convertAllColouredPointsToColour Dark_Grey_Colour blocks) shape offset direction
