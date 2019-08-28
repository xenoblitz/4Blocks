
module Core.SimplePoint ( SimplePoint,
                          getSimplePointX,
                          getSimplePointY,
						              moveSimplePointDown,
                          moveSimplePointUp,
                          moveSimplePointLeft,
                          moveSimplePointRight )

where

type SimplePoint = (Double,Double)

getSimplePointX :: SimplePoint -> Double
getSimplePointX (x,_) = x

getSimplePointY :: SimplePoint -> Double
getSimplePointY (_,y) = y

moveSimplePointDown :: SimplePoint -> SimplePoint
moveSimplePointDown (x,y) = (x,y-1)

moveSimplePointUp :: SimplePoint -> SimplePoint
moveSimplePointUp (x,y) = (x,y+1)

moveSimplePointLeft :: SimplePoint -> SimplePoint
moveSimplePointLeft (x,y) = (x-1,y)

moveSimplePointRight :: SimplePoint -> SimplePoint
moveSimplePointRight (x,y) = (x+1,y)
