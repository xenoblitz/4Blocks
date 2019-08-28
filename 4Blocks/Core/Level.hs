
module Core.Level ( Level,
                    getGravityFromLevel,
                    getGoalFromLevel,
                    getLevelNumFromLevel,
                    initLevel,
                    incLevel,
                    decLevel )

where

data Level = Level {
               gravity :: Int,
               goal :: Integer
             } deriving Show
             
getGravityFromLevel :: Level -> Int
getGravityFromLevel (Level gravity _) = gravity             

getGoalFromLevel :: Level -> Integer
getGoalFromLevel (Level _ goal) = goal

getLevelNumFromLevel :: Level -> Int
getLevelNumFromLevel (Level gravity _)
  = if gravity >= 1 
      then gravity - 1
      else gravity

initLevel :: Int -> Level
initLevel level = Level newGravity newGoal
  where newGravity = if level >= 0 
                       then level+1
                       else level
        newGoal = if (newGravity < 0) 
                    then 0
                    else getGoalFromGravity newGravity
        
incLevel :: Level -> Level
incLevel (Level gravity goal) = Level newGravity newGoal
  where newGravity = incGravity gravity
        newGoal = if (newGravity < 0) 
                    then 0
                    else getGoalFromGravity newGravity
                    
decLevel :: Level -> Level
decLevel (Level gravity goal) = Level newGravity newGoal
  where newGravity = decGravity gravity
        newGoal = if (newGravity < 0) 
                    then 0
                    else getGoalFromGravity newGravity

getGoalFromGravity :: Int -> Integer
getGoalFromGravity n =  toInteger $ foldr1 (+) (map (*5) [1..n])

-- Increment Gravity
incGravity :: Int -> Int
incGravity gravity
  | gravity < -1   = gravity+1
  | gravity == -1  = 1
  | (gravity >= 1) = gravity+1

-- Decrement Gravity
decGravity :: Int -> Int
decGravity gravity
  | gravity > 1     = gravity-1
  | gravity == 1    = -1
  | (gravity <= -1) = gravity-1
