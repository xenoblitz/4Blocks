
module Core.Shape ( Shape ( O_Shape,
                            L_Shape,
                            J_Shape,
                            T_Shape,
                            I_Shape,
                            S_Shape,
                            Z_Shape
                          )
                  )

where

data Shape
  = O_Shape
  | L_Shape
  | J_Shape
  | T_Shape
  | I_Shape
  | S_Shape
  | Z_Shape
    deriving (Eq,Enum)
    

