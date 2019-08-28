
module AI.Util (trip1,
                trip2,
                trip3,
                quad1,
                quad2,
                quad3,
                quad4,
                quin1,
                quin2,
                quin3,
                quin4,
                quin5
               )
                
where

trip1 :: (a,b,c) -> a
trip1 (a,_,_) = a

trip2 :: (a,b,c) -> b
trip2 (_,b,_) = b
                   
trip3 :: (a,b,c) -> c
trip3 (_,_,c) = c

quad1 :: (a,b,c,d) -> a
quad1 (a,_,_,_) = a

quad2 :: (a,b,c,d) -> b
quad2 (_,b,_,_) = b

quad3 :: (a,b,c,d) -> c
quad3 (_,_,c,_) = c

quad4 :: (a,b,c,d) -> d
quad4 (_,_,_,d) =  d

quin1 :: (a,b,c,d,e) -> a
quin1 (a,_,_,_,_) = a

quin2 :: (a,b,c,d,e) -> b
quin2 (_,b,_,_,_) = b

quin3 :: (a,b,c,d,e) -> c
quin3 (_,_,c,_,_) = c

quin4 :: (a,b,c,d,e) -> d
quin4 (_,_,_,d,_) =  d

quin5 :: (a,b,c,d,e) -> e
quin5 (_,_,_,_,e) = e