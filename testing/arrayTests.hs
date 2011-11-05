import Data.Array

someSize = let foo = array ((0,0),(2,2)) [((i,j), 3) | i <- [0..2], j<- [0..2]]
               bar = array ((0,0),(2,2)) [((i,j), 2) | i <- [0..2], j<- [0..2]]
           in let bf = bounds foo 
                  bb = bounds bar
              in if bf == bb then sameSizeIndexMultiply foo bar else foo
              
sameSizeIndexMultiply arr1 arr2 = let ((minx,miny),(maxx,maxy)) = bounds arr1
                                  in array ((minx,miny),(maxx,maxy)) 
                                       [((i,j),(arr1 ! (i,j)) * (arr2 ! (i,j))) | i<-[minx..maxx], j<-[miny..maxy]]
                                     
diffSizeOdd arr1 arr2 = let ((minx1, miny1),(maxx1,maxy1)) = bounds arr1
                            ((minx2, miny2),(maxx2,maxx2)) = bounds arr2
                        in (cx1,cx2) = (((maxx1-minx1) `div` 2), m
                         