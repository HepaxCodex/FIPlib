-- ! This Module tests the Array features in Haskell

module ArrayTest where

import Data.Array.IO
import Data.Array
import qualified Data.ByteString


{-- The Follwoing Functions are used to test MArrays within the IO monad.  It would
    seem that it is impossible to use MultiDimensional MArrays (As far as I can tell anyway)
--} 

updateTest = do arr <- newArray (1,10) 37 :: IO (IOArray Int Int)
                a <- readArray arr 1
                writeArray arr 1 64
                b <- readArray arr 1 
                print (a,b)
                
createAndShowArray = do arr <- newArray (1,10) 37 :: IO (IOArray Int Int)
                        writeArray arr 1 64
                        showArray arr 10 1
                 

-- showArray :: t -> t1 -> t2 -> IO ()
showArray myArray xMax xItr = do value <- readArray myArray xItr
                                 print value
                                 if xMax == xItr
                                    then print ()
                                    else showArray myArray xMax (xItr+1)
                                         

-- y is rows, x is columns
--showArray2 :: t -> t2 -> t2 -> t2 -> t2 -> t2 -> IO b0
-- showArray2 :: (MArray a a1 IO, Ix a2, Show a1, Num a2) => a a2 a1 -> a2 -> a2 -> a2 -> a2 -> IO ()
--showArray2 :: a a2 a1 -> a2 -> a2 -> a2 -> a2 -> IO ()
showArray2 myArray xMax yMax xItr yItr = do value <- readArray myArray xItr
                                            print value
                                            if xItr == xMax -- we are at the end of a row
                                              then if yItr == yMax -- We are at the end of the array
                                                   then print ()
                                                   else  showArray2 myArray xMax yMax 1 (yItr + 1)
                                              else showArray2 myArray xMax yMax (xItr+1) (yItr+1)



-- ! creates a dummy Byte String (containing the byte value for "Hello World") and                                              
-- ! packs them into a Data.ByteString.  This ByteString is then put into a 
-- ! One Dimensional Array
byteStringTo1DArray = 
  let a = Data.ByteString.pack [ 72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100 ]
  in array (1,Data.ByteString.length a ) [(i, Data.ByteString.index a (i-1)) | i <- [1..(Data.ByteString.length a) ]]
     
byteStringTo2DArray =
  let a = Data.ByteString.pack [1,2,3,4,5,6,7,8,9]
  in let maxWidth = 3
     in array ((0,0),(maxWidth-1, maxWidth-1)) 
        [((i,j), 
          Data.ByteString.index a ((i) * maxWidth + j)) | 
         i <- map(\x ->  floor((x / (toRational maxWidth)) - 0.1)) [1..9], 
         j <- map(\x ->  mod (x-1) maxWidth) [1..9]]
        
changeArray arr xIndex yIndex newValue = 
  arr // [((xIndex, yIndex), newValue)]
  
main =
  changeArray (byteStringTo2DArray) 1 2 56
  