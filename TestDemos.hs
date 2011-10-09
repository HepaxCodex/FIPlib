-- | This is the primary Module for Functional Image Processing Course
-- | Developed by Andrew Kordik for
-- | the University of Dayton
-- | All Rights Reserved
-- | This library is still highly volitile.
-- | The goal is to provide an interface to 
-- | Discrete Signal Processing with a Functional Approach
-- | by leveraging Haskell and demonstraiting it through
-- | an image processing libarary, which means we are
-- | using two dimentional signal spaces



import FIPlib.Core
import FIPlib.Filters
import Data.Array
import Data.Word
import Criterion.Main


main = 
  defaultMain
    [bench "warmup" $ whnf putStrLn "Hello World",
     bench "smoothingDemo" $ smoothingDemo]
    
filterTest = 
  do inputImage <- loadImage "lena.bmp"
     case inputImage of
       Nothing -> putStrLn "Failed to Load Image"
       Just myImage -> let window = gaussian 3 3 1
                       in let resultImage = valueMap  
                                            (applyWindow window)
                                            myImage
                          in writeImage "smoothingDemo.bmp" resultImage
  


{--
testWindow = 
  let window = array ((-2,-2),(2,2)) [((i,j),(1 / 25)) | i <- [-2..2], j <- [-2..2]] -- Param 
      imageArray = array  ((0,0),(2,2)) [((i,j),9) | i <- [0..2], j <- [0..2]] -- Param
  in applyWindow window imageArray
     
 --}  
  
--sharpeningDemo =
--  do inputImage <- loadImage "lena.bmp"
--     case inputImage of 
--       Nothing -> putStrLn "Failed to Load Image"
--       Just myImage -> let resultImage = unSharpMask 1 myImage
--                       in writeImage "sharpeningDemo.bmp" resultImage
     
     
smoothingDemo = 
  do inputImage <- loadImage "lena.bmp"
     case inputImage of
       Nothing -> putStrLn "Failed to Load Image"
       Just myImage -> let window = array ((-2,-2),(2,2)) [((i,j),(1 / 25)) | i <- [-2..2], j <- [-2..2]] -- Param 
                       in let resultImage = valueMap  
                                            (applyWindow window)
                                            myImage
                          in writeImage "smoothingDemo.bmp" resultImage
     


histogramEqDemo = 
  do inputImage <- loadImage "lena.bmp"
     case inputImage of
       Nothing -> putStrLn "Histogram Equalization Failed"
       Just myImage -> let resultImage = valueMap 
                                        (fullHistogramEq (width myImage) (height myImage))
                                        myImage
                       in writeImage "FullHistEq.bmp" resultImage
                          
                          
-- Normalized hist should be adjusted so that minCount is subtracted from the numerator and
-- denominator.  See Wikipedia
fullHistogramEq ::  (Ix i) =>
     Int -> Int -> Array i Word8 -> Array i Word8                                      
fullHistogramEq width height inputArray = 
  let histogramArray =  (hist 
                         (0,255) 
                         (elems inputArray )
                        )
      summedHist = array 
                   (bounds histogramArray) 
                   (zip [0..255] $ scanl1 (+) (elems histogramArray))
      minCount = minNotZero (elems summedHist)
      normalizedHist = fmap 
                       (\x -> (255 * (fromIntegral (x) ) `div` ((width*height )  )))
                       summedHist

  in fmap 
     (\x -> (floor ( fromIntegral(normalizedHist ! x))) ::Word8     ) 
     inputArray
      -- result = valueMap 
      --         (\array -> fmap (\x -> floor(255 * (summedHist ! x))) array) 
      --         myImage
                           
hist            :: (Ix a, Integral b) => (a,a) -> [a] -> Array a b
hist bnds is    =  accumArray (+) 0 bnds [(i, 1) | i <- is, inRange bnds i]

minNotZero [] = 0
minNotZero (x:xs) = let minRest = minNotZero xs
                    in if minRest == 0 
                       then x
                       else if x < minRest && x /= 0
                            then x
                            else minRest

{--

histTest3 = 
  let foo = array ((0,0),(2,2)) [((i,j), i + j) | i <- [0..2], j <- [0..2]]
  in hist (0,5) (arrayToByteString foo 3 3)


sumHist = 
  let foo = array ((0,0),(2,2)) [((i,j), i + j) | i <- [0..2], j <- [0..2]]
      histogramArray = hist 
                       (0,4) 
                       (arrayToByteString
                        foo
                        3
                        3
                       )
      minCount = minimum (elems histogramArray)
      normalizedHist = fmap
                       (\x -> ((fromIntegral(x)) / ((3*3))))
                       histogramArray
      summedHist = array 
                   (0,4)
                   (zip  [0..4] $ scanl1 (+)  (elems normalizedHist))
  in summedHist
     
     
sumHist2 = 
  let foo = array ((0,0),(2,2)) [((i,j), i + j) | i <- [0..2], j <- [0..2]]
      histogramArray = hist 
                       (0,7) 
                       (arrayToByteString
                        foo
                        3
                        3
                       )
      summedHist = array 
                   (0,7)
                   (zip  [0..7] $ scanl1 (+)  (elems histogramArray))
      minCount = minNotZero (elems summedHist)
      normalizedHist = fmap
                       (\x -> (7 * (fromIntegral (x)  - minCount) `div` ((3*3)-minCount)))
                       summedHist
      
  in fmap
     (\x -> (floor ( fromIntegral(normalizedHist ! x)))  ) 
     foo
--}


{--

histogramEq ::  (Ix i) =>
     Int -> Int -> Array i Word8 -> Array i Word8                                      
histogramEq width height inputArray = 
  let histogramArray =  (hist 
                         (0,255) 
                         (elems inputArray )
                        )
      normalizedHist = fmap 
                       (\x -> (fromIntegral(x) * 255 `div`(width*height)))
                       histogramArray
      summedHist = array 
                   (bounds normalizedHist) 
                   (zip [0..255] $ scanl1 (+) (elems normalizedHist))
  in fmap (\x -> (floor ( fromIntegral(summedHist ! x))) ::Word8     ) inputArray
 --}
     


{--     

main = 
  do inputBMP <- loadBitmap "lena.bmp"
     case inputBMP of
       Nothing -> putStrLn "Failed To Load Image"
       Just bmp -> let bar = bmpToImage bmp
                   in let foo = imageToBmp bar
                      in writeBMP "WORKS.bmp" foo
--}
thumbnailDemo = 
  do inputImage <- loadImage "lena.bmp"
     case inputImage of
       Nothing -> putStrLn "Failed to Load Image"
       Just myImage -> let resultImage = indexMap  
                                         (\width -> (fromIntegral (width)) `div` fromIntegral(2))
                                         (\height -> (fromIntegral (height)) `div` fromIntegral(2))
                                         (\image -> thumbnail image)
                                         myImage
                      in writeImage "ThumbnailDemo.bmp" resultImage
--thumbnail2 :: Array (Int, Int) Word8 -> Array (Int, Int) Word8
thumbnail array =  
  let (w,h) = getWidthAndHeight ( bounds array )
  in ixmap 
     ((0,0), (w-1, h-1)) 
     (\i -> ((fst i)*2, (snd i) * 2)) 
     array
     
-- | Extracts width and height from a bounds representation
getWidthAndHeight :: ((Int,Int), (Int, Int)) -- ^ ((wStart, hStart), (wEnd, hEnd))
                     -> (Int, Int) -- ^ (width, height)
getWidthAndHeight ((wStart, hStart), (wEnd, hEnd)) =
  ((wEnd - wStart), (hEnd - hStart))

        {--                 
valueTest =
  do inputBMP <- loadBitmap "lena.bmp"
     case inputBMP of
       Nothing -> putStrLn "Failed to Load Image"
       Just bmp -> let myImage = bmpToImage bmp
                   in let resultImage = valueMap  
                                         (\image -> fmap ((*) 2) image )
                                         myImage
                      in writeBMP "ThumbnailTest2.bmp" (imageToBmp resultImage)
                         
--}