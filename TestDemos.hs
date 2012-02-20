-- *****************************************************************************
--
-- Filename    : TestDemos.hs
--
-- Description : Contains code to generate the demos in the FIPlib paper
--
-- Author      : Andrew Kordik
--
-- Copyright A.M. Kordik and S. Perugini
--
-- *****************************************************************************



{- LANGUAGE FlexibleInstances -}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{- LANGUAGE BangPatterns -}


import FIPlib.Core
import FIPlib.Filters
import Data.Array.Unboxed
import Data.Array
import Data.Word


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
       Just myImage -> let window = arithmeticMean 5 5
                       in let resultImage = valueMap
                                            (applyWindow window)
                                            myImage
                          in writeImage "smoothingDemo.bmp" resultImage


histogramEqDemo =
  do inputImage <- loadImage "lena.bmp"
     case inputImage of
       Nothing -> putStrLn "Histogram Equalization Failed"
       Just myImage -> let resultImage
                             = valueMap
                               (fullHistogramEq (width myImage) (height myImage))
                               myImage
                       in writeImage "FullHistEq.bmp" resultImage




-- Normalized hist should be adjusted so that minCount is subtracted from the numerator and
-- denominator.  See Wikipedia
fullHistogramEq :: forall e. (Enum e, Ix e, Integral e,Num e, IArray UArray e) => Int -> Int -> UArray (Int,Int) e-> UArray (Int, Int) e
fullHistogramEq width height inputArray =
  let histogramArray =  (hist
                         (0,255)
                         (Data.Array.Unboxed.elems inputArray ))
      summedHist = Data.Array.array
                   (0,255) --(bounds histogramArray)
                   (zip [0..255] $  scanl1 (+) (Data.Array.elems histogramArray))
      minCount = minNotZero (Data.Array.elems summedHist)
      normalizedHist = fmap
                       (\x -> (255 * (fromIntegral (x) ) `div` ((width*height )  )))
                       summedHist
      final = (amap
              (\x -> (floor ( fromIntegral(normalizedHist Data.Array.! x)))     )
              inputArray)

  in final

--hist            :: (Ix a, Integral b) => (a,a) -> [a] -> UArray a b
hist bnds is    =  Data.Array.accumArray (+) 0 bnds [(i, 1) | i <- is, inRange bnds i]

minNotZero [] = 0
minNotZero (x:xs) = let minRest = minNotZero xs
                    in if minRest == 0
                       then x
                       else if x < minRest && x /= 0
                            then x
                            else minRest
{-
thumbnailDemo =
  do inputImage <- loadImage "lena.bmp"
     case inputImage of
       Nothing -> putStrLn "Failed to Load Image"
       Just myImage -> let resultImage = indexMap
                                         (\width ->
                                           (fromIntegral (width)) `div` fromIntegral(2))
                                         (\height ->
                                           (fromIntegral (height)) `div` fromIntegral(2))
                                         (\image ->
                                           thumbnail image)
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
-}
