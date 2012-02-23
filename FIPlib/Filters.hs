-- *****************************************************************************
--
-- Filename    : FIPlib/Filters.hs
--
-- Description : Contains code for the generation and manipulation of common
--               Image processing filter techniques.
--
-- Author      : Andrew Kordik
--
-- Copyright A.M. Kordik and S. Perugini
--
-- *****************************************************************************




{-# LANGUAGE NoMonomorphismRestriction #-}

-- | This Module is used to generate different Window Filters
-- For use with the applyWindow functions.
--
-- In general these are intended to be Spatial filters in the
-- time domain
--
--
-- TODO ::
-- 1) Filter should check to make sure width and height are odd

module FIPlib.Filters
       --(
       --  gaussian
       --)
       where


import Data.Array.Unboxed
import FIPlib.Core


----------------------
-- Helper Functions --
----------------------

-- | dimToIndex, converts dimensions (width and height) into
-- a touple containing starting and ending indexes for arrays
-- to be used in window filtering
-- SHOULD BE PRIVATE
{--
dimToIndex :: (Integral t, Integral t1) => t -> t1 -> (t,t1)
--}
dimToIndex width height =
  (width `div` 2, height `div` 2)


-- | genFilter takes a widht and height of a desired filter
-- which should be odd (but does not check!!), and a
-- function which takes a widthIndex and a heightIndex
-- and produces a single value that is the filter weight
-- in that position.

{--
genFilter :: (Num e)
             => a -- ^ Width of the filter (MUST BE ODD!)
             -> a -- ^ Height of the filter (MUST BE ODD!)
             -> (a -> a -> e) -- ^ Function which takes a width and height position and returns a value
             -> UArray (Int,Int) e -- ^ Array containing the filter
             --}
genFilter width height fcn =
  let (endWidth, endHeight) = dimToIndex width height
  in (array
     ((0-endWidth, 0-endHeight), (endWidth, endHeight))
     [ ((x,y), (fcn x y)) | x <- [0-endWidth..endWidth], y <- [0-endHeight..endHeight]])


-------------------------------------------
-- Spatial Mean Filters (Gonzalez p.323) --
-------------------------------------------

-- | Generates a filter to get the arithmetic mean of all samples in the
-- neighboorhood of the pixel of interest. This is the sum of all pixel
-- values divided by the area (width * height) of the filter
{--
arithmeticMean :: (Floating e, Integral a, Ix a)
                  => a -- ^ desired width of the filter to be generated
                  -> a -- ^ desired height of the filter to be generated
                  -> Array (a, a) e -- ^ Resulting Arithmetic  Spatial Mean Window Filter
--}
{--arithmeticMean :: ( Num a) => t -> t -> UArray (t,t) a--}
arithmeticMean :: Int -> Int -> UArray (Int, Int) Float
arithmeticMean width height=
  let fcn = (\ x y -> (1.0 / ( fromIntegral(width * height))))
  in genFilter width height fcn


-- Geometric
-- Harmonic
-- Contra Harmonic

-------------------------------
-- Spatial Smoothing Filters --
-------------------------------

-- | Generates a 2-D Gaussian Spatial filter.  Using the equation:
-- g(x,y) = exp( - (x^2 + y^2) / (2 * sigma^2)) where sigma is the
-- desired std-deviation
--gaussian :: (Integral a, Floating b, Floating b1) => a -> a -> b -> Array(a,a) b1
{--
gaussian :: (Floating e, Integral a, Ix a)
     => a -- ^ desired witdth of the filter to be generated
     -> a -- ^ desired height of the filter to be generated
     -> e -- ^ standard deviation of gaussian function to be used
     -> Array (a, a) e -- ^ Resulting Gaussian Spatial Smoothing  Window Filter
--}
--gaussian width height sigma =
--  let gaussFcn = (\sigma x y -> exp (-((fromIntegral x)^2 + (fromIntegral y)^2)/(2*sigma^2)))
--   in genFilter width height (gaussFcn sigma)


gaussian width height sigma =
  let gaussFcn = (\sigma x y -> exp (-(( x)^2 + ( y)^2)/(2*sigma^2)))
   in genFilter width height (gaussFcn sigma)



------------------------
-- Sharpening Filters --
------------------------

--unSharpMask weight inputImage =
--  let filter = gaussian 3 3 1
--      blurredImage = valueMap
--                     (applyWindow filter)
--                     inputImage
--      mask = inputImage - blurredImage
--  in inputImage + weight * mask
