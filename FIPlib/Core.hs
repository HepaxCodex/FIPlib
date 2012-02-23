-- *****************************************************************************
--
-- Filename    : FIPlib/Core.hs
--
-- Description : This is the core code behind the FIPlib library
--               The base Image type and primary operations on Images are
--               defined here.
--
--               This library is still highly volitile.
--
-- Author      : Andrew Kordik
--
-- Copyright A.M. Kordik and S. Perugini
--
-- *****************************************************************************



{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

-- | This is the primary Module for the Funtional Image Processing Libaray
module FIPlib.Core
       (Image,
        indexMult,
        loadImage,
        writeImage,
        valueMap,
        indexMap,
        applyWindow,
        arrayToByteString,
        width,
        height,
        red,
        blue,
        green,
        alpha
        )where

import Codec.BMP
import Data.Array.Unboxed
import qualified Data.ByteString
import Data.Maybe
import System.IO
import GHC.Word

import qualified Data.List as L

-- haddock FIP.hs --ignore-all-exports -h -o do


-- Haskell works on Kinds . . .  a "type"  has kind *
-- because values are types (red, green and blue are values)
-- they must have kind *.  Array is a "type constructor"  not
-- a type itself.  Array has kind * -> * -> * which does not
-- match kind * of a type.
-- You can think of kinds as "types of types"

-- | This is the core type of the library, each component of the
-- image is stored as an Array of Word8s
data Image i e = Image {
  -- |Width in pixels of the image
  width :: Int,
  -- |Height in pixels of the image
  height :: Int,
  -- |The Red pixel Values of the image
  red :: UArray i e,
  -- |The Green pixel values of the image
  green :: UArray i e,
  -- |The Blue pixel values of the image
  blue :: UArray i e,
  -- |The Alpha pixel values of the image
  alpha :: UArray i e
  }


{-# INLINE [1] valueMap #-}
{-# RULES
"valueMap/valueMap" forall win1 win2 image. valueMap (applyWindow win1) (valueMap (applyWindow win2 ) image) = valueMap (applyWindow (indexMult win1 win2 )) image
"valueMap/valueMap2" forall w1 w2 image. valueMap (applyWindow w1) $ valueMap (applyWindow w2) $ image = valueMap (applyWindow (indexMult w1 w2 )) image
"valueMap/valueMap3" forall w1 w2 image. valueMap (applyWindow w1) . valueMap (applyWindow w2) $ image = valueMap (applyWindow (indexMult w1 w2 )) image
  #-}
-- | The Type-Class ValueMappable manipulates Arrays without changing thier dimensions.
-- There is no checking to ensure that this is maintained.
class ValueMappable a where
  valueMap :: (UArray (Int, Int) a -> UArray (Int, Int) b) -- ^ A Function that can manipulate a 2-D Array
              -> Image (Int, Int) a -- ^ The input Image
              -> Image (Int, Int) b -- ^ The output Image



-- | The Type-Class IndexMappable maniplates Arrays and may-or-maynot change their dimenions.
-- There is no checking to ensure that the dimension changes are the same as the changes to the array
class IndexMappable a where
  indexMap :: (Int -> Int) -- ^ A Function that changes the width of the image
              -> (Int -> Int) -- ^ A Function that changes the height of the image
              -> (UArray (Int, Int) a ->
                  UArray (Int, Int) b) -- ^ A Function that manipulates a 2-D Array
              -> Image (Int, Int) a -- ^ The input Image
              -> Image (Int, Int) b -- ^ The output Image

-- | Changes the Values in the components of an Image without changing their dimensions.
-- Each component is changed in the same way
instance ValueMappable a where
  valueMap f myImage =
    let mw = width myImage
        mh = height myImage
        mr = red myImage
        mg = green myImage
        mb = blue myImage
        ma = alpha myImage
    in Image {width  = mw
             ,height = mh
             ,red    = f mr
             ,green  = f mg
             ,blue   = f mb
             ,alpha  = f ma
             }


-- | Changes the Values in the components of an Image and changes the dimensions of the image
-- Each component is changed in the same way
instance IndexMappable a where
  indexMap fw fh farr myImage =
    let mw = width myImage
        mh = height myImage
        mr = red myImage
        mg = green myImage
        mb = blue myImage
        ma = alpha myImage
    in Image {width  = fw mw
             ,height = fh mh
             ,red    = farr mr
             ,green  = farr mg
             ,blue   = farr mb
             ,alpha  = farr ma
             }

{--
-- | This is never used however it should have been
instance Functor (Image (Int, Int) ) where
  fmap f myImage =
    let mw = width myImage
        mh = height myImage
        mr = red myImage
        mg = green myImage
        mb = blue myImage
        ma = alpha myImage
    in Image {width  = mw
             ,height = mh
             ,red    = fmap f mr
             ,green  = fmap f mg
             ,blue   = fmap f mb
             ,alpha  = fmap f ma
             }
--}




-- instance (Num a) => Num (Image (Int, Int) a) where

--class Addable a where
--  (+) ::  Image (Int, Int) a -- ^ The first input Image
--         -> Image (Int, Int) b -- ^ The second input  Image
--         -> Image (Int, Int) c -- ^ The second input  Image

--instance Addable (Image (Int, Int) a) where
--  (+) fstImage sndImage =
--    let mw = width fstImage
--        mh = height fstImage
--        mr = red fstImage
--        mg = green fstImage
--        mb = blue fstImage
--        ma = alpha fstImage
--    in Image {
--      width = mw,
--      height = mh,
--      red = array ((0,0), (mw,mh)) [((i,j), (red fstImage) ! (i,j) + (red sndImage) ! (i,j)) | i <- [0..mw], j<- [0..mh]],
--      green = array ((0,0), (mw,mh)) [((i,j), (green fstImage) ! (i,j) + (green sndImage) ! (i,j)) | i <- [0..mw], j<- [0..mh]],
--      blue = array ((0,0), (mw,mh)) [((i,j), (blue fstImage) ! (i,j) + (blue sndImage) ! (i,j)) | i <- [0..mw], j<- [0..mh]],
--      alpha = array ((0,0), (mw,mh)) [((i,j), (alpha fstImage) ! (i,j) + (alpha sndImage) ! (i,j)) | i <- [0..mw], j<- [0..mh]]
--      }

--class Subtractable a where
--  (-) :: Image (Int, Int) a -- ^ The first input Image
--         -> Image (Int, Int) b -- ^ The second input  Image
--         -> Image (Int, Int) c -- ^ The second input  Image


--instance Subtractable (Image (Int, Int) a) where
--  (-) fstImage sndImage =
--    let mw = width fstImage
--        mh = height fstImage
--        mr = red fstImage
--        mg = green fstImage
--        mb = blue fstImage
--        ma = alpha fstImage
--    in Image {
--      width = mw,
--      height = mh,
--      red = array ((0,0), (mw,mh)) [((i,j), (red fstImage) ! (i,j) - (red sndImage) ! (i,j)) | i <- [0..mw], j<- [0..mh]],
--      green = array ((0,0), (mw,mh)) [((i,j), (green fstImage) ! (i,j) - (green sndImage) ! (i,j)) | i <- [0..mw], j<- [0..mh]],
--      blue = array ((0,0), (mw,mh)) [((i,j), (blue fstImage) ! (i,j) - (blue sndImage) ! (i,j)) | i <- [0..mw], j<- [0..mh]],
--      alpha = array ((0,0), (mw,mh)) [((i,j), (alpha fstImage) ! (i,j) - (alpha sndImage) ! (i,j)) | i <- [0..mw], j<- [0..mh]]
--      }




-- (-)
  -- (*)
  -- abs
  -- signum
  -- fromInteger

--}
indexMult :: UArray (Int, Int) Float -> UArray (Int, Int) Float -> UArray (Int, Int) Float
indexMult arr1 arr2 = let ((minx,miny),(maxx,maxy)) = bounds arr1
                      in array ((minx,miny),(maxx,maxy))
                           [((i,j),(arr1 ! (i,j)) * (arr2 ! (i,j))) | i<-[minx..maxx], j<-[miny..maxy]]



-- | applyWindow takes a 2-D array that contains the filter values.  This
--   is synonmous with a Window filter common in Image Processing Algorithms.
--   The Window Filter is applied as a sum of products on an element my element
--   basis.  First some basic information about the Array being processed (imageArray)
--   and the window being applied (window) are gathered (i.e. width and height).
--   Next the imageArray is padded with zeros.  This is common but not required by
--   Windowing techniques.  The padding is accomplished by extending the image Array
--   in all directions, such that the values are at the same index as the original,
--   In other words, that is the padding data actually exists at indicies below zero
{-# INLINE [1] applyWindow #-}
{-# RULES
    "applyWindow/applyWindow" forall win1 win2 image. applyWindow win1
                                                                  (applyWindow win2 image) =
                                                                  applyWindow (indexMult win1 win2) image
  #-}
{--
applyWindow :: (RealFrac a, Integral a1, Integral e) =>
     Array (Int, Int) a -- ^
     -> Array (Int, Int) a1 -- ^
     -> Array (Int, Int) e -- ^
--}
--applyWindow :: (Num e) => UArray (Int,Int) e -> UArray (Int, Int) e -> UArray (Int, Int) e


--applyWindow :: forall e. (Integral e, Num e, IArray UArray e)
--            => UArray (Int,Int) Float -> UArray (Int,Int) Word8 -> UArray (Int,Int) Word8
applyWindow :: UArray (Int,Int) Float -> UArray (Int,Int) Word8 -> UArray (Int,Int) Word8
applyWindow  window imageArray =
  let ((windowWidthMin, windowHeightMin), (windowWidthMax, windowHeightMax)) = Data.Array.Unboxed.bounds (window) -- keep
      windowWidth = windowWidthMax - windowWidthMin  -- Keep Calculated
      windowHeight = windowHeightMax - windowHeightMin -- Keep Cakculated
      w = (floor (fromIntegral(windowWidth) / 2)) :: Int -- Keep
      h = (floor (fromIntegral(windowHeight) / 2)) :: Int -- Keep
      ((imageWidthMin, imageHeightMin), (imageWidthMax, imageHeightMax)) = Data.Array.Unboxed.bounds imageArray -- Keep
      paddedImage :: UArray (Int, Int) Word8 =
                    Data.Array.Unboxed.array
                        ((imageWidthMin-w,imageHeightMin-h),(imageWidthMax+w,imageHeightMax+h))
                        [ ((i,j), if i >= imageWidthMin &&
                                j >= imageHeightMin &&
                                i <= imageWidthMax &&
                                j <= imageHeightMax
                                  then (imageArray) Data.Array.Unboxed.! (i,j)
                                  else 0
                          ) |
                          i <- [imageWidthMin-w..imageWidthMax+w],
                          j <- [imageHeightMin-h..imageHeightMax+h]]
      filteredPaddedImage :: UArray (Int, Int) Word8 = {-# SCC "filter" #-}
        (Data.Array.Unboxed.array
        ((imageWidthMin-w,imageHeightMin-h),(imageWidthMax+w,imageHeightMax+h))
        [((i,j), if i >= imageWidthMin &&
                    j >= imageHeightMin &&
                    i <= imageWidthMax &&
                    j <= imageHeightMax
                 then (L.foldl' (+) 0 [(myMult (paddedImage Data.Array.Unboxed.!(i+m,j+n)) (window Data.Array.Unboxed.!(m,n)))|
                                             m <- [windowWidthMin..windowWidthMax],
                                             n <- [windowHeightMin..windowHeightMax] ])
                 else 0
         ) |
         i <- [imageWidthMin-w..imageWidthMax+w],
         j <- [imageHeightMin-h..imageHeightMax+h]])
     in (filteredPaddedImage)

myMult :: Word8 -> Float -> Word8
myMult !x !y = x `seq` y `seq` floor(fromIntegral(x) * y)

-- | loadImage takes a filename including the extension of a 32bit
--   Bitmap image and returns an Image wrapped in a Maybe and IO monad
--   If the load fails, An Error is Printed and the function returns IO (Nothing)
--   Otherwise it returns IO( Just Image (Int,Int) Word8))
{--
loadImage :: String -- ^ Input filename of a Bitmap Image with extension
             -> IO ( Maybe (Image (Int, Int) Word8)) -- ^ Loaded Image
--}
loadImage filename =
  do inputBMP <- loadBitmap filename
     case inputBMP of
       Nothing -> return Nothing
       Just bmp -> return $ Just (bmpToImage bmp)

-- | writeImage takes a file name and an image to be written as a 32-bit
-- Bitmap Image
{--
writeImage :: String -- ^ The Desired output filename
              -> Image (Int, Int) Word8 -- ^ The Image data to be written as a Bitmap
              -> IO () -- ^ Empty IO Monad
--}
writeImage filename image =
  writeBMP filename (imageToBmp image)




-- |loadBitmap : Loads a 24 or 32 bit BMP.
--  This obviously requires that the image
--  is stored in a Color Bitmap file.
{--
loadBitmap :: String -> IO (Maybe BMP)
--}
loadBitmap fileName  =
  do handle <- openFile fileName ReadMode
     mBMP <- hGetBMP handle
     case mBMP of
       Left err -> do putStrLn ( "Failed to Load Image:" ++ fileName)
                      print err
                      return Nothing
       Right bmp -> return $ Just bmp



-- | This takes an a BMP type and coverts it to an Image type
-- so that it can be worked with
{--
bmpToImage :: BMP -> Image (Int, Int) GHC.Word.Word8
--}
bmpToImage colorBMP =
  let colorRGBA = unpackBMPToRGBA32 colorBMP
      (bmpWidth, bmpHeight) = bmpDimensions colorBMP
      redByteString = Data.ByteString.pack $ getOnlyRed $ Data.ByteString.unpack colorRGBA
      blueByteString = Data.ByteString.pack $ getOnlyBlue $ Data.ByteString.unpack colorRGBA
      greenByteString = Data.ByteString.pack $ getOnlyGreen $ Data.ByteString.unpack colorRGBA
      alphaByteString = Data.ByteString.pack $ getOnlyAlpha $ Data.ByteString.unpack colorRGBA
      in Image { width  = bmpWidth,
                 height = bmpHeight,
                 red = byteStringToArray redByteString bmpWidth bmpHeight,
                 green = byteStringToArray greenByteString bmpWidth bmpHeight,
                 blue = byteStringToArray blueByteString bmpWidth bmpHeight,
                 alpha = byteStringToArray alphaByteString bmpWidth bmpHeight
               }


-- | imageToBmp takes the custom image type and puts it into a
--   32bit RGBA Bitmap
{--
imageToBmp :: Image (Int, Int) GHC.Word.Word8 -> BMP
--}

imageToBmp image =
  let redList = arrayToByteString (red image) (width image) (height image)
      blueList = arrayToByteString (blue image) (width image) (height image)
      greenList = arrayToByteString (green image) (width image) (height image)
      alphaList = arrayToByteString (alpha image) (width image) (height image)
  in
   packRGBA32ToBMP (width image) ( height image ) (Data.ByteString.pack (combineComponents redList greenList blueList alphaList))


-- | arrayToByteString takes a 2-D array and moves it to a 1-D list
--   so that it can be packed into a bytestring
{--
arrayToByteString :: (Ix t2, Ix t1, Num t1, Num t2 , Enum t1, Enum t2)
                     => Array (t1, t2) t -- ^ a 2-D Array indexed from 0
                     -> t1 -- ^ the width in pixels / array elements
                     -> t2 -- ^ the height in pixels / array elements
                     -> [t] -- ^ a 1 dimentional list in raster order of all pixels/elements
--}
--arrayToByteString :: UArray (Int,Int) Float -> Int -> Int -> [Word8]
arrayToByteString image width height =
  [(image ! (i,j)) |
   i <- [0..width-1],
   j <- [0..height-1]]


-- | combineComponents takes arrays containing a single component
--   and aggregates them into a single multiple component array in Raster order (RGBA)
{--
combineComponents :: [a] -- ^ An array of Red components only
                     -> [a] -- ^ An array of Green components only
                     -> [a] -- ^ An array of Blue components only
                     -> [a] -- ^ An array of Alpha components only
                     -> [a] -- ^ An array of All components in raster order RGBA
--}
combineComponents (r:rxs) (g:gxs) (b:bxs) (a:axs) =
  [r,g,b,a] ++ ( combineComponents rxs gxs bxs axs )
combineComponents [] [] [] [] = []


-- |byteStringToArray is takes a ByteString
--  and converts it to a 2-d array of the appropriate width and height.
--  Arrays are indexed from 0,0 to width-1, height-1
{--
byteStringToArray :: Data.ByteString.ByteString -- ^ A Single compnent bytestring
                     -> Int -- ^ Width in Pixels
                     -> Int -- ^ Height in Pixels
                     -> Array (Int, Int) Word8 -- ^ 2-d Array containing a single component
--}
byteStringToArray byteString width height =
  array ((0,0),(width-1,height-1))
  [((i,j), Data.ByteString.index byteString ( i * width + j )) |
   i <- [0..width-1],
   j <- [0..height-1]]


-- | Puts only the red components into a standalone array
getOnlyRed :: [a] -- ^ A List containing RGBA data
              -> [a] -- ^ A List containing only Red data
getOnlyRed (r:_:_:_:xs) = [r] ++ ( getOnlyRed xs )
getOnlyRed [] = []

-- | Puts only the Green components into a standalone array
getOnlyGreen :: [a] -- ^ A List containing RGBA data
                -> [a] -- ^ A List containing only Green data
getOnlyGreen (_:g:_:_:xs) = [g] ++ ( getOnlyGreen xs )
getOnlyGreen [] = []


-- | Puts only the Blue components into a standalone array
getOnlyBlue :: [a]  -- ^ A List containing RGBA data
               -> [a] -- ^ A List containing only Blue data
getOnlyBlue (_:_:b:_:xs) = [b] ++ ( getOnlyBlue xs )
getOnlyBlue [] = []

-- | Puts only the Alpha components into a standalone array
getOnlyAlpha :: [a]  -- ^ A List containing RGBA data
                -> [a] -- ^ A List containing only Alpha data
getOnlyAlpha (_:_:_:a:xs) = [a] ++ ( getOnlyAlpha xs )
getOnlyAlpha [] = []
