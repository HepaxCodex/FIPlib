-- | This module is used as a test base for Haskell 
--   Image processing techniques
module BMPTest where

import Codec.BMP
import Data.Maybe
import qualified Data.ByteString
import Data.Word
import System.Environment
import System.IO
  

-- | runs both loadBitmap and writeRaw
doBoth :: String     -- ^ Path to Input File (BMP)
          -> String  -- ^ Path to Output File (BMP)
          -> IO ()    -- ^
doBoth inFileName outFileName = 
  do myBMP <- loadBitmap inFileName
     writeRaw outFileName myBMP
       
-- | This Function is currently not functional
showBMPInfo :: Maybe BMP -> IO ()
showBMPInfo bmp = 
  case bmp of
    Nothing -> putStrLn "No Information"
    Just bmp -> putStrLn "test"

     
makeComponents :: IO ()
makeComponents = 
  do mBMP <- loadBitmap "lena.bmp"
     case mBMP of
       Nothing -> putStrLn "Failed"
       Just bmp -> makeRedBMP (unpackBMPToRGBA32 bmp)
     
makeRedBMP colorBMP = 
  do let redRGBA = Data.ByteString.pack $ getRed $ Data.ByteString.unpack colorBMP
     let redBMP = packRGBA32ToBMP 512 512 redRGBA
     let blueRGBA = Data.ByteString.pack $ getBlue $ Data.ByteString.unpack colorBMP
     let blueBMP = packRGBA32ToBMP 512 512 blueRGBA
     let greenRGBA = Data.ByteString.pack $ getGreen $ Data.ByteString.unpack colorBMP
     let greenBMP = packRGBA32ToBMP 512 512 greenRGBA
     writeBMP "red.bmp" redBMP 
     writeBMP "blue.bmp" blueBMP
     writeBMP "green.bmp" greenBMP


-- | Gets the red component from an array of 
--   Word8s 
getRed (r:_:_:_:xs) = [r,0,0,0] ++ ( getRed xs )
getRed [] = []

getGreen (_:g:_:_:xs) = [0,g,0,0] ++ ( getGreen xs )
getGreen [] = []

getBlue (_:_:b:_:xs) = [0,0,b,0] ++ ( getBlue xs )
getBlue [] = []

  
-- | Test Of the Comments
writeRaw :: String -> Maybe BMP -> IO ()
writeRaw outputFileName (Just mBMP) = 
  do let (width, height) = bmpDimensions mBMP
     putStrLn $ "width = " ++ show width
     putStrLn $ "height = " ++ show height
     putStrLn "Got it!!!"
     print outputFileName
     do let values = unpackBMPToRGBA32 mBMP
        let newValues = Data.ByteString.map (\x -> x `div` 4) values
        let newBmp = packRGBA32ToBMP width height newValues
        writeBMP outputFileName newBmp
writeRaw fileName Nothing = 
  do putStrLn "Failed "
     
-- | loadBitmap : Loads a 24 or 32 bit BMP.
--   This obviously requires that the image is color
loadBitmap :: String -> IO (Maybe BMP)
loadBitmap fileName  = 
  do handle <- openFile fileName ReadMode
     mBMP <- hGetBMP handle
     case mBMP of
       Left err -> do putStrLn ( "Failed to Load Image:" ++ fileName)
                      print err
                      return Nothing
       Right bmp -> return $ Just bmp

     

           