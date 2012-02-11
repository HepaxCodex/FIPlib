{-- LANGUAGE NoMonomorphismRestriction --}
-- | Developed by Andrew Kordik for
-- | the University of Dayton
-- | All Rights Reserved
-- | This library is still highly volitile.

-- module Benchmarking (
--   main,
--   benchMarkFilterNoIO,
--   doGauss,
--   doAvgGauss
--   )
--where



import FIPlib.Core
import FIPlib.Filters
--import Data.Array.IArray
import Data.Array.Unboxed
import Data.Word
import Criterion.Main


main = profileFunc
--  defaultMain
--    [bench "warmup" $ whnf putStrLn "Hello World",
--     bench "smoothingDemo" $ smoothingDemo]

profileFunc =
  do thumbImage <- loadImage "ThumbnailDemo.bmp"
     case thumbImage of
       Nothing -> putStrLn "Failed to Load ThumbnailDemo Image"
       Just thumb -> doAvg thumb



--doGauss image    = writeImage "doGauss"        $ valueMap ( applyWindow (gaussian 3 3 1)) image
doAvg image  = writeImage "doAvg.bmp" $ valueMap ( applyWindow (arithmeticMean 3 3 )) image
--doAvgGauss image = writeImage "doAvgGauss.bmp" $ valueMap ( applyWindow (gaussian 3 3 1 )) ( valueMap (applyWindow (arithmeticMean 3 3)) image )

--avgGauss image = valueMap ( applyWindow (gaussian 3 3 1 )) ( valueMap (applyWindow (arithmeticMean 3 3)) image )
--do5 image = writeImage "do10.bmp" $  avgGauss $ avgGauss $ avgGauss $ avgGauss $ avgGauss image

--foo image = valueMap( applyWindow (gaussian 3 3 1)) ( valueMap ( applyWindow ( arightmeticMean 3 3)) image)

--do10 image = writeImage "do10.bmp" $ foo . foo . foo . foo . foo image
