
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
-- module Benchmarking (
--   main,
--   benchMarkFilterNoIO,
--   doGauss,
--   doAvgGauss
--   )
--where


import FIPlib.Core
import FIPlib.Filters
import Criterion.Main


main = benchMarkFilterNoIO
--  defaultMain
--    [bench "warmup" $ whnf putStrLn "Hello World",
--     bench "smoothingDemo" $ smoothingDemo]

benchMarkFilterNoIO =
  do thumbImage <- loadImage "ThumbnailDemo.bmp"
     case thumbImage of
       Nothing -> putStrLn "Failed to Load ThumbnailDemo Image"
       Just thumb -> let gaussWindow = gaussian 3 3 1
                         avgWindow  = arithmeticMean 3 3
                     in defaultMain
                        [bench "warmup (whnf)"          $ whnf putStrLn "HelloWorld",
--                         bench "warmup (whnf)"          $ whnf putStrLn "HelloWorld",
--                         bench "GaussWindow (whnf)"     $ nfIO $ doGauss thumb,
--                         bench "Avg Window (whnf)"      $ nfIO $ doAvg thumb,
--                         bench "AvgGauss Window (whnf)" $ nfIO $ doAvgGauss thumb,
                         bench "10 windows (whnf)"      $ nfIO $ do5 thumb]


--doGauss image    = writeImage "doGauss"        $ valueMap ( applyWindow (gaussian 3 3 1)) image
doAvg image      = writeImage "doAvg.bmp"      $ valueMap ( applyWindow (arithmeticMean 3 3 )) image
--doAvgGauss image = writeImage "doAvgGauss.bmp" $ valueMap ( applyWindow (gaussian 3 3 1 )) ( valueMap (applyWindow (arithmeticMean 3 3)) image )

avgGauss image = valueMap ( applyWindow (arithmeticMean 3 3 )) ( valueMap (applyWindow (arithmeticMean 3 3)) image )
do5 image = writeImage "do10.bmp" $  avgGauss $ avgGauss $ avgGauss $ avgGauss $ avgGauss image

-- foo image = valueMap( applyWindow (gaussian 3 3 1)) ( valueMap ( applyWindow ( arithmeticMean 3 3)) image)

-- do10 image = writeImage "do10.bmp" $ foo . foo . foo . foo . foo image
