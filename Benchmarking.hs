-- *****************************************************************************
--
-- Filename    : Benchmakring.hs
--
-- Description : Contains code used to benchmakr the FIPlib library
--
-- Author      : Andrew Kordik
--
-- Copyright A.M. Kordik and S. Perugini
--
-- *****************************************************************************



{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction#-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

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
--   where
-- module Benchmarking where



import FIPlib.Core
import FIPlib.Filters
import Criterion.Main
import Data.Array.Unboxed


main = benchMarkFilterNoIO
--  defaultMain
--    [bench "warmup" $ whnf putStrLn "Hello World",
--     bench "smoothingDemo" $ smoothingDemo]

benchMarkFilterNoIO =
  do thumbImage <- loadImage "ThumbnailDemo.bmp"
     case thumbImage of
       Nothing -> putStrLn "Failed to Load ThumbnailDemo Image"
       Just thumb -> let --gaussWindow = gaussian 3 3 1
                         --1(avgWindow)= (arithmeticMean 3 3)
                     in defaultMain
                        [bench "warmup (whnf)"          $ whnf putStrLn "HelloWorld",
                         bench "do5Avg" $ nfIO $ do35Avg thumb]
--                         bench "warmup (whnf)"          $ whnf putStrLn "HelloWorld",
--                         bench "GaussWindow (whnf)"     $ nfIO $ doGauss thumb,
--                         bench "Avg Window (whnf)"      $ nfIO $ doAvg thumb,
--                         bench "AvgGauss Window (whnf)" $ nfIO $ doAvgGauss thumb,
                         --bench "1 window (whnf)"        $ nfIO $ doRed thumb,
                         --bench "1 window (whnf)"        $ nfIO $ doRed2 thumb]
                         --bench "10 windows (whnf)"      $ nfIO $ do10 thumb]


-- doGauss image    = writeImage "doGauss"        $ valueMap ( applyWindow ( gaussian 3 3 1     ) ) image
doAvg image      = writeImage "doAvg.bmp"      $ valueMap ( applyWindow ( arithmeticMean 3 3 ) ) image

do5Avg image =
         let filterSize = 3
         in writeImage "do5Avg.bmp" $ valueMap
            (applyWindow ( arithmeticMean filterSize filterSize ))
             (valueMap
              (applyWindow ( arithmeticMean filterSize filterSize))
              (valueMap
               (applyWindow ( arithmeticMean filterSize filterSize))
               (valueMap
                (applyWindow ( arithmeticMean filterSize filterSize))
                (valueMap
                 (applyWindow ( arithmeticMean filterSize filterSize)) image))))
do35Avg image =
          let filterSize = 3
          in writeImage "do35Avg.bmp" $ valueMap
             (applyWindow ( arithmeticMean filterSize filterSize ))
             (valueMap
              (applyWindow ( arithmeticMean filterSize filterSize))
              (valueMap
               (applyWindow ( arithmeticMean filterSize filterSize))
               (valueMap
                (applyWindow ( arithmeticMean filterSize filterSize))
                (valueMap
                 (applyWindow ( arithmeticMean filterSize filterSize)) -- 5
                 (valueMap
                  (applyWindow ( arithmeticMean filterSize filterSize))
                  (valueMap
                   (applyWindow ( arithmeticMean filterSize filterSize))
                   (valueMap
                    (applyWindow ( arithmeticMean filterSize filterSize))
                    (valueMap
                     (applyWindow ( arithmeticMean filterSize filterSize)) -- 10
                     (valueMap
                      (applyWindow ( arithmeticMean filterSize filterSize ))
                      (valueMap
                       (applyWindow ( arithmeticMean filterSize filterSize))
                       (valueMap
                        (applyWindow ( arithmeticMean filterSize filterSize))
                        (valueMap
                         (applyWindow ( arithmeticMean filterSize filterSize))
                         (valueMap
                          (applyWindow ( arithmeticMean filterSize filterSize)) -- 15
                          (valueMap
                           (applyWindow ( arithmeticMean filterSize filterSize ))
                           (valueMap
                            (applyWindow ( arithmeticMean filterSize filterSize))
                            (valueMap
                             (applyWindow ( arithmeticMean filterSize filterSize))
                             (valueMap
                              (applyWindow ( arithmeticMean filterSize filterSize))
                              (valueMap
                               (applyWindow ( arithmeticMean filterSize filterSize)) -- 20
                               (valueMap
                                (applyWindow ( arithmeticMean filterSize filterSize ))
                                (valueMap
                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                 (valueMap
                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                  (valueMap
                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                   (valueMap
                                    (applyWindow ( arithmeticMean filterSize filterSize)) -- 25
                                    (valueMap
                                     (applyWindow ( arithmeticMean filterSize filterSize ))
                                     (valueMap
                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                      (valueMap
                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                       (valueMap
                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                        (valueMap
                                         (applyWindow ( arithmeticMean filterSize filterSize)) -- 30
                                         (valueMap
                                          (applyWindow ( arithmeticMean filterSize filterSize ))
                                          (valueMap
                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                           (valueMap
                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                            (valueMap
                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                             (valueMap
                                              (applyWindow ( arithmeticMean filterSize filterSize)) image -- 35
                                              )))))))))))))))))))))))))))))))))


do70Avg image =
          let filterSize = 3
          in writeImage "do70Avg.bmp" $
            (valueMap
             (applyWindow ( arithmeticMean filterSize filterSize ))
             (valueMap
              (applyWindow ( arithmeticMean filterSize filterSize))
              (valueMap
               (applyWindow ( arithmeticMean filterSize filterSize))
               (valueMap
                (applyWindow ( arithmeticMean filterSize filterSize))
                (valueMap
                 (applyWindow ( arithmeticMean filterSize filterSize)) -- 5
                 (valueMap
                  (applyWindow ( arithmeticMean filterSize filterSize))
                  (valueMap
                   (applyWindow ( arithmeticMean filterSize filterSize))
                   (valueMap
                    (applyWindow ( arithmeticMean filterSize filterSize))
                    (valueMap
                     (applyWindow ( arithmeticMean filterSize filterSize)) -- 10
                     (valueMap
                      (applyWindow ( arithmeticMean filterSize filterSize ))
                      (valueMap
                       (applyWindow ( arithmeticMean filterSize filterSize))
                       (valueMap
                        (applyWindow ( arithmeticMean filterSize filterSize))
                        (valueMap
                         (applyWindow ( arithmeticMean filterSize filterSize))
                         (valueMap
                          (applyWindow ( arithmeticMean filterSize filterSize)) -- 15
                          (valueMap
                           (applyWindow ( arithmeticMean filterSize filterSize ))
                           (valueMap
                            (applyWindow ( arithmeticMean filterSize filterSize))
                            (valueMap
                             (applyWindow ( arithmeticMean filterSize filterSize))
                             (valueMap
                              (applyWindow ( arithmeticMean filterSize filterSize))
                              (valueMap
                               (applyWindow ( arithmeticMean filterSize filterSize)) -- 20
                               (valueMap
                                (applyWindow ( arithmeticMean filterSize filterSize ))
                                (valueMap
                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                 (valueMap
                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                  (valueMap
                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                   (valueMap
                                    (applyWindow ( arithmeticMean filterSize filterSize)) -- 25
                                    (valueMap
                                     (applyWindow ( arithmeticMean filterSize filterSize ))
                                     (valueMap
                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                      (valueMap
                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                       (valueMap
                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                        (valueMap
                                         (applyWindow ( arithmeticMean filterSize filterSize)) -- 30
                                         (valueMap
                                          (applyWindow ( arithmeticMean filterSize filterSize ))
                                          (valueMap
                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                           (valueMap
                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                            (valueMap
                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                             (valueMap
                                              (applyWindow ( arithmeticMean filterSize filterSize))  -- 35
                                              (valueMap
                                               (applyWindow ( arithmeticMean filterSize filterSize ))
                                               (valueMap
                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                (valueMap
                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                 (valueMap
                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                  (valueMap
                                                   (applyWindow ( arithmeticMean filterSize filterSize)) -- 40
                                                   (valueMap
                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                    (valueMap
                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                     (valueMap
                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                      (valueMap
                                                       (applyWindow ( arithmeticMean filterSize filterSize)) -- 45
                                                       (valueMap
                                                        (applyWindow ( arithmeticMean filterSize filterSize ))
                                                        (valueMap
                                                         (applyWindow ( arithmeticMean filterSize filterSize))
                                                         (valueMap
                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                          (valueMap
                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                           (valueMap
                                                            (applyWindow ( arithmeticMean filterSize filterSize)) -- 50
                                                            (valueMap
                                                             (applyWindow ( arithmeticMean filterSize filterSize ))
                                                             (valueMap
                                                              (applyWindow ( arithmeticMean filterSize filterSize))
                                                              (valueMap
                                                               (applyWindow ( arithmeticMean filterSize filterSize))
                                                               (valueMap
                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                (valueMap
                                                                 (applyWindow ( arithmeticMean filterSize filterSize)) -- 55
                                                                 (valueMap
                                                                  (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                  (valueMap
                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                   (valueMap
                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                    (valueMap
                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                     (valueMap
                                                                      (applyWindow ( arithmeticMean filterSize filterSize)) -- 60
                                                                      (valueMap
                                                                       (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                       (valueMap
                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                        (valueMap
                                                                         (applyWindow ( arithmeticMean filterSize filterSize))
                                                                         (valueMap
                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                          (valueMap
                                                                           (applyWindow ( arithmeticMean filterSize filterSize)) -- 65
                                                                           (valueMap
                                                                            (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                            (valueMap
                                                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                                                             (valueMap
                                                                              (applyWindow ( arithmeticMean filterSize filterSize))
                                                                              (valueMap
                                                                               (applyWindow ( arithmeticMean filterSize filterSize))
                                                                               (valueMap
                                                                                (applyWindow ( arithmeticMean filterSize filterSize)) image  -- 70
                                              ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))




{--
doAvg image = writeImage "doAvg" $ valueMap (applyWindow (arithmeticMean 3 3)) (valueMap (applyWindow (arithmeticMean 3 3)) image)
doRed image =
  let win1 = arithmeticMean 3 3
      win2 = win1
  in writeImage "doRed.bmp" $ valueMap (applyWindow win1) (valueMap (applyWindow win2 ) image)
doRed2 image =
  let win1 = arithmeticMean 3 3
      win2 = win1
  in writeImage "doRed.bmp" $ valueMap (applyWindow win1) (valueMap (applyWindow win2 ) (valueMap (applyWindow win1) (valueMap (applyWindow win2 ) image)))
--}

--do10 image = writeImage "do10.bmp" $ foo $ foo $ image

--do10 image = writeImage "do10.bmp" $ valueMap (applyWindow (arithmeticMean 3 3)) ( valueMap (applyWindow (arithmeticMean 3 3)) image)

--do10 image = writeImage "do10.bmp" $ valueMap (applyWindow (arithmeticMean 3 3)) . valueMap (applyWindow (arithmeticMean 3 3)) $ image

do10 image = writeImage "do10.bmp" $ foo $ foo $ image

{-# INLINE foo #-}
foo  = valueMap (applyWindow (arithmeticMean 3 3))
