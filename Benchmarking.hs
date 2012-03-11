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
-- Notes : It would seem that when using this benchmarking code, the images
-- produced are all black (all zero valued pixels in all components).
-- However when the same functions are removed from the benchmarking code
-- then the result images are correct.  Thus I am considering it a result
-- of how the benchmarking code works.  The timing results are still valid
-- because when testing outside of the benchmarking code, the timing is the
-- same (timing taken with bash built-in time repetitively).
--
-- *****************************************************************************



{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction#-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE BangPatterns #-}

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
                         --bench "writeThumb (whnfIO)"    $ nfIO $ writeImage "test.bmp" thumb,
                         --bench "do1Avg"                   $ nfIO $ doAvg thumb,
                         bench "do5Avg --  " $ nfIO $ do5Avg thumb,
                         bench "do10Avg --  " $ nfIO $ do10Avg thumb,
                         bench "twoFilters--" $ nfIO $ twoFilters thumb]
                         --bench "do20Avg --  " $ nfIO $ do20Avg thumb,
                         --bench "do30Avg --  " $ nfIO $ do30Avg thumb,
                         --bench "do40Avg --  " $ nfIO $ do40Avg thumb,
                         --bench "do50Avg --  " $ nfIO $ do50Avg thumb,
                         --bench "do60Avg --  " $ nfIO $ do60Avg thumb,
                         --bench "do70Avg --  " $ nfIO $ do70Avg thumb,
                         --bench "do80Avg --  " $ nfIO $ do80Avg thumb,
                         --bench "do90Avg --  " $ nfIO $ do90Avg thumb,
                         --bench "do100Avg --  " $ nfIO $ do100Avg thumb,
                         --bench "do110Avg --  " $ nfIO $ do110Avg thumb,
                         --bench "do120Avg --  " $ nfIO $ do120Avg thumb,
                         --bench "do130Avg --  " $ nfIO $ do130Avg thumb,
                         --bench "do140Avg --  " $ nfIO $ do140Avg thumb,
                         --bench "do150Avg --  " $ nfIO $ do150Avg thumb]

--                         bench "warmup (whnf)"          $ whnf putStrLn "HelloWorld",
--                         bench "GaussWindow (whnf)"     $ nfIO $ doGauss thumb,
--                         bench "Avg Window (whnf)"      $ nfIO $ doAvg thumb,
--                         bench "AvgGauss Window (whnf)" $ nfIO $ doAvgGauss thumb,
                         --bench "1 window (whnf)"        $ nfIO $ doRed thumb,
                         --bench "1 window (whnf)"        $ nfIO $ doRed2 thumb]
                         --bench "10 windows (whnf)"      $ nfIO $ do10 thumb]


-- doGauss image    = writeImage "doGauss"        $ valueMap ( applyWindow ( gaussian 3 3 1     ) ) image
doAvg image      = writeImage "doAvg.bmp"      $ valueMap ( applyWindow ( arithmeticMean 3 3 ) ) image

myFcn !image = image `seq` putStr ""

twoFilters image =
   writeImage "04twoFilters.bmp" $ valueMap
   (applyWindow (arithmeticMean 3 3))
   (valueMap
    (applyWindow (arithmeticMean 3 3))
    (valueMap
     (applyWindow (arithmeticMean 3 3))
     (valueMap
      (applyWindow (arithmeticMean 3 3)) image)))

do5aAvg image =
         let filterSize = 3
         in myFcn $ valueMap
            (applyWindow ( arithmeticMean filterSize filterSize ))
             (valueMap
              (applyWindow ( arithmeticMean filterSize filterSize))
              (valueMap
               (applyWindow ( arithmeticMean filterSize filterSize))
               (valueMap
                (applyWindow ( arithmeticMean filterSize filterSize))
                (valueMap
                 (applyWindow ( arithmeticMean filterSize filterSize)) image))))



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



do10Avg image =
          let filterSize = 3
          in writeImage "do10Avg.bmp" $
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
                     (applyWindow ( arithmeticMean filterSize filterSize)) image -- 10
                     )))))))))


do20Avg image =
          let filterSize = 3
          in writeImage "do20Avg.bmp" $
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
                               (applyWindow ( arithmeticMean filterSize filterSize)) image -- 20
                               )))))))))))))))))))

do30Avg image =
          let filterSize = 3
          in writeImage "do30Avg.bmp" $
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
                                         (applyWindow ( arithmeticMean filterSize filterSize)) image -- 30
                                         )))))))))))))))))))))))))))))


do40Avg image =
          let filterSize = 3
          in writeImage "do40Avg.bmp" $
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
                                                   (applyWindow ( arithmeticMean filterSize filterSize)) image -- 40
                                                    )))))))))))))))))))))))))))))))))))))))


do50Avg image =
          let filterSize = 3
          in writeImage "do50Avg.bmp" $
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
                                                            (applyWindow ( arithmeticMean filterSize filterSize)) image -- 50
                                                            ))))))))))))))))))))))))))))))))))))))))))))))))

do60Avg image =
          let filterSize = 3
          in writeImage "do60Avg.bmp" $
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
                                                                      (applyWindow ( arithmeticMean filterSize filterSize)) image -- 60
                                                                      ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
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
                                                                                (applyWindow ( arithmeticMean filterSize filterSize)) image -- 70
                                                                                ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
do80Avg image =
          let filterSize = 3
          in writeImage "do80Avg.bmp" $
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
                                                                                (applyWindow ( arithmeticMean filterSize filterSize)) -- 70
                                                                                (valueMap
                                                                                 (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                 (valueMap
                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                  (valueMap
                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                   (valueMap
                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                    (valueMap
                                                                                     (applyWindow ( arithmeticMean filterSize filterSize)) -- 75
                                                                                     (valueMap
                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                      (valueMap
                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                       (valueMap
                                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                        (valueMap
                                                                                         (applyWindow ( arithmeticMean filterSize filterSize)) image -- 80
                                                                                         )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

do90Avg image =
          let filterSize = 3
          in writeImage "do90Avg.bmp" $
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
                                                                                (applyWindow ( arithmeticMean filterSize filterSize)) -- 70
                                                                                (valueMap
                                                                                 (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                 (valueMap
                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                  (valueMap
                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                   (valueMap
                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                    (valueMap
                                                                                     (applyWindow ( arithmeticMean filterSize filterSize)) -- 75
                                                                                     (valueMap
                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                      (valueMap
                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                       (valueMap
                                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                        (valueMap
                                                                                         (applyWindow ( arithmeticMean filterSize filterSize)) -- 80
                                                                                         (valueMap
                                                                                          (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                          (valueMap
                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                           (valueMap
                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                            (valueMap
                                                                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                             (valueMap
                                                                                              (applyWindow ( arithmeticMean filterSize filterSize)) -- 85
                                                                                              (valueMap
                                                                                               (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                               (valueMap
                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                (valueMap
                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                 (valueMap
                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                  (valueMap
                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize)) image -- 90
                                                                                                   )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
do100Avg image =
          let filterSize = 3
          in writeImage "do100Avg.bmp" $
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
                                                                                (applyWindow ( arithmeticMean filterSize filterSize)) -- 70
                                                                                (valueMap
                                                                                 (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                 (valueMap
                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                  (valueMap
                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                   (valueMap
                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                    (valueMap
                                                                                     (applyWindow ( arithmeticMean filterSize filterSize)) -- 75
                                                                                     (valueMap
                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                      (valueMap
                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                       (valueMap
                                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                        (valueMap
                                                                                         (applyWindow ( arithmeticMean filterSize filterSize)) -- 80
                                                                                         (valueMap
                                                                                          (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                          (valueMap
                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                           (valueMap
                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                            (valueMap
                                                                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                             (valueMap
                                                                                              (applyWindow ( arithmeticMean filterSize filterSize)) -- 85
                                                                                              (valueMap
                                                                                               (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                               (valueMap
                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                (valueMap
                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                 (valueMap
                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                  (valueMap
                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize)) -- 90
                                                                                                   (valueMap
                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                    (valueMap
                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                     (valueMap
                                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                      (valueMap
                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                       (valueMap
                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize)) -- 95
                                                                                                        (valueMap
                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                         (valueMap
                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                          (valueMap
                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                           (valueMap
                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                            (valueMap
                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize)) image -- 100
                                                                    )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

do110Avg image =
          let filterSize = 3
          in writeImage "do110Avg.bmp" $
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
                                                                                (applyWindow ( arithmeticMean filterSize filterSize)) -- 70
                                                                                (valueMap
                                                                                 (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                 (valueMap
                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                  (valueMap
                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                   (valueMap
                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                    (valueMap
                                                                                     (applyWindow ( arithmeticMean filterSize filterSize)) -- 75
                                                                                     (valueMap
                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                      (valueMap
                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                       (valueMap
                                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                        (valueMap
                                                                                         (applyWindow ( arithmeticMean filterSize filterSize)) -- 80
                                                                                         (valueMap
                                                                                          (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                          (valueMap
                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                           (valueMap
                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                            (valueMap
                                                                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                             (valueMap
                                                                                              (applyWindow ( arithmeticMean filterSize filterSize)) -- 85
                                                                                              (valueMap
                                                                                               (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                               (valueMap
                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                (valueMap
                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                 (valueMap
                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                  (valueMap
                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize)) -- 90
                                                                                                   (valueMap
                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                    (valueMap
                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                     (valueMap
                                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                      (valueMap
                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                       (valueMap
                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize)) -- 95
                                                                                                        (valueMap
                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                         (valueMap
                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                          (valueMap
                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                           (valueMap
                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                            (valueMap
                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize)) -- 100
                                                                                                             (valueMap
                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                              (valueMap
                                                                                                               (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                               (valueMap
                                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                (valueMap
                                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                 (valueMap
                                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))  -- 105
                                                                                                                  (valueMap
                                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                   (valueMap
                                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                    (valueMap
                                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                      (valueMap
                                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                       (valueMap
                                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize)) image  -- 110
                                                               )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
do120Avg image =
          let filterSize = 3
          in writeImage "do120Avg.bmp" $
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
                                                                                (applyWindow ( arithmeticMean filterSize filterSize)) -- 70
                                                                                (valueMap
                                                                                 (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                 (valueMap
                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                  (valueMap
                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                   (valueMap
                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                    (valueMap
                                                                                     (applyWindow ( arithmeticMean filterSize filterSize)) -- 75
                                                                                     (valueMap
                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                      (valueMap
                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                       (valueMap
                                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                        (valueMap
                                                                                         (applyWindow ( arithmeticMean filterSize filterSize)) -- 80
                                                                                         (valueMap
                                                                                          (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                          (valueMap
                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                           (valueMap
                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                            (valueMap
                                                                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                             (valueMap
                                                                                              (applyWindow ( arithmeticMean filterSize filterSize)) -- 85
                                                                                              (valueMap
                                                                                               (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                               (valueMap
                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                (valueMap
                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                 (valueMap
                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                  (valueMap
                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize)) -- 90
                                                                                                   (valueMap
                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                    (valueMap
                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                     (valueMap
                                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                      (valueMap
                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                       (valueMap
                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize)) -- 95
                                                                                                        (valueMap
                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                         (valueMap
                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                          (valueMap
                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                           (valueMap
                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                            (valueMap
                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize)) -- 100
                                                                                                             (valueMap
                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                              (valueMap
                                                                                                               (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                               (valueMap
                                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                (valueMap
                                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                 (valueMap
                                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))  -- 105
                                                                                                                  (valueMap
                                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                   (valueMap
                                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                    (valueMap
                                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                      (valueMap
                                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                       (valueMap
                                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize)) -- 110
                                                                                                                        (valueMap
                                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                         (valueMap
                                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                          (valueMap
                                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                           (valueMap
                                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize)) -- 115
                                                                                                                            (valueMap
                                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                             (valueMap
                                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                              (valueMap
                                                                                                                               (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                               (valueMap
                                                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                (valueMap
                                                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize)) image  -- 120
                                              ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

do125Avg image =
          let filterSize = 3
          in writeImage "do125Avg.bmp" $
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
                                                                                (applyWindow ( arithmeticMean filterSize filterSize)) -- 70
                                                                                (valueMap
                                                                                 (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                 (valueMap
                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                  (valueMap
                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                   (valueMap
                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                    (valueMap
                                                                                     (applyWindow ( arithmeticMean filterSize filterSize)) -- 75
                                                                                     (valueMap
                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                      (valueMap
                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                       (valueMap
                                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                        (valueMap
                                                                                         (applyWindow ( arithmeticMean filterSize filterSize)) -- 80
                                                                                         (valueMap
                                                                                          (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                          (valueMap
                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                           (valueMap
                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                            (valueMap
                                                                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                             (valueMap
                                                                                              (applyWindow ( arithmeticMean filterSize filterSize)) -- 85
                                                                                              (valueMap
                                                                                               (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                               (valueMap
                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                (valueMap
                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                 (valueMap
                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                  (valueMap
                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize)) -- 90
                                                                                                   (valueMap
                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                    (valueMap
                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                     (valueMap
                                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                      (valueMap
                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                       (valueMap
                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize)) -- 95
                                                                                                        (valueMap
                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                         (valueMap
                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                          (valueMap
                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                           (valueMap
                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                            (valueMap
                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize)) -- 100
                                                                                                             (valueMap
                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                              (valueMap
                                                                                                               (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                               (valueMap
                                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                (valueMap
                                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                 (valueMap
                                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))  -- 105
                                                                                                                  (valueMap
                                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                   (valueMap
                                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                    (valueMap
                                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                      (valueMap
                                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                       (valueMap
                                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize)) -- 110
                                                                                                                        (valueMap
                                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                         (valueMap
                                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                          (valueMap
                                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                           (valueMap
                                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize)) -- 115
                                                                                                                            (valueMap
                                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                             (valueMap
                                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                              (valueMap
                                                                                                                               (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                               (valueMap
                                                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                (valueMap
                                                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize)) -- 120
                                                                                                                                 (valueMap
                                                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                                  (valueMap
                                                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                   (valueMap
                                                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                    (valueMap
                                                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                     (valueMap
                                                                                                                                      (applyWindow ( arithmeticMean filterSize filterSize)) image  -- 125
                                              )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))




do130Avg image =
          let filterSize = 3
          in writeImage "do130Avg.bmp" $
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
                                                                                (applyWindow ( arithmeticMean filterSize filterSize)) -- 70
                                                                                (valueMap
                                                                                 (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                 (valueMap
                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                  (valueMap
                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                   (valueMap
                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                    (valueMap
                                                                                     (applyWindow ( arithmeticMean filterSize filterSize)) -- 75
                                                                                     (valueMap
                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                      (valueMap
                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                       (valueMap
                                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                        (valueMap
                                                                                         (applyWindow ( arithmeticMean filterSize filterSize)) -- 80
                                                                                         (valueMap
                                                                                          (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                          (valueMap
                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                           (valueMap
                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                            (valueMap
                                                                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                             (valueMap
                                                                                              (applyWindow ( arithmeticMean filterSize filterSize)) -- 85
                                                                                              (valueMap
                                                                                               (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                               (valueMap
                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                (valueMap
                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                 (valueMap
                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                  (valueMap
                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize)) -- 90
                                                                                                   (valueMap
                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                    (valueMap
                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                     (valueMap
                                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                      (valueMap
                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                       (valueMap
                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize)) -- 95
                                                                                                        (valueMap
                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                         (valueMap
                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                          (valueMap
                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                           (valueMap
                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                            (valueMap
                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize)) -- 100
                                                                                                             (valueMap
                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                              (valueMap
                                                                                                               (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                               (valueMap
                                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                (valueMap
                                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                 (valueMap
                                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))  -- 105
                                                                                                                  (valueMap
                                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                   (valueMap
                                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                    (valueMap
                                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                      (valueMap
                                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                       (valueMap
                                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize)) -- 110
                                                                                                                        (valueMap
                                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                         (valueMap
                                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                          (valueMap
                                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                           (valueMap
                                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize)) -- 115
                                                                                                                            (valueMap
                                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                             (valueMap
                                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                              (valueMap
                                                                                                                               (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                               (valueMap
                                                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                (valueMap
                                                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize)) -- 120
                                                                                                                                 (valueMap
                                                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                                  (valueMap
                                                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                   (valueMap
                                                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                    (valueMap
                                                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                     (valueMap
                                                                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))   -- 125
                                                                                                                                      (valueMap
                                                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                                       (valueMap
                                                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                        (valueMap
                                                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                         (valueMap
                                                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                          (valueMap
                                                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize)) image  -- 130
                                              ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))



do140Avg image =
          let filterSize = 3
          in writeImage "do140Avg.bmp" $
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
                                                                                (applyWindow ( arithmeticMean filterSize filterSize)) -- 70
                                                                                (valueMap
                                                                                 (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                 (valueMap
                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                  (valueMap
                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                   (valueMap
                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                    (valueMap
                                                                                     (applyWindow ( arithmeticMean filterSize filterSize)) -- 75
                                                                                     (valueMap
                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                      (valueMap
                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                       (valueMap
                                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                        (valueMap
                                                                                         (applyWindow ( arithmeticMean filterSize filterSize)) -- 80
                                                                                         (valueMap
                                                                                          (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                          (valueMap
                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                           (valueMap
                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                            (valueMap
                                                                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                             (valueMap
                                                                                              (applyWindow ( arithmeticMean filterSize filterSize)) -- 85
                                                                                              (valueMap
                                                                                               (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                               (valueMap
                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                (valueMap
                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                 (valueMap
                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                  (valueMap
                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize)) -- 90
                                                                                                   (valueMap
                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                    (valueMap
                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                     (valueMap
                                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                      (valueMap
                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                       (valueMap
                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize)) -- 95
                                                                                                        (valueMap
                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                         (valueMap
                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                          (valueMap
                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                           (valueMap
                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                            (valueMap
                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize)) -- 100
                                                                                                             (valueMap
                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                              (valueMap
                                                                                                               (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                               (valueMap
                                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                (valueMap
                                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                 (valueMap
                                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))  -- 105
                                                                                                                  (valueMap
                                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                   (valueMap
                                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                    (valueMap
                                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                      (valueMap
                                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                       (valueMap
                                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize)) -- 110
                                                                                                                        (valueMap
                                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                         (valueMap
                                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                          (valueMap
                                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                           (valueMap
                                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize)) -- 115
                                                                                                                            (valueMap
                                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                             (valueMap
                                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                              (valueMap
                                                                                                                               (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                               (valueMap
                                                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                (valueMap
                                                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize)) -- 120
                                                                                                                                 (valueMap
                                                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                                  (valueMap
                                                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                   (valueMap
                                                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                    (valueMap
                                                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                     (valueMap
                                                                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))   -- 125
                                                                                                                                      (valueMap
                                                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                                       (valueMap
                                                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                        (valueMap
                                                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                         (valueMap
                                                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                          (valueMap
                                                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))   -- 130
                                                                                                                                           (valueMap
                                                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                                           (valueMap
                                                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                            (valueMap
                                                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                             (valueMap
                                                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                              (valueMap
                                                                                                                                               (applyWindow ( arithmeticMean filterSize filterSize))   -- 135
                                                                                                                                               (valueMap
                                                                                                                                                (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                                                (valueMap
                                                                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                                 (valueMap
                                                                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                                  (valueMap
                                                                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                                   (valueMap
                                                                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize)) image  -- 140
                                              ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))




do150Avg image =
          let filterSize = 3
          in writeImage "do150Avg.bmp" $
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
                                                                                (applyWindow ( arithmeticMean filterSize filterSize)) -- 70
                                                                                (valueMap
                                                                                 (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                 (valueMap
                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                  (valueMap
                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                   (valueMap
                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                    (valueMap
                                                                                     (applyWindow ( arithmeticMean filterSize filterSize)) -- 75
                                                                                     (valueMap
                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                      (valueMap
                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                       (valueMap
                                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                        (valueMap
                                                                                         (applyWindow ( arithmeticMean filterSize filterSize)) -- 80
                                                                                         (valueMap
                                                                                          (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                          (valueMap
                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                           (valueMap
                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                            (valueMap
                                                                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                             (valueMap
                                                                                              (applyWindow ( arithmeticMean filterSize filterSize)) -- 85
                                                                                              (valueMap
                                                                                               (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                               (valueMap
                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                (valueMap
                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                 (valueMap
                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                  (valueMap
                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize)) -- 90
                                                                                                   (valueMap
                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                    (valueMap
                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                     (valueMap
                                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                      (valueMap
                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                       (valueMap
                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize)) -- 95
                                                                                                        (valueMap
                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                         (valueMap
                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                          (valueMap
                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                           (valueMap
                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                            (valueMap
                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize)) -- 100
                                                                                                             (valueMap
                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                              (valueMap
                                                                                                               (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                               (valueMap
                                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                (valueMap
                                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                 (valueMap
                                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))  -- 105
                                                                                                                  (valueMap
                                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                   (valueMap
                                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                    (valueMap
                                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                      (valueMap
                                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                       (valueMap
                                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize)) -- 110
                                                                                                                        (valueMap
                                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                         (valueMap
                                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                          (valueMap
                                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                           (valueMap
                                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize)) -- 115
                                                                                                                            (valueMap
                                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                             (valueMap
                                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                              (valueMap
                                                                                                                               (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                               (valueMap
                                                                                                                                (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                (valueMap
                                                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize)) -- 120
                                                                                                                                 (valueMap
                                                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                                  (valueMap
                                                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                   (valueMap
                                                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                    (valueMap
                                                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                     (valueMap
                                                                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))   -- 125
                                                                                                                                      (valueMap
                                                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                                       (valueMap
                                                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                        (valueMap
                                                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                         (valueMap
                                                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                          (valueMap
                                                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))   -- 130
                                                                                                                                           (valueMap
                                                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                                           (valueMap
                                                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                            (valueMap
                                                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                             (valueMap
                                                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                              (valueMap
                                                                                                                                               (applyWindow ( arithmeticMean filterSize filterSize))   -- 135
                                                                                                                                               (valueMap
                                                                                                                                                (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                                                (valueMap
                                                                                                                                                 (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                                 (valueMap
                                                                                                                                                  (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                                  (valueMap
                                                                                                                                                   (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                                   (valueMap
                                                                                                                                                    (applyWindow ( arithmeticMean filterSize filterSize))   -- 140
                                                                                                                                                    (valueMap
                                                                                                                                                     (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                                                     (valueMap
                                                                                                                                                      (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                                      (valueMap
                                                                                                                                                       (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                                       (valueMap
                                                                                                                                                        (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                                        (valueMap
                                                                                                                                                         (applyWindow ( arithmeticMean filterSize filterSize))   -- 145
                                                                                                                                                         (valueMap
                                                                                                                                                          (applyWindow ( arithmeticMean filterSize filterSize ))
                                                                                                                                                          (valueMap
                                                                                                                                                           (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                                           (valueMap
                                                                                                                                                            (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                                            (valueMap
                                                                                                                                                             (applyWindow ( arithmeticMean filterSize filterSize))
                                                                                                                                                             (valueMap
                                                                                                                                                              (applyWindow ( arithmeticMean filterSize filterSize)) image  -- 150
                                              ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))



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
