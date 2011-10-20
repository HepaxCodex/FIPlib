
import Criterion.Main


main = defaultMain
       [ bench "warmup (whnf)" $ whnf putStrLn "HelloWorld",
         bench "single (whnf)" $ whnf single [1..10000000],
         bench "single (nf)"   $ nf   single [1..10000000],
         bench "double (whnf)" $ whnf double [1..10000000],
         bench "double (nf)"   $ nf   double [1..10000000]]



single :: [Int] -> [Int]
single lst = fmap (+ 2) lst
            
double :: [Int] -> [Int]             
double lst =  fmap (* 2) $ fmap (+ 2) lst