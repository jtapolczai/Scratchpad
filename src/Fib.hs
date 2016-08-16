module Fib where

import Memo


slowFib :: Int -> Integer
slowFib 0 = 1
slowFib 1 = 1
slowFib n = slowFib (n-1) + slowFib (n-2)

dynFib :: Int -> Integer
dynFib = (fibs !!)
   where fibs = 1 : 1 : zipWith (+) (tail fibs)

memoFib :: Int -> Integer
memoFib n = memoized (mf n)
   where mf 0 = return 1
         mf 1 = return 1
         mf n = do x <- memo' mf (n-1)
                   y <- memo' mf (n-2)
                   return $! x+y

fastFib :: Int -> Integer
fastFib n = if even n then
               fastFib k * (2 * fastFib (k + 1) - fastFib k)
            else
               fastFib (k' + 1) ^ 2 + fastFib k' ^ 2
   where k = n `div` 2
         k' = (n - 1) `div` 2
