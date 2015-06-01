fizzbuzz :: Int -> IO ()
fizzbuzz to = mapM_ output [1..to]
   where
      output = cond [(divBy 15, putStrLn "fizzBuzz!"),
                     (divBy 5, putStrLn "buzz!"),
                     (divBy 3, putStrLn "fizz!")]
                    (putStrLn "-")

      cond :: [(a -> Bool, b)] -> b -> a -> b
      cond ((x,r):xs) y val | x val = r
                            | otherwise = cond xs y val
      cond [] y _ = y

      divBy :: Int -> Int -> Bool
      divBy x y = 0 == y `rem` x