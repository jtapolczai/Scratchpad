{-# LANGUAGE RecursiveDo, BangPatterns #-}
import Control.Applicative
import Data.Function (fix)
import Data.IntMap as IntMap
import Control.Monad.Fix (mfix)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Text.Read (readMaybe)

fibIO4 = do
  rec
    fib <- do
      putStrLn "Enter the start number"
      start <- read <$> getLine
      return $ start : 1 : zipWith (+) fib (tail fib)
  return fib

data Tree a = Node a (Tree a) (Tree a) | Leaf a
   deriving (Show, Eq)

nodeVal :: Tree a -> a
nodeVal (Node a _ _) = a
nodeVal (Leaf a) = a

sum :: Tree Integer -> Tree Integer
sum = sum' 0
   where
      sum' s (Leaf v) = Leaf (s + v)
      sum' s (Node v l r) = (Node valNewL newL newR)
         where v' = v + s
               --valOldL = nodeVal l
               --newR = sum' (v' + valOldL) r           
               --valNewR = nodeVal newR
               --newL = sum' (valNewR - valOldL) l
               --valNewL = nodeVal newL

               newL = sum' (v' + nodeVal newR) l
               newR = sum' (v' + nodeVal newL) r
               newS = Node (nodeVal newL) newL newR
