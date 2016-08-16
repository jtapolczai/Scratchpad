{-# LANGUAGE ScopedTypeVariables #-}

module RandomChoice where

import qualified Data.Array as A
import System.Random
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Proxy
import Data.Foldable
import Control.Monad


data Die = One | Two | Three | Four | Five | Six
   deriving (Show, Eq, Ord, Enum, Bounded)

choose :: (Num a, Random a) => a -> IO a
choose to = randomRIO (0, to)

initMap :: forall a b.(Bounded a, Enum a, Ord a, Num b) => M.Map a b
initMap = M.fromList $ zip keys (repeat 0)
   where
      keys :: [a]
      keys = [minBound..maxBound]

trials :: forall a.(Bounded a, Enum a, Ord a) => Int -> IO (M.Map a Int)
trials n = fmap (foldl' accum initMap) $ sequence $ replicate n $ choose $ fromEnum (maxBound :: a)
   where
      accum :: M.Map a Int -> Int -> M.Map a Int
      accum m k = M.adjust (+1) (toEnum k) m

printMap :: (Show a, Show b, Ord b) => (a -> b -> String) -> M.Map a b -> IO ()
printMap display = mapM_ (putStrLn . uncurry display) . M.toList

avg :: (Foldable f, Integral a, Fractional b) => f a -> b
avg = uncurry d . foldl' accum (0,0)
   where
      d s l = fromIntegral s / fromIntegral l
      accum (s,l) x = (s+x,l+1)

defDisp :: (Show a, Show b, Integral b) => a -> (b, Float) -> String
defDisp k (v,diff) = show k
                     ++ ": " ++ show v
                     ++ ", Δ: " ++ show diff
                     ++ ", Δ²: " ++ show (diff * diff)
                     ++ ", Δ%: " ++ show (diff / fromIntegral v)

addDiff :: (Num a, Integral b) => a -> b -> (b, a)
addDiff a x = (x, fromIntegral x - a)

randomTest :: forall a.(Show a, Enum a, Bounded a, Ord a) => Proxy a -> Int -> IO ()
randomTest _ n = do
   res <- (trials n :: IO (M.Map a Int))
   let (avgRes :: Float) = avg res
       res' = fmap (addDiff avgRes) res
   printMap defDisp res'
   putStrLn $ replicate 20 '-'
   putStrLn $ "Avg: " ++ show avgRes


