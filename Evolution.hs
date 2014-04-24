{-# LANGUAGE TupleSections #-}

module Evolution where

import System.Random
import Control.Monad
import Control.Arrow
import Data.List
import Data.Ord
--import Data.MList

type Probability = Double
type FitnessValue = Double
type Quantile = Double
type FitnessFunction a = a -> FitnessValue
type ReproducingFunction a = [a] -> [a]
type ReproductivePressureFunction = Quantile -> Probability
type Population a = [a]

boolChance :: Double -> IO Bool
boolChance p = liftM (p <) (randomRIO (0.0,100.0))

assignQuantile :: [a]
               -> [(a, Quantile)]
assignQuantile xs = zipWith divByLen xs ([0..]::[Integer])
   where len = fromIntegral $ length xs
         divByLen e i = (e, fromIntegral i / len)

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = take (i-1) xs ++ drop (i+1) xs

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do ind <- randomRIO (0, length xs - 1)
                rest <- shuffle (deleteAt ind xs)
                return $! (xs !! ind) : rest

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

evolution :: FitnessFunction a
          -> ReproducingFunction a
          -> ReproductivePressureFunction
          -> Population a
          -> IO (Population a)
evolution fitness reproduce pressure pop =
   let
     doesReproduce = second (boolChance . pressure)

     fitnessSorted = assignQuantile $ sortBy (comparing fitness) pop
     chosenPop = liftM (map fst) $ filterM snd $ map doesReproduce fitnessSorted

     chosenPairs = liftM (chop 2) $ shuffle =<< chosenPop

     newGen = liftM (concatMap reproduce) chosenPairs

     --futureGens = newGen >>= evolution fitness reproduce pressure
   in
      --newGen :# futureGens
      newGen
