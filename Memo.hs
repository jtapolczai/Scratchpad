import qualified Data.IntMap as IM
import qualified Data.Array.IArray as A

-- Naive solution.
f :: (Int -> Int) -> Int -> Int
f _ 0 = 0
f memo n = max n $ memo (n `div` 2) +
                       memo (n `div` 3) + 
                       memo (n `div` 4)

-- Inefficient solutions, as bad as the naive one.
-- The data structures are likely not shared in the
-- course of the mutual recursion.
memoList :: Int -> Int
memoList n = map (f memoList) [0..] !! n

memoMap :: Int -> Int -> Int
memoMap to = (IM.!) (IM.fromAscList vals)
   where
      vals :: [(Int, Int)]
      vals = [(i, f (memoMap to) i) | i <- [0..to]]

memoArr :: Int -> Int -> Int
memoArr to = (A.!) vals
   where
      vals :: A.Array Int Int
      vals = A.array (0, to) [(i, f (memoArr to) i) | i <- [0..to]]


-- Ugly, hand-crafted solution. Works as expected.
-- Could be solved with an applicative functor.
f' :: IM.IntMap Int -> Int -> (Int, IM.IntMap Int)
f' m 0 = (0,m)
f' m n = (res, mOut)
   where (two,   m1) = checkMemo m (n `div` 2)
         (three, m2) = checkMemo m1 (n `div` 3)
         (four,  m3) = checkMemo m2 (n `div` 4)

         res = max n $ two + three + four
         mOut = IM.insert n res m3 

         checkMemo mp i = case IM.lookup i mp of
                             Nothing -> f' mp i
                             Just e -> (e, mp)

fMemoList = f memoList
fMemoMap n = f (memoMap (n `div` 2)) n
fMemoArr n = f (memoArr (n `div` 2)) n
fMemoHand = fst . f' IM.empty

fNoMemo 0 = 0
fNoMemo n = max n $ fNoMemo (n `div` 2) +
                    fNoMemo (n `div` 3) +
                    fNoMemo (n `div` 4)