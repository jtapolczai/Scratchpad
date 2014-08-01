{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}

module Memo where

import Prelude hiding (return, (>>=))

--import Control.Applicative (Applicative(..))
--import Control.Arrow (first)
import qualified Data.Map as M
import qualified Data.IntMap as IM

--newtype Memo m k v = Memo{runMemo:: m k v -> (v, m k v)}

-- |A generic wrapper for keys which should be memoized with trees.
newtype TreeKey k = TreeKey{fromTreeKey::k} deriving (Show, Eq, Ord, Read)

-- |A generic wrapper for keys which should be memoized with (ascendingly
--  sorted) lists of key-value pairs.
newtype AscListKey k = AscListKey{fromAscListKey::k} deriving (Show, Eq, Ord, Read)

newtype PairList k v = PL{fromPL::[(k,v)]} deriving (Show, Eq, Ord, Read)

-- |The class of memoizable structures.
class Ord k => MemoKey k where
   data MemoStruct k :: * -> *
   data Memo k :: * -> *
   -- |Takes a memo structure, a key, an unevaluated value
   --  and does one of two things:
   --  1. if the key is present, the corresponding value
   --     and the unchanged structure are returned.
   --  2. if the key is not present, the unevaluated value v
   --     is inserted and v, together with the new structure,
   --     is returned.
   compute :: k -> Memo k v -> Memo k v
   makeMemo :: (MemoStruct k v -> (v, MemoStruct k v)) -> Memo k v
   runMemo :: Memo k v -> MemoStruct k v -> (v, MemoStruct k v)

   emptyMemo :: Ord k => MemoStruct k v

compute' :: MemoKey k => k -> (k -> Memo k v) -> Memo k v
compute' k f = compute k (f k)

runMemoized :: MemoKey k => (k -> Memo k v) -> k -> v
runMemoized f k = fst $ runMemo (f k) emptyMemo

instance MemoKey Int where
   -- Ints use IntMaps as memo structures.
   newtype MemoStruct Int v = IMS (IM.IntMap v)
   newtype Memo Int v = Memo (MemoStruct Int v -> (v, MemoStruct Int v))

   compute k thunkV = makeMemo $ \(IMS m) ->
      case IM.lookup k m of
         Nothing -> let
                       (computedV, IMS m') = runMemo thunkV (IMS m)
                    in 
                       (computedV , IMS $ IM.insert k computedV m')
         Just memoizedV -> (memoizedV, IMS m)

   makeMemo = Memo
   runMemo (Memo m) = m
   emptyMemo = IMS IM.empty


instance Ord k => MemoKey (TreeKey k) where
   newtype MemoStruct (TreeKey k) v = TMS (M.Map k v)
   newtype Memo (TreeKey k) v = TreeMemo (MemoStruct (TreeKey k) v -> (v, MemoStruct (TreeKey k) v))
   
   compute (TreeKey k) thunkV = makeMemo $ \(TMS m) ->
      case M.lookup k m of
         Nothing -> let
                       (computedV, TMS m') = runMemo thunkV (TMS m)
                    in 
                       (computedV , TMS $ M.insert k computedV m')
         Just memoizedV -> (memoizedV, TMS m)

   makeMemo = TreeMemo
   runMemo (TreeMemo m) = m
   emptyMemo = TMS M.empty


class Endomonad m where
   return :: a -> m a
   (>>=) :: m a -> (a -> m a) -> m a

instance (MemoKey k, Ord k) => Endomonad (Memo k) where
   return x = makeMemo $ \m -> (x,m)
   f >>= g = makeMemo $ \m -> let (v,m') = runMemo f m in
                              runMemo (g v) m'

fMemo :: Int -> Memo Int Int
fMemo 0 = return 0
fMemo n = n2 >>= (\x -> n3 >>= (\y -> n4 >>= (\z -> return $! max n (x+y+z))))
   where n2 = compute' (n `div` 2) fMemo
         n3 = compute' (n `div` 3) fMemo
         n4 = compute' (n `div` 4) fMemo

-- Ugly, hand-crafted solution. Works as expected.
-- Could be solved with an applicative functor.
fHandMemo' :: IM.IntMap Int -> Int -> (Int, IM.IntMap Int)
fHandMemo' m 0 = (0,m)
fHandMemo' m n = (res, mOut)
   where (two,   m1) = checkMemo m (n `div` 2)
         (three, m2) = checkMemo m1 (n `div` 3)
         (four,  m3) = checkMemo m2 (n `div` 4)

         res = max n $ two + three + four
         mOut = IM.insert n res m3 

         checkMemo mp i = case IM.lookup i mp of
                             Nothing -> fHandMemo' mp i
                             Just e -> (e, mp)


fHandMemo :: Int -> Int
fHandMemo = fst . fHandMemo' IM.empty

fNoMemo :: Int -> Int
fNoMemo 0 = 0
fNoMemo n = max n $ fNoMemo (n `div` 2) +
                    fNoMemo (n `div` 3) +
                    fNoMemo (n `div` 4)