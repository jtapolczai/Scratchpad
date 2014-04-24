module Trees where

import Prelude hiding (lines)
import Data.List
import Data.Ord

data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a deriving (Eq, Read, Show)
data Dir = N | L | R

verticalScaling = 1

instance PrettyPrint Dir where
   ppr N = "+"
   ppr L = "\\"
   ppr R = "/"

class PrettyPrint a where
   ppr :: a -> String
   pprint :: a -> IO ()
   pprint = putStr . ppr

instance Show a => PrettyPrint (BinTree a) where
   ppr = showCharArray . (\t -> ppr' (rw t, 0) t) . weights
      where ppr' (y,x) (Leaf (v,_,_)) = putText (y,x) ('+':show v)
            ppr' (y,x) (Node (v,_,_) l r) = let
                  lline = goLeft (y,x) l
                  rline = goRight (y,x) r

                  lxy = (y+1+verticalScaling*rw l, x+1+verticalScaling*rw l)
                  rxy = (y-1-verticalScaling*lw r, x+1+verticalScaling*lw r)

                  this = putText (y,x) ('+':show v)
               in this ++ lline ++ rline ++ ppr' lxy l ++ ppr' rxy r

t1 = Leaf 'A'
t2 = Node 'B' (Leaf 'A') (Leaf 'C')
t3 = Node 'F' t2 (Leaf 'G')
t4 = Node 'H' t3 (Node 'M' (Node 'J' (Leaf 'I') (Node 'K' (Leaf 'J') (Leaf 'L'))) (Leaf 'N'))


lw (Leaf (_,l,_)) = l
lw (Node (_,l,_) _ _) = l
rw (Leaf (_,_,r)) = r
rw (Node (_,_,r) _ _) = r

weights (Leaf x) = Leaf (x,1,1)
weights (Node x l r) = Node (x, lw''+1, rw''+1) lw' rw'
   where lw' = weights l
         rw' = weights r

         lw'' = lw lw' + rw lw'
         rw'' = lw rw' + rw rw'

goLeft (y,x) n = map ((,) '\\') $ rasterArrow (y+1, x+1) (y+verticalScaling*rw n, x+verticalScaling*rw n)
goRight (y,x) n = map ((,) '/') $ rasterArrow (y-1, x+1) (y-verticalScaling*lw n, x+verticalScaling*lw n)
putText (y,x) t = zip t (zip (repeat y) [x..])

rasterArrow (x1,y1) (x2,y2) = if xd == 0 || yd == 0 then [(x1,y1)] else (x1,y1) : rasterArrow (x1+xd,y1+yd) (x2,y2)
   where xd = case compare x1 x2 of LT -> 1
                                    GT -> -1
                                    EQ -> 0

         yd = case compare y1 y2 of LT -> 1
                                    GT -> -1
                                    EQ -> 0

showCharArray :: [(Char, (Int,Int))] -> String
showCharArray = unlines . map (mkLine (-1)) . groupBy (grouping $ fst.snd) . sortBy (comparing snd)
   where mkLine prev xs@((c,(_,x)):t) = if prev+1 >= x then c : mkLine x t else ' ' : mkLine (prev+1) xs
         mkLine _ [] = []


grouping :: Eq a => (b -> a) -> b -> b -> Bool
grouping f x y = (f x) == (f y)