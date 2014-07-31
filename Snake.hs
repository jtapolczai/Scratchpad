module Snake where

import Control.Monad
import Data.Array
import Data.Maybe
import Data.Ord
import Data.List
import Data.List.Split

data Cell = Snake | Empty | Apple deriving (Eq, Ord)

instance Show Cell where
   show Empty = " "
   show Snake = "x"
   show Apple = "o"

sk = Sk [(2,2),(2,3)] (2,2)

type Field = Array (Int,Int) Cell
data Snake = Sk [(Int,Int)] (Int, Int) deriving (Eq, Ord, Show)

data Dir = Up | Le | Ri | Do

mkField :: Snake -> Int -> Int -> Field
mkField (Sk cells _) cols rows = array rng empties // map (\i -> (i,Snake)) cells
   where
      rng = ((0,0),(rows, cols))
      empties = map (\i -> (i,Empty)) (range rng)

printField :: Field -> IO ()
printField = mapM_ f . splitWhen ((0==) . snd . fst) . assocs
   where f = mapM_ (putStr . show. snd) >=> const (putStrLn "")

moveSnake :: Dir-> Field -> Snake -> Maybe (Field, Snake)
moveSnake dir f (Sk ss h) = if conflict then Nothing else Just (f',sk')
   where f' = (f // [(last ss, Empty), (head', Snake)]) // [(last ss', Snake)]
         head' = cell $ head ss
         cell (y,x) = case dir of Up -> (y-1,x)
                                  Le -> (y,x-1)
                                  Ri -> (y,x+1)
                                  Do -> (y+1,x)
         sk' = Sk ss' (cell $ head ss)
         ss' = if f ! head' == Apple then head' : ss
               else head' : (init ss)

         conflict = f ! head' == Snake || x < 0 || y < 0 || y > maxy || y > maxx
            where x = snd head'
                  y = fst head'
                  maxy = fst $ snd $ bounds f
                  maxx = snd $ snd $ bounds f

main :: IO ()
main = undefined