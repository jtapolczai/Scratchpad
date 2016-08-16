module Pattern where

{-
Encode this imperative pattern:

def foo(x,y):
    x = f(x) if a(x)
    if c(x): 
        x = g(x)
    else:
        x = h(x)
    x = f(x)
    y = f(y) if a(y)
    x = g(x) if b(y)
    return [x,y]
-}

import Control.Arrow (first, second, (>>>))

if' :: (a -> Bool) -> (a -> a) -> (a -> a) -> a -> a
if' f ifB elseB v = if f v then ifB v else elseB v

cond :: (a -> a) -> (a -> Bool) -> a -> a
cond ifB f = if' f ifB id

a :: a -> Bool
a = undefined
b = undefined
c = undefined
f = undefined
g = undefined
h = undefined

foo :: a -> a -> [a]
foo = curry $   onX f `cond` ifX c
            >>> if' (ifX c) (onX g)
                          (onX h)
            >>> onX f
            >>> onY f `cond` ifY a
            >>> onX g `cond` ifY b
            >>> \(x,y) -> [x,y]
   where
      onX = first
      onY = second
      ifX f = fst . first f
      ifY f = snd . second f
