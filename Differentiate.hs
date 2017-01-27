{-# LANGUAGE ViewPatterns #-}

module Differentiate where

import Data.Ratio
import qualified Data.Set as S

data Number =
   Arbitrary Rational
   | Euler
   | Pi
   deriving (Eq)

type VariableName = String

data Expr =
   Const Number
   | Var VariableName
   | Expr :+ Expr
   | Expr :- Expr
   | Expr :* Expr
   | Expr :/ Expr
   | Expr :^ Expr
   | Log Expr
   | Sin Expr
   | Cos Expr
   | Tan Expr
   | Arcsin Expr
   | Arccos Expr
   | Arctan Expr
   deriving (Eq)

-- |Decomposes a unary operator into operator and operand.
unaryExpr :: Expr -> Maybe (Expr -> Expr, Expr)
unaryExpr (Log a) = Just (Log, a)
unaryExpr (Sin a) = Just (Sin, a)
unaryExpr (Cos a) = Just (Cos, a)
unaryExpr (Tan a) = Just (Tan, a)
unaryExpr (Arcsin a) = Just (Arcsin, a)
unaryExpr (Arccos a) = Just (Arccos, a)
unaryExpr (Arctan a) = Just (Arctan, a)
unaryExpr _ = Nothing

-- |Decomposes a binary operator into operator and operands.
binaryExpr :: Expr -> Maybe (Expr -> Expr -> Expr, (Expr, Expr))
binaryExpr (a :+ b) = Just ((:+), (a,b))
binaryExpr (a :- b) = Just ((:-), (a,b))
binaryExpr (a :* b) = Just ((:*), (a,b))
binaryExpr (a :/ b) = Just ((:/), (a,b))
binaryExpr (a :^ b) = Just ((:^), (a,b))
binaryExpr _ = Nothing

-- partial implementation of Num.
instance Num Expr where
   Const (Arbitrary a) + Const (Arbitrary b) = Const $ Arbitrary (a + b)
   a + b = a :+ b

   Const (Arbitrary a) * Const (Arbitrary b) = Const $ Arbitrary (a * b)
   a * b = a :* b

   Const (Arbitrary a) - Const (Arbitrary b) = Const $ Arbitrary (a - b)
   a - b = a :- b

   fromInteger = Const . Arbitrary . (%1)

-- |Creates a constant out of a rational number.
constR :: Rational -> Expr
constR = Const . Arbitrary

   --todo: abs, signum (keep in mind the expr. tree has to be evaluated for this!)

-- |Returns True iff the variable does not occur in the expression.
isConst :: VariableName -> Expr -> Bool
isConst v (Var x) = v /= x
isConst _ (Const _) = True
isConst v (binaryExpr -> Just (_,(a,b))) = isConst v a && isConst v b
isConst v (unaryExpr -> Just (_,a)) = isConst v a

collectNames :: Expr -> S.Set VariableName
collectNames (Const _) = S.empty
collectNames (Var x) = S.singleton x
collectNames (binaryExpr -> Just (_,(a,b))) = collectNames a `S.union` collectNames b
collectNames (unaryExpr -> Just (_,a)) = collectNames a

-- |Returns a new variable name that does not occur in the expression.
freshName :: Expr -> VariableName
freshName x = head $ filter (flip S.notMember names) candidates
   where
      names = collectNames x
      candidates = map (("x_"++) . show) [0..]

-- |Replaces all occurrences of F with R in T.
replace
   :: Expr -- ^The subtree F to find in T.
   -> Expr -- ^The tree R should replace F in T.
   -> Expr -- ^The expression tree T.
   -> Expr -- ^The tree T wherein F has been replaced with R.
replace f r t | f == t = r
replace f r (binaryExpr -> Just (t,(a,b))) = t (replace f r a) (replace f r b)
replace f r (unaryExpr -> Just (t,t')) = t (replace f r t')

-- |Differentiate against a variable
d :: VariableName -> Expr -> Expr
-- constant rule
d v a | isConst v a = 0
-- sum rule
d v (f :+ g) = d v f :+ d v g
-- product rule
d v (f :* g) = (d v f :* g) :+ (f :* d v g)
-- quotient rule
d v (f :/ g) = ((d v f :* g) :- (f :* d v g)) :/ (g :^ 2)
-- power rule
d _ (Var _) = 1
d _ (Var x :^ Const a) = Const a :* (Var x :^ ((Const a) - 1))
-- exponential rules
d _ (Const Euler :^ Var x) = Const Euler :^ Var x
d _ (Const a :^ Var x) = (Const a :^ Var x) :* Log (Const a)
-- logarithmic rules
d _ (Log (Var x)) = 1 :/ Var x
-- trigonometric rules
d _ (Sin (Var x)) = Cos (Var x)
d _ (Cos (Var x)) = 1 :- Sin (Var x)
d _ (Tan (Var x)) = 1 :+ (Tan (Var x) :^ 2)
d _ (Arcsin (Var x)) = 1 :/ ((1 :- (Var x :^ 2)) :^ (constR $ 1%2))
d _ (Arccos (Var x)) = 0 :- (1 :/ ((1 :- (Var x :^ 2)) :^ (constR $ 1%2)))
d _ (Arctan (Var x)) = 1 :/ (1 :+ (Var x :^ 2))
-- chain rule
d v f@(unaryExpr -> Just (_,g)) = f' :* d v g
   where
      -- replace g with an alias in f.
      -- then we derive f => f' w.r.t. to the alias
      -- and lastly, we replace the alias with the original g
      -- in f'.
      fAliased = replace g (Var gAlias) f
      f' = replace (Var gAlias) g $ d gAlias fAliased
      gAlias = freshName f

-- |Re-writes an expression according to the following rules:
--
--  1. + and * are made left-associative.
--  2. x+0, 0+x, 1*x, x*1 are turned into x.
--  3. x*0, 0*x are turned into 0.
--  4. sin(0), cos(x/2) where x % (pi/2) == 0 are turned into 0.
--  5. cos()
simplify :: Expr -> Expr
simplify = undefined

-- |Starts at the root of a tree and descends as long as the nodes or binary
--  and fulfil a criterion. These nodes are put into left-associative order
--  (meaning the tree will be nested to the right).
--  Subtrees whose nodes do not fulfil the criterion will be treated as
--  black boxes and be left as-is.
makeLAssociative
   :: (Expr -> Bool) -- ^The criterion. E.g. "is an addition".
   -> (Expr -> Expr -> Expr) -- ^The node type to use in the result tree.
   -> Expr
   -> Expr
makeLAssociative f node = mkTree . collectValues
   where
      -- Collects the values of a tree.
      -- The nodes must be binary and fulfil the criterion.
      -- The nodes are traversed left-to-right.
      collectValues n@(binaryExpr -> Just (_,(l,r)))
         | f n = collectValues l ++ collectValues r
         | otherwise = [n]
      collectValues n = [n]

      -- |Make a left-associative tree out of a list of nodes.
      mkTree [x,y] = node x y
      mkTree (x:ys) = node x (mkTree ys)


{-

f(g(x)) = f'(g(x)) * g'(x)

2^(x²) => f=2^[_]   =>  ^
          g=x²         / \
                      2   ^
                         / \
                        x   2


x²^x   =>  ^
          / \
         ^   x
        / \
       x   2

g(x)    = x²
f(g(x)) = [ ]^x

g'(x) = 2x
f'(y) = (y^x)' = x*y^(x-1) = x*(x²^(x-1))

a*x^r =

   0 * x^r     +      a * r * x^(r-1)

-}

--make log into log with a base; special case with pattern synonym for natural log
--log_a(x) = 1 / (x*ln(a))

