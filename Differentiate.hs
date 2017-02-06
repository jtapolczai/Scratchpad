{-# LANGUAGE ViewPatterns #-}

module Differentiate where

import Data.List (foldl')
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

infixl 4 :+
infixl 4 :-
infixl 5 :*
infixl 5 :/
infixl 6 :^

-- Differentiation
-------------------------------------------------------------------------------

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
d v (Var x :^ a) | isConst v a = a :* (Var x :^ (a - 1))
-- exponential rules
d _ (Const Euler :^ Var x) = Const Euler :^ Var x
d _ (Const a :^ Var x) = Const a :^ Var x :* Log (Const a)
-- functional power rule
d v (f :^ g) = (f :^ g) :* (d v g :* Log f :+ g :* (d v f :/ f))
-- logarithmic rules
d _ (Log (Var x)) = 1 :/ Var x
-- trigonometric rules
d _ (Sin (Var x)) = Cos (Var x)
d _ (Cos (Var x)) = 1 :- Sin (Var x)
d _ (Tan (Var x)) = 1 :+ Tan (Var x) :^ 2
d _ (Arcsin (Var x)) = 1 :/ ((1 :- Var x :^ 2) :^ (constR $ 1%2))
d _ (Arccos (Var x)) = 0 :- (1 :/ ((1 :- Var x :^ 2) :^ (constR $ 1%2)))
d _ (Arctan (Var x)) = 1 :/ (1 :+ Var x :^ 2)
-- chain rule
d v f@(unaryExpr -> Just (_,g)) = dExpr g f :* d v g

-- |Derives against an expression @g@, not just against a variable name.
--  For the purposes of derivation, @g@ will be temporarily replaced
--  with a fresh variable @y@ and the expression will be derived against @y@.
dExpr
   :: Expr -- ^The expression against which to derive.
   -> Expr
   -> Expr
dExpr g f = replace (Var gAlias) g $ d gAlias fAliased
   where
      fAliased = replace g (Var gAlias) f
      gAlias = freshName f


-- Helpers
-------------------------------------------------------------------------------

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


-- Optimizations
-------------------------------------------------------------------------------

-- |Re-writes an expression according to the following rules:
--
--  1. + and * are made left-associative.
--  2. x+0, 0+x, 1*x, x*1 are turned into x.
--  3. x*0, 0*x are turned into 0.
simplify :: Expr -> Expr
simplify = go
   where
      go :: Expr -> Expr
      go (binaryExpr -> Just (n,(l,r))) = applyOpts $ n (simplify l) (simplify r)
      go (unaryExpr -> Just (n,n')) = applyOpts $ n (simplify n')
      go t = applyOpts t

      -- Applies all optimizations to a node.
      applyOpts x = foldl' (flip ($)) x optimizations

      optimizations = [makeLAssociative isAdd (:+),
                       makeLAssociative isMul (:*),
                       eliminateNeutralElems isAdd (isConst 0),
                       eliminateNeutralElems isMul (isConst 1),
                       eliminateZeroElems isMul (isConst 0)]

      isAdd (_ :+ _) = True
      isAdd _ = False

      isMul (_ :* _) = True
      isMul _ = False

      isConst n m@(Const _) = n == m

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

-- |Eliminates neutral elements from a node's children. If the root's
--  left or right child is a neutral element, the root is replaced by its
--  other child. This optimization performs no recursion.
eliminateNeutralElems
   :: (Expr -> Bool) -- ^Criterion for the node. E.g. "is an addition".
   -> (Expr -> Bool) -- ^Criterion for the neutral element. E.g. "is 0".
   -> Expr
   -> Expr
eliminateNeutralElems nodeF neutralF t@(binaryExpr -> Just (_,(l,r))) =
   if nodeF t then
      if neutralF l then l
      else if neutralF r then r
      else t
   else t
eliminateNeutralElems _ _ t = t

-- |Eliminates zero elements from a node's children. If the root's left or
--  right child is a zero, the whole tree is replaced by zero.
--  This optimization performs no recursion.
eliminateZeroElems
   :: (Expr -> Bool) -- ^Criterion for the node. E.g. "is a multiplication".
   -> (Expr -> Bool) -- ^Criterion for the zero element. E.g. "is 0".
   -> Expr
   -> Expr
eliminateZeroElems nodeF zeroF t@(binaryExpr -> Just (_,(l,r))) =
   if nodeF t then
      if zeroF l then l
      else if zeroF r then r
      else t
   else t
eliminateZeroElems nodeF zeroF t@(unaryExpr -> Just (_,n)) =
   if nodeF t && zeroF n then n else t
eliminateZeroElems _ _ t = t
