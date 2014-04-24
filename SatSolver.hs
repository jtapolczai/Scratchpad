{-# LANGUAGE FlexibleInstances #-}

-- A naive, monadic SAT-solver.
module SatSolver where

import Data.Maybe
import Data.List
import Control.Applicative

data Formula = And [Formula]
               | Or [Formula]
               | Neg Formula
               | Atom String
               | Constant Bool
   deriving (Eq, Ord)

data Assignment = Ass{runAss::[(String, Bool)]}
   deriving (Eq, Show, Read, Ord)

instance Show Formula where
   show (And fs) = "(" ++ intercalate " ^ " (map show fs) ++ ")"
   show (Or fs) = "(" ++ intercalate " v " (map show fs) ++ ")"
   show (Neg (Atom s)) = '-' : s
   show (Neg (Constant c)) = '-' : show c
   show (Neg f) = "(-" ++ show f ++ ")"
   show (Atom s) = s
   show (Constant c) = show c

-- |Evaluates a formula under an assigments.
{-apply :: Formula -> Assignment -> Bool
apply (And fs) ass = all (`apply` ass) fs
apply (Or fs) ass = any (`apply` ass) fs
apply (Neg f) ass = not $ apply f ass
apply (Atom s) ass = fromJust $ lookup s (runAss ass)
apply (Constant c) _ = c-}

-- |Returns the set of all occurring variables in a formula.
vars :: Formula -> [String]
vars = nub . vars'
   where vars' (And fs) = concatMap vars fs
         vars' (Or fs) = concatMap vars fs
         vars' (Neg f) = vars f
         vars' (Atom s) = [s]
         vars' (Constant _) = []

-- |Evaluates a formula under an assigments.
--  Applicative version. (@f = (Assignment ->)@)
apply :: Formula -> Assignment -> Bool
apply (And fs) = all <$> flip apply <*> pure fs
apply (Or fs) = any <$> flip apply <*> pure fs
apply (Neg f) = not <$> apply f
apply (Constant c) = pure c
apply (Atom s) = fromJust . lookup s . runAss

satisfyingAssignments :: Formula -> [Assignment]
satisfyingAssignments f = 
   do ass <- mapM (\v -> [(v,False), (v, True)]) (vars f)
      let ass' = Ass ass
      if apply f ass' then return ass'
                     else []

satSolver :: Formula -> Maybe Assignment
satSolver = listToMaybe . satisfyingAssignments

f1 :: Formula
f1 = And [Or [Atom "a", Atom "b", Neg $ Atom "c"],
          Or [Neg $ Atom "a", Atom "c", Atom "d"],
          Or [Neg $ Atom "b", Neg $ Atom "d"]]

f2 :: Formula
f2 = And [Or [Atom "a", Atom "b", Neg $ Atom "c"],
          Or [Neg $ Atom "a", Atom "c", Atom "d"],
          Or [Neg $ Atom "b", Neg $ Atom "d"],
          Neg $ Atom "b",
          Atom "b"]

print' :: Show a => [a] -> IO ()
print' = mapM_ print