{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}

module Wolf where

import Prelude.Unicode
import Control.Monad
import Control.Arrow
import Data.List
import Data.List.Unicode

data Boat = West | East deriving (Eq, Show, Read, Enum, Ord)
data Object = Wolf | Goat | Cabbage | Fire | Stick | Farmer deriving (Eq, Show, Read, Enum, Ord)
type Objects = ([Object], [Object])
type Constraint = GameState -> Bool

type GameState = (Objects, Boat)
type Plan = [GameState]
type PlanningStep = (PreviousStates,[Plan])
type PreviousStates = [GameState]

-- Helper functions
powerset :: [a] → [[a]]
powerset = filterM (const [False,True])
chooseBetween :: Int → Int → [a] → [[a]]
chooseBetween n m = filter (\p -> length p >= n ∧ length p <= m) ∘ powerset
foldM1 :: Monad m ⇒ (a → a → m a) → [a] → m a
foldM1 f = uncurry (foldM f) ∘ (head &&& tail)
boundedIterate :: (b → Maybe ([a],b)) → b → [a]
boundedIterate f s = concat $ unfoldr f s


-- |Nondeterminisitically moves between 1 and n items via boat to the other side,
--  always, taking the Farmer.
nextMove :: Int -> GameState → [GameState]
nextMove boatSize x =
   case x of
      ((w,e),West) → liftM (,East) (transport w e)
      ((w,e),East) → liftM (\(e',w') → ((w',e'),West)) (transport e w)
   where transport from to = do passengers ← chooseBetween 1 boatSize from
                                let from' = from \\ (Farmer:passengers)
                                    to' = nub $ Farmer:passengers ++ to
                                return (from', to')

-- |Nondeterministically makes one valid, non-redundant step in the plan.
--  If no steps are possible, Nothing is returned.
makeStep :: Constraint -> Int -> PlanningStep → Maybe ([Plan], PlanningStep)
makeStep constraintsFulfilled boatSize (previousStates,plans) =
   let newplans = do (laststep:oldsteps) ← plans
                     next ← nextMove boatSize laststep
                     guard $ constraintsFulfilled next
                     guard $ next ∉ previousStates
                     return $ next:laststep:oldsteps
       res = (previousStates ++ map head newplans, newplans)
   in if null newplans then Nothing else Just (newplans, res)

-- |Merges multiple constraints into one.
constraints :: [Constraint] → Constraint
constraints xs obj = all ($ obj) xs

-- |Returns true iff all items have arrived on the eastern side.
goalState :: GameState → Bool
goalState (([],_),East) = True
goalState _             = False

-- |Solves a problem and returns the list of possible plans.
doPlan :: Constraint -> Int -> GameState -> [Plan]
doPlan c bs begin = solutions
   where makeStep' = makeStep c bs
         solutions = do
            plan <- boundedIterate makeStep' ([begin], [[begin]])
            guard $ (goalState ∘ head) plan
            return plan

-- |Prints a plan to the console.
printPlan :: Plan → IO ()
printPlan p = void $ foldM1 printStep $ reverse p
   where printStep :: GameState -> GameState -> IO GameState
         printStep (prev, b1) (cur,b2) = do

            let dir = case (b1,b2) of (East,West) → putStr " <== "
                                      _           → putStr " ==> "
                w = putStr $ show $ fst prev
                e = putStr $ show $ snd prev
                movement = putStr $ show $ fst prev ∆ fst cur
            sequence_ [w,dir,movement,dir,e, putStrLn ""]
            return (cur,b2)




problem1 :: [Plan]
problem1 = doPlan constraints1 boatSize1 begin1
problem2 :: [Plan]
problem2 = doPlan constraints2 boatSize2 begin2

boatSize1 :: Int
boatSize1 = 1
boatSize2 :: Int
boatSize2 = 2

begin1 :: GameState
begin1 = (([Wolf, Goat, Cabbage, Farmer], []), West)

begin2 :: GameState
begin2 = (([Wolf, Goat, Cabbage, Stick, Fire, Farmer], []), West)

wolfGoatNotAlone :: Constraint
wolfGoatNotAlone ((w,e),_) = c' w ∧ c' e
   where c' set | Wolf ∈ set ∧ Goat ∈ set ∧ Farmer ∉ set = False
                | otherwise                              = True

cabbageGoatNotAlone :: Constraint
cabbageGoatNotAlone ((w,e),_) = c' w ∧ c' e
   where c' set | Goat ∈ set ∧ Cabbage ∈ set ∧ Farmer ∉ set = False
                | otherwise                                 = True

fireStickNotAlone :: Constraint
fireStickNotAlone ((w,e),_) = c' w ∧ c' e
   where c' set | Fire ∈ set ∧ Stick ∈ set ∧ Farmer ∉ set = False
                | otherwise                               = True

wolfStickNotAlone :: Constraint
wolfStickNotAlone ((w,e),_) = c' w ∧ c' e
   where c' set | Wolf ∈ set ∧ Stick ∈ set ∧ Farmer ∉ set = False
                | otherwise                               = True

farmerWithBoat :: Constraint
farmerWithBoat ((w,_),West) = Farmer ∈ w
farmerWithBoat ((_,e),East) = Farmer ∈ e

constraints1 :: GameState → Bool
constraints1 = constraints [wolfGoatNotAlone,
                            cabbageGoatNotAlone,
                            farmerWithBoat]

constraints2 :: GameState → Bool
constraints2 = constraints [wolfGoatNotAlone,
                            cabbageGoatNotAlone,
                            fireStickNotAlone,
                            wolfStickNotAlone,
                            farmerWithBoat]

