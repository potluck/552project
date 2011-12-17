-- Thanat Owlarn (towlarn@)
-- Pulak Mittal (pulak@)
-- CIS 552 Hw6

{-# OPTIONS -Wall -fwarn-tabs #-} 
module Sat where

import Data.Map (Map)
import qualified Data.Map as Map


import Test.QuickCheck
import Test.HUnit

import Control.Monad

-- | An expression in CNF, the conjunction of clauses
newtype CNF = CNF [ Clause ] deriving (Eq, Ord, Show)
unCNF :: CNF -> [ Clause ]
unCNF (CNF cs) = cs

-- | A clause -- the disjunction of a number of literals
type Clause = [ Lit ]

-- | A literal, either a positive or negative variable
type Lit    = Int 

-- | invert the polarity of a literal
invert :: Lit -> Lit
invert a = negate a

example_formula :: CNF
example_formula = CNF [[1],[2,-1]]

example_assignment :: Map Lit Bool
example_assignment = Map.fromList [(1, True), (2, True)]

valid :: Map Lit Bool  -> Bool
valid m = Map.foldrWithKey (\ lit b1 val -> 
                              case Map.lookup (invert lit) m of 
                                 Just b2 -> b1 == not b2 && val
                                 Nothing -> True && val) True m

test1 :: Test
test1 =  valid 
   (Map.fromList ( [(-2,True),(-1,True),(2,True)])) ~?= False

test_interp :: Test
test_interp = TestList [ 
  interp example_assignment example_formula ~?= True, 
  interp (Map.fromList [(1,False)]) example_formula ~?= False
  ]

interp :: (Map Lit Bool) -> CNF -> Bool
interp m (CNF f) = all (any (findLit m)) f where
  findLit m' k = 
     case (Map.lookup k m', Map.lookup (invert k) m') of
          (Just b1, Just b2) | b1 == not b2 -> b1
          (Just _, Just  _)  -> error "invalid map"
          (Just b, Nothing)  -> b
          (Nothing, Just b)  -> not b
          (Nothing, Nothing) -> True  

dpll :: CNF -> Maybe (Map Lit Bool)
dpll (CNF x) = aux x Map.empty where
  aux :: [Clause] -> Map Lit Bool -> Maybe (Map Lit Bool)
  aux cnf m -- m is the current state of the map
    | not $ valid m = Nothing
    | null cnf     = Just m
    | any null cnf = Nothing  
    | otherwise    = case l of
      Nothing -> aux [] (Map.union m m4)
      Just l' -> case (r1, r2) of 
        (Nothing, Nothing) -> Nothing
        (Just _, _) -> r1 -- only r1 matches or both match
        (Nothing, _) -> r2
        where
          f1 = filter_cnf cnf'' [l']
          f2 = filter_cnf cnf'' [(-l')]
          r1 = aux (unCNF f1) (Map.insert l' (True) (Map.union m m4))
          r2 = aux (unCNF f2) (Map.insert l' (False) (Map.union m m4))
      where
      (m2, cnf') = unitPropagate (CNF cnf)
      (m3, cnf'') = pureLitAssign cnf'
      m4 = Map.union m2 m3
      l = choose_literal cnf''
  
  choose_literal (CNF cnf)
    | all null cnf = Nothing
    | otherwise = Just $ head $ concat cnf

-- | Given a list of literals, create the trivial assignment 
-- that satisfies that list (if one exists). 
satisfy :: Clause -> Maybe (Map Lit Bool)
satisfy []     = Nothing
satisfy (l:ls) = Just $ Map.fromList ((l, l>0):(createFalses ls)) where
  createFalses [] = []
  createFalses x  = map (\ a -> if (abs a) == (abs l) then (l, l>0) else (a,False)) x
  
satisfy_test :: Test
satisfy_test = TestList [
  satisfy [2,-1] ~?= Just (Map.fromList [(2,True),(-1,False)]),
  satisfy [1,-1] ~?= Just (Map.fromList [(1,True),(1,True)]),
  satisfy [1,3,1] ~?= Just (Map.fromList [(1,True),(3,False)]),
  satisfy [] ~?= Nothing
  ]

-- | If a propositional variable occurs with only one polarity in the
-- formula, it is called pure. Pure literals can always be assigned in
-- a way that makes all clauses containing them true. Thus, these
-- clauses do not constrain the search anymore and can be deleted. 
-- This function collects all pure literals from the formula and 
-- returns the assignment paired with the refactored formula 
-- that reflects that assignment.
pureLitAssign :: CNF -> (Map Lit Bool, CNF)
pureLitAssign (CNF cnf) = (pure_map, filtr_cnf) where
  pure_map = Map.fromList $ map (\ y -> (y,True)) pures
  pures = filter (\ x -> (-x) `notElem` cc) cc
  cc    = concat cnf
  
  -- we want all clauses that do NOT contain elements in 'pures'
  filtr_cnf = CNF $ filter no_pures cnf
  
  no_pures [] = True
  -- we would use foldr here, but the partial application of notElem does not
  -- typecheck properly. notElem :: Eq a => a -> [a] -> Bool
  no_pures (l:ls) = l `notElem` pures && (no_pures ls)

test_pure :: Test
test_pure = TestList[
  (pureLitAssign $ CNF [[1],[2,-1]]) ~?= (Map.fromList [(2,True)], CNF [[1]]),
  
  (pureLitAssign $ CNF [[1],[2,-1],[2,5,1],[-5,3]]) ~?= 
  (Map.fromList [(2,True),(3,True)],CNF [[1]]),

  (pureLitAssign $ CNF [[1],[2,-1],[2,5,1],[-5,3,1],[4]]) ~?= 
  (Map.fromList [(2,True),(3,True),(4,True)],CNF [[1]]),

  (pureLitAssign $ CNF [[1],[2,-1],[2,5,1],[-5,3,1],[1]]) ~?=
  (Map.fromList [(2,True),(3,True)],CNF [[1],[1]]),

  (pureLitAssign $ CNF [[1],[2,-1],[2,5,1],[-5,3,1],[1,-2]]) ~?=
  (Map.fromList [(3,True)],CNF [[1],[2,-1],[2,5,1],[1,-2]]),

  (pureLitAssign $ CNF [[1],[2,-1],[2,5,1],[-5,3,1],[1,5]]) ~?=
  (Map.fromList [(2,True),(3,True)],CNF [[1],[1,5]])
  ]

singleton :: [a] -> Bool
singleton [] = False
singleton [_] = True
singleton (_:_) = False

-- | If a clause is a unit clause, i.e. it contains only a single
-- unassigned literal, this clause can only be satisfied by assigning
-- the necessary value to make this literal true. This function collects
-- all unit clauses from the formula and returns the assignment paired 
-- with the refactored formula that reflects that assignment.
unitPropagate :: CNF -> (Map Lit Bool, CNF)
unitPropagate (CNF cnf) = (units_map, filter_cnf (CNF cnf) units) where
  units = concat (filter singleton cnf)
  units_map = Map.fromList $ map (\l -> (l,True)) units
  

-- Removes all satisfied clauses and remove each literal from
-- a clause whose negation is an element of 'lits'
filter_cnf :: CNF -> [Lit] -> CNF
filter_cnf (CNF cnf) lits = CNF $ map remove_literal $ filter no_lits cnf where
  no_lits [] = True  
  no_lits (l:ls) = l `notElem` lits && (no_lits ls)  
  remove_literal  = filter (\x -> (-x) `notElem` lits) 
    
test_unit :: Test
test_unit = TestList[
  (unitPropagate $ CNF [[1],[1,2]]) ~?=
  (Map.fromList [(1,True)],CNF []),
  
  (unitPropagate $ CNF [[1],[-1,2]]) ~?=
  (Map.fromList [(1,True)],CNF [[2]]),
  
  (unitPropagate $ CNF [[1],[-1,1]]) ~?=
  (Map.fromList [(1,True)],CNF []),
  
  (unitPropagate $ CNF [[-1],[-1,1]]) ~?=
  (Map.fromList [(-1,True)],CNF []),
  
  -- this will need to be checked with a call to valid
  (unitPropagate $ CNF [[1],[-1]]) ~?=
  (Map.fromList [(1,True),(-1,True)],CNF []),
  
  (unitPropagate $ CNF [[1],[2],[-1,-2]]) ~?=
  (Map.fromList [(1,True),(2,True)],CNF [[]])
  ]
            
prop_dpll :: CNF -> Property
prop_dpll c = 
  
  case dpll c of 
    Just m -> if valid m then
       (property (interp m c))
      else property False
    Nothing ->  (property True)  
    
arblit :: Gen Lit
arblit = arbitrary :: Gen Int

arblit' :: Gen Lit
arblit' = oneof [ elements [1..20],
                  elements [(-20)..(-1)] ]

arbclause :: Gen Clause
arbclause = frequency [ (1, return [])
                      , (15, liftM2 (:) arblit' arbclause)] 
            
arbcnf :: Gen CNF
arbcnf = frequency [ (1, return $ CNF [])
                   , (15, liftM2 cnfcons arbclause arbcnf)] where  
  cnfcons x (CNF y) = CNF (x:y)
                           
-- sample arbcnf

instance Arbitrary CNF where         
  arbitrary = arbcnf
  
qc :: IO ()
qc = quickCheck prop_dpll