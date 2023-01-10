module SAT where

import Data.List
import Data.Maybe
import Data.Tuple

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp x
  = fromJust . lookup x

-- 3 marks
vars :: Formula -> [Id]
vars (Var v)
  = [v]
vars (Not f)
  = vars f
vars (And f1 f2)
  = sort (nub (vars f1 ++ vars f2))
vars (Or f1 f2)
  = sort (nub (vars f1 ++ vars f2))

-- 1 mark
idMap :: Formula -> IdMap
idMap f
  = zip (vars f) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (Or f1 f2))
  = And (toNNF (Not f1)) (toNNF (Not f2))
toNNF (Not (And f1 f2))
  = Or (toNNF (Not f1)) (toNNF (Not f2))
toNNF (Not (Not f))
  = toNNF f
toNNF (And f1 f2)
  = And (toNNF f1) (toNNF f2)
toNNF (Or f1 f2)
  = Or (toNNF f1) (toNNF f2)
toNNF f
  = f

-- 3 marks
toCNF :: Formula -> CNF
toCNF
  = toCNF' . toNNF
  where
    toCNF' (And f1 f2)
      = And (toCNF' f1) (toCNF' f2)
    toCNF' (Or f1 f2)
      = distribute (toCNF' f1) (toCNF' f2)
    toCNF' f
      = f

-- 4 marks
flatten :: CNF -> CNFRep
flatten f
  = flatten' f
  where
    ids = idMap f
    flatten' (Var v)
      = [[lookUp v ids]]
    flatten' (Not (Var v))
      = [[negate (lookUp v ids)]]
    flatten' (And f1 f2)
      = flatten' f1 ++ flatten' f2
    flatten' (Or f1 f2)
      = [concat (flatten' f1 ++ flatten' f2)]

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits rep
  | null us   = (rep, [])
  | otherwise = (rep'', u : us')
  where
    us           = [cl | [cl] <- rep]
    (u : _)      = us
    rep'         = [filter (/= negate u) cl | cl <- rep, u `notElem` cl]
    (rep'', us') = propUnits rep'

-- 4 marks
dp :: CNFRep -> [[Int]]
dp rep
  | null rep'     = [us]
  | any null rep' = []
  | otherwise     = map (us ++) (tUnits ++ fUnits)
  where
    (rep', us)     = propUnits rep
    ((v : _) : _)  = rep'
    tUnits         = dp ([v] : rep')
    fUnits         = dp ([-v] : rep')
        
--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat f
  = (map sort . concatMap (addImp . map convert) . (dp . flatten . toCNF)) f
  where
    addImp asgn
      | null impVars = [asgn]
      | otherwise    = [fImp, tImp]
      where
        impVars = vars f \\ map fst asgn
        fImp    = map (, False) impVars ++ asgn
        tImp    = map (, True) impVars ++ asgn
    convert u
      | u > 0     = (lookUp u ids, True)
      | otherwise = (lookUp (negate u) ids, False)
      where
        ids = map swap (idMap f)