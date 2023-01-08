module BDD where

import Data.List
import Data.Maybe

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x 
  = fromJust . lookup x

checkSat :: BDD -> Env -> Bool
checkSat (rId, ns) env
  = checkSat' (lookUp rId ns)
  where
    checkSat' (i, fId, tId)
      | lookUp i env = eval tId
      | otherwise    = eval fId
      where
        eval 0
          = False
        eval 1
          = True
        eval id
          = checkSat' (lookUp id ns)

sat :: BDD -> [[(Index, Bool)]]
sat (rId, ns)
  = sat' rId
  where
    sat' 0
      = []
    sat' 1
      = [[]]
    sat' id
      = map ((i, False) :) (sat' fId) ++ map ((i, True) : ) (sat' tId)
      where
        (i, fId, tId) = lookUp id ns

------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim False))
  = Prim True
simplify (Not (Prim True))
  = Prim False
simplify (And (Prim True) (Prim True))
  = Prim True
simplify (And (Prim _) (Prim _))
  = Prim False
simplify (Or (Prim False) (Prim False))
  = Prim False
simplify (Or (Prim _) (Prim _))
  = Prim True
simplify e
  = e

restrict :: BExp -> Index -> Bool -> BExp
restrict (IdRef i') i b
  | i == i' = Prim b
restrict (Not e) i b
  = simplify (Not (restrict e i b))
restrict (And e1 e2) i b
  = simplify (And (restrict e1 i b) (restrict e2 i b))
restrict (Or e1 e2) i b
  = simplify (Or (restrict e1 i b) (restrict e2 i b))
restrict e _ _
  = e

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD
  = flip buildBDD' 2

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' (Prim b) _ []
  = if b then (1, []) else (0, [])
buildBDD' e id (i : is)
  = (id, (id, (i, fId, tId)) : fNodes ++ tNodes)
  where
    (fId, fNodes) = buildBDD' (restrict e i False) (2 * id) is
    (tId, tNodes) = buildBDD' (restrict e i True) (2 * id + 1) is

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD b is
  = (rId, nub ns)
  where
    (rId, ns) = buildROBDD' b 2 is []

-- First three parameters are the same as buildBDD'
-- Fourth parameter represents the nodes that have already been built
-- so we could check if the current node has already been built already
buildROBDD' :: BExp -> NodeId -> [Index] -> [BDDNode] -> BDD
buildROBDD' (Prim b) _ [] _
  = if b then (1, []) else (0, [])
buildROBDD' e id (i : is) built
  | sat fBDD == sat tBDD = fBDD
  | not (null shared)    = (sharedId, built) -- We are not adding new nodes
  | otherwise            = (id, (id, (i, fId, tId)) : fNodes ++ tNodes)
  where
    fBDD@(fId, fNodes) = buildROBDD' (restrict e i False) (2 * id) is built
    built'             = built ++ fNodes
    tBDD@(tId, tNodes) = buildROBDD' (restrict e i True) (2 * id + 1) is built'
    shared             = filter isShared built
    sharedId           = fst (head shared)
    isShared (_, (i', fId, tId))
      = i == i' && sat fBDD == sat (fId, built) && sat tBDD == sat (tId, built)

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
-- bdd3 = (5,[(5,(1,0,1))])
bdd3 = (2,[(2,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
-- bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
--            (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd5 = (2,[(4,(3,8,9)),(2,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
-- bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
--            (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd7 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])

sortBDD :: BDD -> BDD
sortBDD (rId, ns)
  = (rId, sort ns)

