module Alloc where

import Data.Maybe
import Data.List
import Data.Tuple

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count x 
  = length . filter (== x)

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (ns, es)
  = map (\n -> (n, count n fs + count n ts)) ns
  where
    (fs, ts) = unzip es

neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (_, es)
  = [f | (f, t) <- es, t == n] ++ [t | (f, t) <- es, f == n]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n (ns, es)
  = (delete n ns, filter (\(f, t) -> f /= n && t /= n) es)

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([], _)
  = []
colourGraph maxC g@(ns, es)
  = (n, c) : cMap
  where
    (_, n)  = minimum (map swap (degrees g))
    g'      = removeNode n g
    cMap    = colourGraph maxC g'
    c       = head (([1..maxC] \\ map (`lookUp` cMap) (neighbours n g)) ++ [0])

------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap ig
  = ("return", "return") : map mapId ig
  where
    mapId (v, 0)
      = (v, v)
    mapId (v, n)
      = (v, 'R' : show n)

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments args idMap
  = map (\arg -> Assign (lookUp arg idMap) (Var arg)) args

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap.
renameExp (Const c) _
  = Const c 
renameExp (Var v) idMap
  = Var (lookUp v idMap)
renameExp (Apply op e1 e2) idMap
  = Apply op (renameExp e1 idMap) (renameExp e2 idMap)

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock b idMap
  = filter (not . isRedundant) (map renameStatement b)
  where
    renameStatement (Assign v e)
      = Assign (lookUp v idMap) (renameExp e idMap)
    renameStatement (If ePred bThen bElse)
      = If ePred' bThen' bElse'
      where
        ePred' = renameExp ePred idMap
        bThen' = renameBlock bThen idMap
        bElse' = renameBlock bElse idMap
    renameStatement (While ePred bWhile)
      = While (renameExp ePred idMap) (renameBlock bWhile idMap)

    isRedundant (Assign v (Var v'))
      | v == v' = True
    isRedundant _
      = False

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG lvss
  = (nub (concat lvss), nub ((map order . concatMap edges) lvss))
  where
    edges []
      = []
    edges (lv : lvs)
      = zip (repeat lv) lvs ++ edges lvs
    order (n, n')
      = (min n n', max n n')

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars cfg
  = livePass (replicate (length cfg) [])
  where
    livePass lvs
      | res == lvs = res
      | otherwise  = livePass res
      where
        res = map live cfg
        live ((def, use), succs)
          = use `union` delete def (nub (concatMap (lvs !!) succs))

vars :: Exp -> [Id]
vars (Const _)
  = []
vars (Var v)
  = [v]
vars (Apply op e1 e2)
  = nub (vars e1 ++ vars e2)

buildCFG :: Function -> CFG
buildCFG (_, _, body)
  = fst (buildBlock body 0)
  where
    -- Given 1) the statement, and 
    --       2) the line number of that statement,
    -- returns 1) the CFG for that statement, and
    --         2) the line number after the statement
    buildStatement :: Statement -> Int -> (CFG, Int)
    buildStatement (Assign v e) n
      | v == "return" = ([((v, vars e), [])], n + 1)
      | otherwise     = ([((v, vars e), [n + 1])], n + 1)
    buildStatement (If ePred bThen bElse) n
      = (predDUS : thenDUS ++ elseDUS, nElse)
      where
        predDUS          = (("_", vars ePred), [n + 1, nThen])
        (thenDUS, nThen) = buildBlock bThen (n + 1)
        (elseDUS, nElse) = buildBlock bElse nThen
    buildStatement (While ePred bWhile) n
      = (predDUS : rest ++ [(defUse, [n])], nWhile)
      where
        predDUS               = (("_", vars ePred), [n + 1, nWhile])
        (whileDUS, nWhile)    = buildBlock bWhile (n + 1)
        (rest, [(defUse, _)]) = splitAt (length whileDUS - 1) whileDUS 

    -- Given 1) the block, and 
    --       2) the line number of the first statement in the block,
    -- returns 1) the CFG for that block, and
    --         2) the line number after the block
    buildBlock :: Block -> Int -> (CFG, Int)
    buildBlock [] n
      = ([], n)
    buildBlock (stm : b) n
      = (cfg ++ cfg', n'')
      where
        (cfg, n')   = buildStatement stm n
        (cfg', n'') = buildBlock b n'