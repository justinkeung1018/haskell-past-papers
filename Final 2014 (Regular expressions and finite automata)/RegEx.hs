module RegEx where

import Data.Maybe
import Data.List

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int

data Label = C Char | Eps
           deriving (Eq, Ord, Show)

type Transition = (State, State, Label)

type Automaton = (State, [State], [Transition])

--------------------------------------------------------
-- showRE - this may be useful for testing

showRE :: RE -> String
showRE (Seq re re')
  = showRE re ++ showRE re'
showRE (Alt re re')
  = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)
  = showRE' re ++ "*"
showRE (Plus re)
  = showRE' re ++ "+"
showRE (Opt re)
  =  showRE' re ++ "?"
showRE re
  = showRE' re

showRE' Null
  = ""
showRE' (Term c)
  = [c]
showRE' (Alt re re')
  = showRE (Alt re re')
showRE' re
  = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Part I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: There is exactly one occurrence of the item being looked up.
lookUp k
  = fromJust . lookup k

simplify :: RE -> RE
simplify Null
  = Null
simplify (Term c)
  = Term c
simplify (Seq e e')
  = Seq (simplify e) (simplify e')
simplify (Alt e e')
  = Alt (simplify e) (simplify e')
simplify (Rep e)
  = Rep (simplify e)
simplify (Plus e)
  = Seq e (Rep e)
simplify (Opt e)
  = Alt e Null

--------------------------------------------------------
-- Part II

startState :: Automaton -> State
startState (startState, _, _)
  = startState
terminalStates :: Automaton -> [State]
terminalStates (_, terminalStates, _)
  = terminalStates
transitions :: Automaton -> [Transition]
transitions (_, _, transitions)
  = transitions
isTerminal :: State -> Automaton -> Bool
isTerminal s a
  = s `elem` terminalStates a

transitionsFrom :: State -> Automaton -> [Transition]
transitionsFrom s a
  = filter (\(f, _, _) -> f == s) (transitions a)

labels :: [Transition] -> [Label]
labels tns
  = nub [l | (_, _, l) <- tns, l /= Eps]

accepts :: Automaton -> String -> Bool
accepts a
  = accepts' (startState a)
  where
    accepts' s str
      | isTerminal s a && null str = True
      | otherwise                  = any (try str) (transitionsFrom s a)
      where
        try str (_, t, Eps)
          = accepts' t str
        try "" _
          = False
        try (c' : cs') (_, t, C c)
          = c' == c && accepts' t cs'

--------------------------------------------------------
-- Part III

makeNDA :: RE -> Automaton
makeNDA re
  = (1, [2], sort transitions)
  where
    (transitions, k) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int)
make Null m n k
  = ([(m, n, Eps)], k)
make (Term c) m n k
  = ([(m, n, C c)], k)
make (Seq r1 r2) m n k
  = ((k, k + 1, Eps) : t1s ++ t2s, k2)
  where
    (t1s, k1) = make r1 m k (k + 2)
    (t2s, k2) = make r2 (k + 1) n k1
make (Alt r1 r2) m n k
  = (es ++ t1s ++ t2s, k2)
  where
    es = [(m, k, Eps), (m, k + 2, Eps), (k + 1, n, Eps), (k + 3, n, Eps)]
    (t1s, k1) = make r1 k (k + 1) (k + 4)
    (t2s, k2) = make r2 (k + 2) (k + 3) k1
make (Rep r) m n k
  = (es ++ ts, k')
  where
    es = [(m, k, Eps), (m, n, Eps), (k + 1, k, Eps), (k + 1, n, Eps)]
    (ts, k') = make r k (k + 1) (k + 2)

--------------------------------------------------------
-- Part IV

type MetaState = [State]

type MetaTransition = (MetaState, MetaState, Label)

getFrontier :: State -> Automaton -> [Transition]
getFrontier s a
  | isTerminal s a = [(s, s, Eps)]
  | otherwise      = concatMap go (transitionsFrom s a)
  where
    go (_, t, Eps)
      = getFrontier t a
    go tn
      = [tn]

groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions tns
  = [(l, [t | (_, t, l') <- tns, l == l']) | l <- labels tns]

makeDA :: Automaton -> Automaton
-- Pre: Any cycle in the NDA must include at least one non-Eps transition
makeDA nda
  = (r', sort terminal', sort ts')
  where
    (r, ms, ts) = makeDA' [startState nda] [] []
    states      = zip (reverse ms) [1..]
    r'          = lookUp r states
    terminal    = filter (\m -> any (`elem` m) (terminalStates nda)) ms
    terminal'   = map (`lookUp` states) terminal
    ts'         = map (\(f, t, l) -> (lookUp f states, lookUp t states, l)) ts

    makeDA' :: [State] -> [MetaState] -> [MetaTransition]
      -> (MetaState, [MetaState], [MetaTransition])
    makeDA' r ms ts
      | m `elem` ms = (m, ms, ts)
      | otherwise   = (m, ms', ts')
      where 
        frontier   = concatMap (`getFrontier` nda) r
        grouped    = groupTransitions frontier
        m          = (sort . nub . map (\(f, _, _) -> f)) frontier
        (ms', ts') = foldr makeDA'' (m : ms, ts) grouped
        
        makeDA'' (l, r) (ms, ts) 
          = (ms', (m, r', l) : ts')
          where
            (r', ms', ts') = makeDA' r ms ts

--------------------------------------------------------
-- Test cases

reFigure, re1, re2, re3, re4, re5 :: RE
reFigure
  = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1
  = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2
  = Seq (Term 'x') (Rep (Term '\''))
re3
  = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4
  = Seq (Alt (Term 'a') Null) (Term 'a')
re5
  = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

nd, nd' :: Automaton
nd = (1,[4],[(1,2,C 'a'),(1,3,C 'b'),(2,3,Eps),(2,4,C 'c')])

nd' = (1,[4],[(1,2,Eps),(1,3,C 'a'),(2,4,C 'a'),(2,4,C 'b'),
              (3,4,C 'b'),(3,4,Eps)])

da :: Automaton
da = (0,[3],[(0,1,C 'a'),(0,2,C 'a'),(0,2,C 'b'),(1,2,C 'a'),
             (1,3,C 'b'),(2,2,C 'a'),(2,1,C 'a'),(2,3,C 'b')])

re :: RE
re = Seq (Alt (Term 'a') (Term 'b')) (Seq (Rep (Term 'a')) (Term 'b'))

ndaFigure, nda1, nda2, nda3, nda4, nda5 :: Automaton
daFigure, da1, da2, da3, da4, da5 :: Automaton
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],
     [(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1
  = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2
  = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2
  = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3
  = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3
  = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4
  = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5
  = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

