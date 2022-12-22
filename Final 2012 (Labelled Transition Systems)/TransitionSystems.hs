module TransitionSystems where

import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp k
  = fromJust . lookup k

states :: LTS -> [State]
states
  = nub . concatMap (\((f, t), _) -> [f, t])

transitions :: State -> LTS -> [Transition]
transitions state
  = filter (\((f, _), _) -> f == state)

alphabet :: LTS -> Alphabet
alphabet
  = nub . map snd

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions STOP
  = []
actions (Ref _)
  = []
actions (Prefix id p)
  = nub (id : actions p)
actions (Choice ps)
  = nub (concatMap actions ps)

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts trace defs@((_, start) : _)
  = accepts' trace start
  where
    accepts' [] _
      = True
    accepts' trace@(id : ids) process
      | STOP       <- process = False
      | Ref id'    <- process = accepts' trace (lookUp id' defs)
      | Prefix a p <- process = a == id && accepts' ids p
      | Choice ps  <- process = any (accepts' trace) ps

------------------------------------------------------
-- PART III

composeTransitions :: Transition -> Transition 
                  -> Alphabet -> Alphabet 
                  -> StateMap 
                  -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions t1@((s, t), a) t2@((s', t'), a') a1 a2 m
  | a == a'                     = [((from, lookUp (t, t') m), a)]
  | a `elem` a2 && a' `elem` a1 = []
  | a' `elem` a1                = [((from, lookUp (t, s') m), a)]
  | a `elem` a2                 = [((from, lookUp (s, t') m), a')]
  | otherwise                   = [((from, lookUp (t, s') m), a), 
                                   ((from, lookUp (s, t') m), a')]
  where
    from = lookUp (s, s') m
  
pruneTransitions :: [Transition] -> LTS
pruneTransitions ts
  = visit 0 []
  where
    visit s visited
      | s `elem` visited = []
      | otherwise        =  outgoing ++ rest
      where
        outgoing = transitions s ts
        rest = concatMap ((`visit` (s : visited)) . (snd . fst)) outgoing

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose lts1 lts2
  = (nub . pruneTransitions . filter notSentinel . concatMap outgoing) m
  where
    m = zip (cartesianProduct (states lts1) (states lts2)) [0..]
    notSentinel (_, a)
      = a /= "$" && a /= "$'"
    cartesianProduct xs ys
      = [(x, y) | x <- xs, y <- ys]
    outgoing ((s, s'), _)
      = concatMap (\(t1, t2) -> composeTransitions t1 t2 a1 a2 m) ts
      where
        a1 = "$" : alphabet lts1
        a2 = "$'" : alphabet lts2
        t1s = transitions s lts1
        t2s = transitions s' lts2
        ts
          | null t1s  = cartesianProduct [((s, s), "$")] t2s
          | null t2s  = cartesianProduct t1s [((s', s'), "$'")]
          | otherwise = cartesianProduct t1s t2s

------------------------------------------------------
-- PART V

--
-- NOT WORKING
-- 

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS defs
  = processDefsToLTS defs 0 []
  
processDefsToLTS :: [(Id, Process)] -> State -> [(Process, State)] -> [Transition]
processDefsToLTS [] _ _
  = []
processDefsToLTS ((id, p) : defs) n refs
  = lts ++ processDefsToLTS defs n' refs'
  where
    (lts, n', refs')
      | isNothing state = processToLTS p n ((Ref id, n) : refs)
      | otherwise       = processToLTS p n refs
    state = lookup (Ref id) refs

processToLTS :: Process -> State -> [(Process, State)] -> ([Transition], State, [(Process, State)])
processToLTS STOP n refs
  = ([], n, refs)
processToLTS p@(Ref id) n refs
  | isNothing state = ([((n, n + 1), id)], n + 1, (p, n) : refs)
  | otherwise       = ([((n, fromJust state), id)], n, refs)
  where
    state = lookup p refs
processToLTS p@(Prefix id p') n refs
  | isNothing state = (((n, n + 1), id) : ts, n', refs')
  | otherwise       = (((n, fromJust state), id) : ts, n', refs')
  where
    (ts, n', refs')
      | isNothing state = processToLTS p' (n + 1) ((p, n) : refs)
      | otherwise       = processToLTS p' n refs
    state = lookup p refs
processToLTS (Choice ps) n refs
  = choicesToLTS ps n refs
  where
    choicesToLTS [] n refs
      = ([], n, refs)
    choicesToLTS (p@(Prefix id p') : ps) n refs 
      = (ts ++ ts', n'', refs'')
      where
        (ts', n'', refs'') = choicesToLTS ps n' refs'
        (ts, n', refs')
          | isNothing state = processToLTS p' (n + 1) ((p, n) : refs)
          | otherwise       = processToLTS p' n refs
        state = lookup p refs
    
------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, userLTS', 
  makerUserLTS, makerUserLTS', pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

userLTS' -- Removed "use" action
  = [((0,1),"ready")]

makerUserLTS -- Reordered
  = [((0,2),"make"),((2,1),"ready"),((1,3),"make"),((1,0),"use"),((3,2),"use")]

makerUserLTS'
  = [((0,2),"make"),((2,1),"ready"),((1,3),"make")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS -- Reordered
  = [((0,3),"a"),((0,1),"d"),((3,4),"d"),((1,4),"a")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]
