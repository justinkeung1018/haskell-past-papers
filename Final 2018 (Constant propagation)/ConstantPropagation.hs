module ConstantPropagation where

import Data.Maybe
import Data.List
import Data.Bifunctor

type Id = String

type Function = (Id, [Id], Block)

type Block = [Statement]

data Statement = Assign Id Exp |
                 If Exp Block Block |
                 DoWhile Block Exp 
               deriving (Eq, Show)

data Exp = Const Int | Var Id | Apply Op Exp Exp | Phi Exp Exp
         deriving (Eq, Show)

data Op = Add | Mul | Eq | Gtr 
        deriving (Eq, Show)

------------------------------------------------------------------------
-- Given functions to support the interpreter...

lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp i table
  = fromMaybe (error ("lookup failed on identifier: " ++ show i)) 
              (lookup i table) 

execFun :: Function -> [Int] -> State
execFun (name, args, p) vs
  = execBlock p (zip args vs)

------------------------------------------------------------------------
-- Part I

type State = [(Id, Int)]

update :: (Id, Int) -> State -> State
update (x, v) st
  = (x, v) : filter (\(x', _) -> x /= x') st

apply :: Op -> Int -> Int -> Int
apply Add x y
  = x + y
apply Mul x y
  = x * y
apply Eq x y
  | x == y    = 1
  | otherwise = 0
apply Gtr x y
  | x > y     = 1
  | otherwise = 0

eval :: Exp -> State -> Int
-- Pre: the variables in the expression will all be bound in the given state 
-- Pre: expressions do not contain phi instructions
eval (Const n) _
  = n
eval (Var x) st 
  = lookUp x st
eval (Apply op e1 e2) st
  = apply op (eval e1 st) (eval e2 st)

execStatement :: Statement -> State -> State
execStatement (Assign x e) st
  = update (x, eval e st) st
execStatement (If ePred bThen bElse) st
  | eval ePred st == 1 = execBlock bThen st
  | otherwise          = execBlock bElse st
execStatement dw@(DoWhile bDo ePred) st
  | eval ePred st' == 1 = execStatement dw st'
  | otherwise           = st'
  where
    st' = execBlock bDo st

execBlock :: Block -> State -> State
execBlock b st
  = foldl (flip execStatement) st b

------------------------------------------------------------------------
-- Given function for testing propagateConstants...

-- Converts a function in SSA form into its optimised SSA form...
applyPropagate :: Function -> Function
applyPropagate (name, args, body)
  = (name, args, propagateConstants body)

------------------------------------------------------------------------
-- PART II

foldConst :: Exp -> Exp
-- Pre: the expression is in SSA form
foldConst (Phi (Const c1) (Const c2))
  | c1 == c2 = Const c1
foldConst (Apply op (Const c1) (Const c2))
  = Const (apply op c1 c2)
foldConst (Apply Add v@(Var _) (Const 0))
  = v
foldConst (Apply Add (Const 0) v@(Var _))
  = v
foldConst e
  = e

sub :: Id -> Int -> Exp -> Exp
-- Pre: the expression is in SSA form
sub x v (Var x')
  | x == x' = Const v
sub x v (Apply op e1 e2)
  = foldConst (Apply op (sub x v e1) (sub x v e2))
sub x v (Phi e1 e2)
  = foldConst (Phi (sub x v e1) (sub x v e2))
sub _ _ e
  = e

-- Use (by uncommenting) any of the following, as you see fit...

type Worklist = [(Id, Int)]

scan :: Id -> Int -> Block -> (Worklist, Block)
scan v c b
  = bimap concat concat (unzip (map subStatement b))
  where
    subStatement :: Statement -> (Worklist, [Statement])
    subStatement (Assign x e)
      | x == "$return" = ([], [Assign x e'])
      | Const c' <- e' = ([(x, c')], [])
      | otherwise      = ([], [Assign x e'])
      where
        e' = sub v c e
    subStatement (If ePred bThen bElse)
      = (wlThen ++ wlElse, [If (sub v c ePred) bThen' bElse'])
      where
        (wlThen, bThen') = scan v c bThen
        (wlElse, bElse') = scan v c bElse
    subStatement (DoWhile bDo ePred)
      = (wlDo, [DoWhile bDo' (sub v c ePred)])
      where
        (wlDo, bDo') = scan v c bDo
      
-- scan :: (Exp -> Exp) -> Block -> (Exp -> Exp, Block)

propagateConstants :: Block -> Block
-- Pre: the block is in SSA form
propagateConstants b
  = propagateConstants' wlInit b
  where
    (wlInit, _) = scan "$INVALID" 0 b

    propagateConstants' :: Worklist -> Block -> Block
    propagateConstants' [] b
      = b
    propagateConstants' ((v, c) : wl) b
      = propagateConstants' (wl ++ wl') b'
      where
        (wl', b') = scan v c b
 
------------------------------------------------------------------------
-- Given functions for testing unPhi...

-- Applies unPhi to a given function...
applyUnPhi :: Function -> Function
applyUnPhi (name, args, body)
  = (name, args, unPhi body)

-- Combines propagation/folding and unPhi to convert a function from its
-- unoptimised SSA form to a final non-SSA form...
optimise :: Function -> Function
optimise (name, args, body)
  = (name, args, unPhi (propagateConstants body))

------------------------------------------------------------------------
-- PART III

unPhi :: Block -> Block
-- Pre: the block is in SSA form
unPhi ((If ePred bThen bElse) : (Assign v (Phi e1 e2)) : b)
  = If ePred bThen' bElse' : unPhi b
  where
    bThen' = unPhi bThen ++ [Assign v e1]
    bElse' = unPhi bElse ++ [Assign v e2]
unPhi ((DoWhile ((Assign v (Phi e1 e2)) : bDo) ePred) : b)
  = Assign v e1 : unPhi (DoWhile (unPhi bDo ++ [Assign v e2]) ePred : b)
unPhi (If ePred bThen bElse : b)
  = If ePred (unPhi bThen) (unPhi bElse) : unPhi b
unPhi (DoWhile bDo ePred : b)
  = DoWhile (unPhi bDo) ePred : unPhi b
unPhi (stm : b)
  = stm : unPhi b
unPhi []
  = []

------------------------------------------------------------------------
-- Part IV

type Ids = [(Id, Int)]

-- Retrieves all variable names in the block
-- Includes "$return"
varNames :: Block -> [Id]
varNames 
  = nub . varNames'
  where
    varNames' []
      = []
    varNames' (Assign x _ : b)
      = x : varNames' b
    varNames' (If _ bThen bElse : b)
      = varNames' bThen ++ varNames' bElse ++ varNames' b
    varNames' (DoWhile bDo _ : b) 
      = varNames' bDo ++ varNames' b

-- Concatenates the variable name and identifier number
ssaId :: (Id, Int) -> Id
ssaId (x, n)
  = x ++ show n

-- Given the variable and the current identifier number,
-- returns 1) a modified expression where all occurrences of the variable are
--            renamed,
--         2) the next available identifier number
-- renameExp :: (Id, Int) -> Exp -> (Exp, Int)
-- Pre: phi functions will not appear in any operator applications
-- renameExp (x, n) (Var v)
--   | x == v = (Var (ssaId (x, n)), n + 1)
-- renameExp (x, n) (Apply op (Apply op' e1 e2) e3)
--   = undefined
-- renameExp (x, n) (Apply op e1 e2)
--   = (Apply op e1' e2', n2)
--   where
--     (e1', n1) = renameExp (x, n) e1
--     (e2', n2) = renameExp (x, n1) e2
-- renameExp (x, n) (Phi e1 e2)
--   = (Phi e1' e2', n2)
--   where
--     (e1', n1) = renameExp (x, n) e1
--     (e2', n2) = renameExp (x, n1) e2
-- renameExp (_, n) e
--   = (e, n)

-- Given the variable and the current identifier number, returns 
-- a modified expression where all occurrences of the variable are renamed
renameExp :: (Id, Int) -> Exp -> Exp
-- Pre: phi functions do not need to be renamed
renameExp (x, n) (Var v)
  | x == v = Var (ssaId (x, n))
renameExp (x, n) (Apply op e1 e2)
  = Apply op (renameExp (x, n) e1) (renameExp (x, n) e2)
renameExp _ e
  = e

-- Given the variable and the current identifier number,
-- returns 1) a modified block where all occurrences of the variable are
--            renamed and phi functions are added where appropriate,
--         2) the next available identifier number
renameBlock :: (Id, Int) -> Block -> (Block, Int)
renameBlock (_, n) []
  = ([], n)

-- Nested operator applications
renameBlock (x, n) ((Assign x' (Apply op (Apply op' e1 e2) e3)) : b)
  = undefined
renameBlock (x, n) ((Assign x' (Apply op e1 (Apply op' e2 e3))) : b)
  = undefined

renameBlock (x, n) ((Assign x' e) : b)
  | x == x'   = (Assign (ssaId (x, n)) e' : bNew, nNew)
  | otherwise = (Assign x' e' : bSame, nSame)
  where
    e'             = renameExp (x, n - 1) e
    (bNew, nNew)   = renameBlock (x, n + 1) b
    (bSame, nSame) = renameBlock (x, n) b

renameBlock (x, n) ((If ePred bThen bElse) : b)
  -- No reassignments in both branches
  | n == nElse = (if' : bSame, nSame) 
  -- Otherwise, reassigned in at least one branch
  -- "Then" block contains return
  | "$return" `elem` varNames bThen = (if' : bSame, nSame) 
  -- "Else" block contains return
  | "$return" `elem` varNames bElse = (if' : bSame, nSame) 
  -- "Then" block is empty
  | null bThen = (if' : Assign xPhi (Phi (Var xInit) (Var xElse)) : bNew, nNew)
  -- "Else" block is empty
  | null bElse = (if' : Assign xPhi (Phi (Var xThen) (Var xInit)) : bNew, nNew)
  -- Both branches are non-empty
  | otherwise = (if' : Assign xPhi (Phi (Var xThen) (Var xElse)) : bNew, nNew)
  where
    ePred'          = renameExp (x, n) ePred
    (bThen', nThen) = renameBlock (x, n) bThen
    (bElse', nElse) = renameBlock (x, nThen) bElse
    if'             = If ePred' bThen' bElse'
    xInit           = ssaId (x, n - 1)
    xThen           = ssaId (x, nThen - 1)
    xElse           = ssaId (x, nElse - 1)
    xPhi            = ssaId (x, nElse)
    (bSame, nSame)  = renameBlock (x, n) b
    (bNew, nNew)    = renameBlock (x, nElse + 1) b

renameBlock (x, n) (DoWhile bDo ePred : b)
  -- Variable is updated in the loop
  | n /= nDoSame = (dwNew : bNew, nNew)
  -- Variable is not updated in the loop
  | otherwise    = (dwSame : bSame, nSame)
  where
    xDo                = ssaId (x, n)
    xInit              = ssaId (x, n - 1)
    xNth               = ssaId (x, nDoNew - 1)
    (bDoSame, nDoSame) = renameBlock (x, n) bDo
    (bDoNew, nDoNew)   = renameBlock (x, n + 1) bDo
    (bSame, nSame)     = renameBlock (x, nDoSame) b
    (bNew, nNew)       = renameBlock (x, nDoNew - 1) b
    dwSame             = DoWhile bDoSame ePred
    xPhi               = Assign xDo (Phi (Var xInit) (Var xNth))
    ePred'             = renameExp (x, nDoNew - 1) ePred
    dwNew              = DoWhile (xPhi : bDoNew) ePred'

makeSSA :: Function -> Function
makeSSA (name, args, body)
  = (name, args, renameBody ssaIds body) 
  where
    ssaIds = zip (filter (/= "$return") (varNames body)) (repeat 0)
    
    renameBody [] b
      = b
    renameBody (id : ids) b
      = renameBody ids b'
      where
        (b', _) = renameBlock id b

------------------------------------------------------------------------
-- Predefined functions for displaying functions and blocks...

opNames
  = [(Add, "+"), (Mul, "*"), (Eq, "=="), (Gtr, ">")]

precTable
  = [(Add, 1), (Mul, 2), (Eq, 0), (Gtr, 0)]

prec op
  = lookUp op precTable

showArgs [] 
  = ""
showArgs as
  = foldr1 (\a s -> a ++ (", " ++ s)) as

showExp :: Int -> Exp -> String
showExp _ (Const n) 
  = show n
showExp _ (Var id) 
  = id
showExp n (Apply op' e e') 
  | n > n'    = "(" ++ s ++ ")"
  | otherwise = s
  where 
    n' = prec op'
    s = showExp n' e ++ " " ++ fromJust (lookup op' opNames ) ++ " " ++ 
        showExp n' e'
showExp _ (Phi e e')
  = "PHI(" ++ showArgs (map (showExp 0) [e, e']) ++ ")"

showLine s n k
  =  putStrLn (show n ++ ": " ++ replicate (k + 2 - length (show n)) ' ' ++ s)

showBlock' b n
  = showBlock'' b n 2
  where
    showBlock'' :: Block -> Int -> Int -> IO Int
    showBlock'' [] n k
      = return n
    showBlock'' (s : b) n k
      = do n'  <- showStatement s n k
           n'' <- showBlock'' b n' k
           return n''
    showStatement (Assign id e) n k
      = do showLine (id ++ " = " ++ showExp 0 e) n k
           return (n + 1)
    showStatement (If p q []) n k
      = do showLine ("if " ++ "(" ++ showExp 0 p ++ ") {") n k
           n' <- showBlock'' q (n + 1) (k + 2)
           showLine "}" n' k 
           return (n' + 1)
    showStatement (If p q r) n k
      = do showLine ("if " ++ "(" ++ showExp 0 p ++ ") {") n k
           n'  <- showBlock'' q (n + 1) (k + 2)
           showLine "} else {" n' k 
           n'' <- showBlock'' r (n' + 1) (k + 2)
           showLine "}" n'' k
           return (n'' + 1)
    showStatement (DoWhile b p) n k
      = do showLine "do {" n k
           n' <- showBlock'' b (n + 1) (k + 2)
           showLine ("} while " ++ showExp 9 p) n' k
           return (n' + 1)

showFun :: Function -> IO()
showFun (name, args, body)
  = do putStrLn ("1:  " ++ name ++ "(" ++ showArgs args ++ ") {")
       n <- showBlock' body 2
       showLine "}" n 0

showBlock ::  Block -> IO()
showBlock b
  = do n <- showBlock' b 1
       return ()

------------------------------------------------------------------------
-- Example state and expressions for testing...

s1 :: State
s1 = [("x", 7), ("y", 8)]

e1, e2, e3, e4, e5 :: Exp
e1 = Var "x"
e2 = Apply Mul (Apply Add (Var "x") (Const 1)) (Var "y")
e3 = Phi (Const 2) (Const 2)
e4 = Apply Add (Const 0) (Var "x")
e5 = Apply Add (Var "a") (Var "x")

------------------------------------------------------------------------
-- Example functions...

-- Figure 1...
example :: Function
example 
  = ("example",["x"],[Assign "a" (Const 1),Assign "b" (Apply Add (Var "x")
    (Const 2)),Assign "c" (Const 3),If (Apply Eq (Var "x") (Const 10)) 
    [Assign "a" (Const 1),Assign "c" (Const 5)] [],Assign "d" 
    (Apply Add (Var "a") (Const 3)),Assign "e" (Apply Add (Var "d") (Var "b")),
    Assign "$return" (Apply Add (Var "e") (Var "c"))])
    
exampleSSA :: Function
exampleSSA
  = ("example",["x"],[Assign "a0" (Const 1),Assign "b0" (Apply Add (Var "x")
    (Const 2)),Assign "c0" (Const 3),If (Apply Eq (Var "x") (Const 10)) [Assign
    "a1" (Const 1),Assign "c1" (Const 5)] [],Assign "a2" (Phi (Var "a1") (Var
    "a0")),Assign "c2" (Phi (Var "c1") (Var "c0")),Assign "d0" (Apply Add (Var
    "a2") (Const 3)),Assign "e0" (Apply Add (Var "d0") (Var "b0")),
    Assign "$return" (Apply Add (Var "e0") (Var "c2"))])
    
exampleSSAPropagated :: Function
exampleSSAPropagated
  = ("example",["x"],[Assign "b0" (Apply Add (Var "x") (Const 2)),If (Apply Eq
    (Var "x") (Const 10)) [] [],Assign "c2" (Phi (Const 5) (Const 3)),
    Assign "e0" (Apply Add (Const 4) (Var "b0")),Assign "$return" 
    (Apply Add (Var "e0") (Var "c2"))])

exampleOptimised :: Function
exampleOptimised 
  = ("example",["x"],[Assign "b0" (Apply Add (Var "x") (Const 2)),If (Apply Eq
    (Var "x") (Const 10)) [Assign "c2" (Const 5)] [Assign "c2" (Const 3)],Assign
    "e0" (Apply Add (Const 4) (Var "b0")),Assign "$return" (Apply Add (Var "e0")
    (Var "c2"))])
    

-- Figure 2 (there is no SSA version of this)...
fact :: Function
fact 
  = ("fact", 
     ["n"], 
     [If (Apply Eq (Var "n") (Const 0))
        [Assign "$return" (Const 1)]
        [Assign "prod" (Const 1),
         Assign "i" (Var "n"),
         DoWhile 
           [Assign "prod" (Apply Mul (Var "prod") (Var "i")),
            Assign "i" (Apply Add (Var "i") (Const (-1)))
           ] 
           (Apply Gtr (Var "i") (Const 0)),
         Assign "$return" (Var "prod")
        ]
     ]
    )


-- Summation loop, specialised loop for the case k=0...
loop :: Function
loop 
  = ("loop",["n"],[Assign "i" (Var "n"),Assign "k" (Const 0),Assign "sum"
    (Const 0),If (Apply Eq (Var "i") (Const 0)) [Assign "$return" (Const 0)]
    [DoWhile [Assign "sum" (Apply Add (Var "sum") (Apply Mul (Apply Add 
    (Var "i") (Apply Mul (Const 2) (Var "k"))) (Apply Add (Apply Add (Var "i") 
    (Apply Mul (Const 2) (Var "k"))) (Const 1)))),Assign "i" (Apply Add 
    (Var "i") (Const (-1)))] (Apply Gtr (Var "i") (Const 0)),
    Assign "$return" (Var "sum")]])
    
loopSSA :: Function
loopSSA
  = ("loop",["n"],[Assign "i0" (Var "n"),Assign "k0" (Const 0),Assign "sum0"
    (Const 0),If (Apply Eq (Var "i0") (Const 0)) [Assign "$return" (Const 0)]
    [DoWhile [Assign "sum1" (Phi (Var "sum0") (Var "sum2")),Assign "i1" 
    (Phi (Var "i0") (Var "i2")),Assign "k1" (Apply Mul (Var "k0") (Const 2)),
    Assign "a0" (Apply Add (Var "i1") (Var "k1")),Assign "k2" (Apply Mul 
    (Var "k0") (Const 2)),Assign "b0" (Apply Add (Var "k2") (Const 1)),
    Assign "b1" (Apply Add (Var "i1") (Var "b0")),Assign "m0" (Apply Mul 
    (Var "a0") (Var "b1")),Assign "sum2" (Apply Add (Var "sum1") (Var "m0")),
    Assign "i2" (Apply Add (Var "i1") (Const (-1)))] (Apply Gtr (Var "i2") 
    (Const 0)),Assign "$return" (Var "sum2")]])
    
loopSSAPropagated :: Function
loopSSAPropagated 
  = ("loop",["n"],[Assign "i0" (Var "n"),If (Apply Eq (Var "i0") (Const 0))
    [Assign "$return" (Const 0)] [DoWhile [Assign "sum1" (Phi (Const 0) (Var
    "sum2")),Assign "i1" (Phi (Var "i0") (Var "i2")),Assign "a0" (Var "i1"),
    Assign "b1" (Apply Add (Var "i1") (Const 1)),Assign "m0" (Apply Mul 
    (Var "a0") (Var "b1")),Assign "sum2" (Apply Add (Var "sum1") (Var "m0")),
    Assign "i2" (Apply Add (Var "i1") (Const (-1)))] (Apply Gtr (Var "i2") 
    (Const 0)),Assign "$return" (Var "sum2")]])
 
loopOptimised :: Function
loopOptimised 
  = ("loop",["n"],[Assign "i0" (Var "n"),If (Apply Eq (Var "i0") (Const 0))
    [Assign "$return" (Const 0)] [Assign "sum1" (Const 0),Assign "i1" (Var
    "i0"),DoWhile [Assign "a0" (Var "i1"),Assign "b1" (Apply Add (Var "i1") 
    (Const 1)),Assign "m0" (Apply Mul (Var "a0") (Var "b1")),Assign "sum2" 
    (Apply Add (Var "sum1") (Var "m0")),Assign "i2" (Apply Add (Var "i1") 
    (Const (-1))),Assign "sum1" (Var "sum2"),Assign "i1" (Var "i2")] 
    (Apply Gtr (Var "i2") (Const 0)),Assign "$return" (Var "sum2")]])
    

-- Basic block (no conditionals or loops)...
basicBlock :: Function
basicBlock 
  = ("basicBlock",[],[Assign "x" (Const 1),Assign "y" (Const 2),Assign "x"
    (Apply Add (Var "x") (Var "y")),Assign "y" (Apply Mul (Var "x") (Const
    3)),Assign "$return" (Var "y")])
    
basicBlockSSA :: Function
basicBlockSSA 
  = ("basicBlock",[],[Assign "x0" (Const 1),Assign "y0" (Const 2),Assign "x1"
    (Apply Add (Var "x0") (Var "y0")),Assign "y1" (Apply Mul (Var "x1") (Const
    3)),Assign "$return" (Var "y1")])
    
basicBlockSSAPropagated :: Function
basicBlockSSAPropagated
  = ("basicBlock", [], [Assign "$return" (Const 9)])

-- (This is the same as above, as there were no phi functions.)
basicBlockOptimised :: Function
basicBlockOptimised
  = ("basicBlock", [], [Assign "$return" (Const 9)])

-- Computes the maximum of two integers; useful for testing unPhi...
max2 :: Function
max2 
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m" (Var "x")]
    [Assign "m" (Var "y")],Assign "$return" (Var "m")])

max2SSA :: Function
max2SSA 
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m0" (Var
    "x")] [Assign "m1" (Var "y")],Assign "m2" (Phi (Var "m0") (Var "m1")),Assign
    "$return" (Var "m2")])

max2SSAPropagated :: Function
max2SSAPropagated
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m0" (Var
    "x")] [Assign "m1" (Var "y")],Assign "m2" (Phi (Var "m0") (Var "m1")),Assign
    "$return" (Var "m2")])

max2Optimised :: Function
max2Optimised 
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m0" (Var
    "x"),Assign "m2" (Var "m0")] [Assign "m1" (Var "y"),Assign "m2" (Var
    "m1")],Assign "$return" (Var "m2")])


