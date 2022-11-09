module Parser where

import Data.List
import Data.Maybe
import Data.Char

type Operator = Char

data Token = TNum Int | TVar String | TOp Operator
             deriving (Eq, Show)

data Expr = ENum Int | EVar String | EApp Operator Expr Expr
            deriving (Eq, Show)

type Precedence = Int

data Associativity = L | N | R
                     deriving (Eq, Ord, Show)

type ExprStack = [Expr]

type OpStack = [Operator]
-------------------------------------------------------------------
-- The following are given...
ops :: [Operator]
ops = "+-*/^()$"

opTable :: [(Operator, (Precedence, Associativity))]
opTable = [('$',(0,N)), ('(',(1,N)), (')',(1,N)), ('+',(6,L)),
 ('-',(6,L)), ('*',(7,L)), ('/',(7,L)), ('^',(8,R))]

stringToInt :: String -> Int
stringToInt = read

showExpr :: Expr -> String
showExpr (ENum n) = show n
showExpr (EVar s) = s
showExpr (EApp op e e') = "(" ++ showExpr e ++ [op] ++ showExpr e' ++ ")"

-------------------------------------------------------------------
-- Returns the first value of the tuple returned from looking up
-- the operator in the given table
precedence :: Operator -> Precedence
-- Pre: the operator has a binding in opTable
precedence op
  = p
  where
    Just (p, _) = lookup op opTable

-- Returns the second value of the tuple returned from looking up
-- the operator in the given table
associativity :: Operator -> Associativity
-- Pre: the operator has a binding in opTable
associativity op
  = a
  where
    Just (_, a) = lookup op opTable

-- Gives the result of a having greater precedence than b, or the
-- same precedence and a having right asssociativity
supersedes :: Operator -> Operator -> Bool
supersedes op op'
  = p > p' || (p == p' && associativity op == R)
  where
    p = precedence op
    p' = precedence op'

-- Converts the input string into a list of tokens, char by char,
-- unless the token is a variable/number, in which case it breaks
-- at the next non alphanumeric character and continues recursion
tokenise :: String -> [Token]
-- Pre: The input string is a well-formed expression
tokenise ""
  = []
tokenise str@(c : cs)
  | isSpace c    = tokenise cs
  | c `elem` ops = TOp c : tokenise cs
  | isDigit c    = TNum (stringToInt s) : tokenise s'
  | otherwise    = TVar s : tokenise s'
  where
    (s, s') = span isAlphanumeric str

    isAlphanumeric c
      = not (isSpace c || c `elem` ops)

-- Returns a list of all the variables found in a well-formed
-- expression, with duplicates removed using nub
allVars :: String -> [String]
allVars str
  = nub [var | TVar var <- tokenise str]

--
-- This function is given
--
expParser :: String -> Expr
expParser s 
  = parse (tokenise s) [] ['$']

-- A stack is used, with a different interaction
-- based on the type of each token, to produce
-- an expression for a list of tokens
parse :: [Token] -> ExprStack -> OpStack -> Expr
-- Pre: the list of tokens is not empty
parse [] [e] _
  = e
parse [] (e : e' : es) (o : os)
  = parse [] eStack' os
  where
    eStack' = EApp o e' e : es
parse tokens@(TOp op : ts) eStack oStack@(o : os)
  | op `supersedes` o = parse ts eStack (op : oStack)
  | otherwise         = parse tokens eStack' os
  where
    (e : e' : es) = eStack
    eStack' = EApp o e' e : es
parse (TNum n : ts) eStack oStack
  = parse ts (ENum n : eStack) oStack
parse (TVar s : ts) eStack oStack
  = parse ts (EVar s : eStack) oStack 

applyEApp :: ExprStack -> OpStack -> Expr
applyEApp 
  = undefined

-----------------------------------------------------------------------------
-- Some test cases...

s1, s2 :: String

s1 = "1+7*9"

s2 = "4+x^2-8*y"