module TypeInference where

import Data.Maybe
import Data.Bifunctor
import Data.Char

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Show)

showT :: Type -> String
showT TInt  
  = "Int"
showT TBool 
  = "Bool"
showT (TFun t t') 
  = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a) 
  = a
showT TErr  
  = "Type error"

type TypeTable = [(String, Type)]

type TEnv 
  = TypeTable    -- i.e. [(String, Type)]

type Sub 
  = TypeTable    -- i.e. [(String, Type)]  

-- Built-in function types...
primTypes :: TypeTable
primTypes 
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

------------------------------------------------------
-- PART I

-- Pre: The search item is in the table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp k
  = tryToLookUp k undefined

tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp k def kvs
  | isNothing v = def
  | otherwise   = fromJust v
  where
    v = lookup k kvs

-- Pre: The given value is in the table
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp v kvs
  = [key | (key, val) <- kvs, v == val]

occurs :: String -> Type -> Bool
occurs v (TFun t t')
  = occurs v t || occurs v t'
occurs v (TVar s)
  = v == s
occurs _ _
  = False

------------------------------------------------------
-- PART II

-- Pre: There are no user-defined functions (constructor Fun)
-- Pre: All variables in the expression have a binding in the given 
--      type environment
inferType :: Expr -> TEnv -> Type
inferType (Number _) _
  = TInt
inferType (Boolean _) _
  = TBool
inferType (Id x) e
  = lookUp x e
inferType (Prim x) _
  = lookUp x primTypes
inferType (Cond x y z) e
  | tx == TBool && ty == tz = ty
  | otherwise               = TErr
  where
    [tx, ty, tz] = map (`inferType` e) [x, y, z]
inferType (App f a) e
  = inferApp (inferType f e) (inferType a e)
  where
    inferApp (TFun pType rType) aType
      | pType == aType = rType
    inferApp _ _
      = TErr

------------------------------------------------------
-- PART III

applySub :: Sub -> Type -> Type
applySub s (TFun t t')
  = TFun (applySub s t) (applySub s t')
applySub s (TVar x)
  | t == TErr = TVar x
  | otherwise = t
  where
    t = tryToLookUp x TErr s
applySub _ t
  = t

unify :: Type -> Type -> Maybe Sub
unify t t'
  = unifyPairs [(t, t')] []

unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs [] s
  = Just s
unifyPairs ((TInt, TInt) : ts) s
  = unifyPairs ts s
unifyPairs ((TBool, TBool) : ts) s
  = unifyPairs ts s
unifyPairs ((TVar v, TVar v') : ts) s  
  | v == v' = unifyPairs ts s  
unifyPairs ((TVar v, t) : ts) s 
  | occurs v t = Nothing
  | otherwise  = unifyPairs (map (bimap applySub' applySub') ts) (s' : s)
  where
    applySub' = applySub [s']
    s'        = (v, t)
unifyPairs ((t, TVar v) : ts) s 
  = unifyPairs ((TVar v, t) : ts) s
unifyPairs ((TFun t1 t2, TFun t1' t2') : ts) s
  = unifyPairs ((t1, t1') : (t2, t2') : ts) s
unifyPairs _ _
  = Nothing

------------------------------------------------------
-- PART IV

updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv tenv tsub
  = map modify tenv
  where
    modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld
  = sNew ++ updateTEnv sOld sNew

-- In combineSubs [s1, s2,..., sn], s1 should be the *most recent* substitution
-- and will be applied *last*
combineSubs :: [Sub] -> Sub
combineSubs 
  = foldr1 combine
    
inferPolyType :: Expr -> Type
inferPolyType x
  = t
  where
    (_, t, _) = inferPolyType' x [] 1

-- You may optionally wish to use one of the following helper function declarations
-- as suggested in the specification. 

-- inferPolyType' :: Expr -> TEnv -> [String] -> (Sub, Type, [String])
-- inferPolyType'
--   = undefined

intToString :: Int -> String
intToString 0
  = "0"
intToString n
  | n < 10    = lastStr
  | otherwise = intToString rest ++ lastStr
  where 
    (rest, last) = quotRem n 10
    lastStr      = [intToDigit last]
    
inferPolyType' :: Expr -> TEnv -> Int -> (Sub, Type, Int)
inferPolyType' (Number _) _ n
  = ([], TInt, n)

inferPolyType' (Boolean _) _ n
  = ([], TBool, n)

inferPolyType' (Id x) tenv n
  | t == TErr = ([], tx, n + 1)
  | otherwise = ([], t, n)
  where
    tx = TVar ('a' : intToString n)
    t  = tryToLookUp x TErr tenv

inferPolyType' (Prim x) _ n
  = ([], lookUp x primTypes, n)

inferPolyType' (Fun x e) tenv n
  | te == TErr = ([], TErr, 0)
  | otherwise  = (tsub, TFun tx' te, n')
  where
    tx             = TVar ('a' : intToString n)
    (tsub, te, n') = inferPolyType' e ((x, tx) : tenv) (n + 1)
    tx'            = applySub tsub tx
  
inferPolyType' (App f e) tenv n
  | isNothing tsub'' = ([], TErr, 0)
  | tp == te'        = (combined, tr, n'' + 1)
  | otherwise        = ([], TErr, 0)
  where
    (tsub, tf, n')    = inferPolyType' f tenv n
    tenv'             = updateTEnv tenv tsub
    (tsub', te, n'')  = inferPolyType' e tenv' n'
    tsub''            = unify tf (TFun te (TVar ('a' : intToString n'')))
    combined          = combineSubs [fromJust tsub'', tsub', tsub]
    [TFun tp tr, te'] = map (applySub combined) [tf, te]
  
inferPolyType' (Cond x y z) tenv n
  | tx' == TBool && ty' == tz' = (combined, ty', nZ + 1)
  | otherwise                  = (combined, TErr, nZ + 1)
  where
    (tsubX, tx, nX) = inferPolyType' x tenv n 
    tenvX           = updateTEnv tenv tsubX
    (tsubY, ty, nY) = inferPolyType' y (updateTEnv tenvX tsubX) nX
    tenvY           = updateTEnv tenv tsubY
    (tsubZ, tz, nZ) = inferPolyType' z (updateTEnv tenvY tsubY) nY
    pred            = unify tx TBool
    thenElse        = unify ty tz
    combined        = combineSubs [fromJust thenElse, 
                                   fromJust pred, 
                                   tsubZ, 
                                   tsubY, 
                                   tsubX]
    [tx', ty', tz'] = map (applySub combined) [tx, ty, tz]
  
------------------------------------------------------
-- Monomorphic type inference test cases from Table 1...

env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Unification test cases from Table 2...

u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Polymorphic type inference test cases from Table 3...

ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3"))) 
              (TVar "a3"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+"))) 
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3")) 
              (TFun (TVar "a2") (TVar "a3"))

-- If x then y + 1 else 2
exCond1 = Cond (Id "x") (App (App (Prim "+") (Number 1)) (Id "y")) (Number 2)
typeCond1 = TInt

-- If 2 then 1 else 0 (should return TErr)
exCond2 = Cond (Number 2) (Number 1) (Number 0)
typeCond2 = TErr

-- If True then 1 else False (should return TErr)
exCond3 = Cond (Boolean True) (Number 1) (Boolean False)
typeCond3 = TErr

-- If x then (if y then 0 else 1) else 2
exCond4 = Cond (Id "x") (Cond (Id "y") (Number 0) (Number 1))  (Number 2)
typeCond4 = TInt