module Supercombinators where

import Data.List
import Data.Maybe
import Data.Bifunctor

import Types
import Examples

---------------------------------------------------------

prims :: [Id]
prims
  = ["+", "-", "*", "<=", "ite"]

lookUp :: Id -> [(Id, a)] -> a
lookUp v env
  = fromMaybe (error ("lookUp failed with search key " ++ v))
              (lookup v env)

---------------------------------------------------------
-- Part I

isFun :: Exp -> Bool
isFun (Fun _ _)
  = True
isFun _
  = False

splitDefs :: [Binding] -> ([Binding], [Binding])
splitDefs 
  = partition (isFun . snd)

topLevelFunctions :: Exp -> Int
topLevelFunctions (Let bs _)
  = length (filter (isFun . snd) bs)
topLevelFunctions _
  = 0

---------------------------------------------------------
-- Part II

unionAll :: Eq a => [[a]] -> [a]
unionAll
  = foldl union []

freeVars :: Exp -> [Id]
freeVars (Const _)
  = []
freeVars (Var v)
  | v `elem` prims = []
  | otherwise      = [v]
freeVars (Fun args body)
  = freeVars body \\ args
freeVars (App f args)
  = unionAll (freeVars f : map freeVars args)
freeVars (Let bs e)
  = unionAll (freeVars e : map (freeVars . snd) bs) \\ map fst bs

---------------------------------------------------------
-- Part III

-- Given...
lambdaLift :: Exp -> Exp
lambdaLift e
  = lift (modifyFunctions (buildFVMap e) e)

buildFVMap :: Exp -> [(Id, [Id])]
buildFVMap (Let bs e)
  = unionAll (buildFVMap e : topLvlFVs : (nestedFVs ++ restFVMap))
  where
    (fs, rest) = splitDefs bs
    siblings   = map fst fs
    fvs        = sort $ unionAll (map ((\\ siblings) . freeVars . snd) fs)
    topLvlFVs  = map (\(f, _) -> (f, fvs)) fs
    restFVMap  = map (buildFVMap . snd) rest
    nestedFVs  = map ((\(Fun _ body) -> buildFVMap body) . snd) fs
buildFVMap (Fun _ body)
  = buildFVMap body
buildFVMap (App f args)
  = unionAll (buildFVMap f : map buildFVMap args)
buildFVMap _
  = []

modifyFunctions :: [(Id, [Id])] -> Exp -> Exp
-- Pre: The mapping table contains a binding for every function
-- named in the expression.
modifyFunctions mapper (Let ((f, Fun args body) : bs) e)
  = Let (('$' : f, Fun args' body') : bs') e'
  where
    args'      = lookUp f mapper ++ args
    body'      = modifyFunctions mapper body
    Let bs' e' = modifyFunctions mapper (Let bs e)
modifyFunctions mapper (Let ((id, exp) : bs) e)
  = Let ((id, modifyFunctions mapper exp) : bs') e'
  where
    Let bs' e' = modifyFunctions mapper (Let bs e)
modifyFunctions mapper (Let [] e)
  = Let [] (modifyFunctions mapper e)
modifyFunctions mapper (Var f)
  | f `elem` prims || isNothing free = Var f
  | null free'                       = f'
  | otherwise                        = App f' (map Var free')
  where
    f'    = Var ('$' : f)
    free  = lookup f mapper
    free' = fromJust free
modifyFunctions _ (Const c)
  = Const c
modifyFunctions mapper (App f args)
  = App (modifyFunctions mapper f) (map (modifyFunctions mapper) args)

-- The default definition here is id.
-- If you implement the above two functions but not this one
-- then lambdaLift above will remove all the free variables
-- in functions; it just won't do any lifting.
lift :: Exp -> Exp
lift (Let [] e)
  = e
lift (Let bs e)
  | null scs && null rest = e'
  | null rest             = Let scs e'
  | null scs              = Let rest e'
  | otherwise             = Let scs (Let rest e')
  where
    bsStripped        = map (second lift') bs
    bsLifted          = map (second (lift . fst)) bsStripped
    nestedScs         = concatMap (snd . snd) bsStripped
    (e', inScs)       = (first lift . lift') e
    (topLvlScs, rest) = splitDefs (map (second fst) bsStripped)
    scs               = inScs ++ topLvlScs ++ nestedScs
lift (Fun args body)
  = Fun args (lift body)
lift (App f args)
  | null scs  = app'
  | otherwise = Let scs app'
  where
    (f', fScs)   = lift' f
    argsStripped = map lift' args
    argsLifted   = map (lift . fst) argsStripped
    argsScs      = concatMap snd argsStripped
    scs          = fScs ++ argsScs
    app'         = App (lift f') argsLifted
lift e 
  = e

-- You may wish to use this...
lift' :: Exp -> (Exp, [Supercombinator])
lift' (Let bs e)
  = (Let rest e, scs)
  where
    (scs, rest) = splitDefs bs
lift' (Fun args body)
  = (Fun args body', scs)
  where
    (body', scs) = lift' body
lift' (App f args)
  = (App f' args', unionAll (fScs : argsScs))
  where
    (f', fScs)       = lift' f
    (args', argsScs) = unzip (map lift' args) 
lift' e
  = (e, [])