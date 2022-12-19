module Tests where

import IC.TestSuite

import TypeInference

instance Reformat Expr where
  reformat = show

lookUpTestCases
  = [ 
      -- ("x", [("hello",1),("x",9),("dolly",1)]) ==> 9,
      ("not", primTypes) ==> TFun TBool TBool 
    ]

tryToLookUpTestCases
  = [ ("k", 0, [("hello",1),("x",9),("dolly",1)]) ==> 0
    ]

reverseLookUpTestCases
  = [ 
      -- (1, [("hello",1),("x",9),("dolly",1)]) ==> ["hello", "dolly"],
      (TFun TInt (TFun TInt TBool), primTypes) ==> [">", "=="]
    ]

occursTestCases
  = [ ("x", TVar "y") ==> False, 
      ("x", TBool) ==> False,
      ("x", TFun TBool (TVar "x")) ==> True
    ]

inferTypeTestCases
  = [ (ex1, env) ==> type1,
      (ex2, env) ==> type2,
      (ex3, env) ==> type3,
      (ex4, env) ==> type4,
      (ex5, env) ==> type5,
      (ex6, env) ==> type6, 
      (ex7, env) ==> type7,
      (ex8, env) ==> type8
    ]

s = [("a",TBool),("b",TFun TBool TInt)]
applySubTestCases
  = [ (s, TInt) ==> TInt,
      (s, TFun TBool (TVar "a")) ==> TFun TBool TBool,
      (s, TVar "c") ==> TVar "c"
    ]

unifyTestCases
  = [ (u1a, u1b) ==> sub1,
      (u2a, u2b) ==> sub2,
      (u3a, u3b) ==> sub3,
      (u4a, u4b) ==> sub4,
      (u5a, u5b) ==> sub5,
      (u6a, u6b) ==> sub6
    ]

inferPolyTypeTestCases
  = [ ex9 ==> type9,
      ex10 ==> type10,
      ex11 ==> type11,
      ex12 ==> type12,
      ex13 ==> type13,
      ex14 ==> type14,
      exCond1 ==> typeCond1,
      exCond2 ==> typeCond2, 
      exCond3 ==> typeCond3,
      exCond4 ==> typeCond4
    ]
  
allTestCases
  = [ TestCase  "lookUp"          (uncurry lookUp)
                                  lookUpTestCases

    , TestCase  "tryToLookUp"     (uncurry3 tryToLookUp)
                                  tryToLookUpTestCases

    , TestCase  "reverseLookUp"   (uncurry reverseLookUp)
                                  reverseLookUpTestCases

    , TestCase  "occurs"          (uncurry occurs)
                                  occursTestCases

    , TestCase  "inferType"       (uncurry inferType)
                                  inferTypeTestCases

    , TestCase  "applySub"        (uncurry applySub)
                                  applySubTestCases   

    , TestCase  "unify"           (uncurry unify)
                                  unifyTestCases      

    , TestCase  "inferPolyType"   (inferPolyType)
                                  inferPolyTypeTestCases              
    ]

runTests = mapM_ goTest allTestCases

main = runTests
