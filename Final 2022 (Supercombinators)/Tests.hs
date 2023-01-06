module Tests where

import IC.TestSuite

import Supercombinators
import Types 
import Examples

instance Reformat Exp where
  reformat = show

isFunTestCases
  = [ e2 ==> False,
      e5 ==> True
    ]

splitDefsTestCases
  = [ [("x",Const 3),("g",Fun ["y"] (App (Var "+") [Var "y",Var "x"]))] 
      ==> ([("g",Fun ["y"] (App (Var "+") [Var "y",Var "x"]))],[("x",Const 3)])
    ]

topLevelFunctionsTestCases
  = [ e4 ==> 1,
      e12 ==> 2,
      e20 ==> 0
    ]

unionAllTestCases
  = [ [[2, 5], [4, 2], [5, 1]] ==> [2,5,4,1],
      [] ==> [],
      [[]] ==> [],
      [[], []] ==> [],
      [[], [1]] ==> [1],
      [[1], []] ==> [1]
    ]

freeVarsTestCases
  = [ e7 ==> ["z"],
      e8 ==> ["y","z"],
      e9 ==> ["x"]
    ]

buildFVMapTestCases
  = [ e1 ==> [("f",[]),("g",["x"])],
      e2 ==> [("succ",[])],
      e19 ==> [("f",["y","z"]),("g",["y","z"])]
    ]

modifyFunctionsTestCases
  = [ modifyFunctions (buildFVMap e19) e19 
         ==> "let\n\
             \  y = 1\n\
             \in let\n\
             \     z = 1\n\
             \     $f y z x = if <= x y\n\
             \                then\n\
             \                  y\n\
             \                else\n\
             \                  ($g y z) x\n\
             \     $g y z x = * x (($f y z) (- x z))\n\
             \   in ($f y z) 6"
    ]

lambdaLiftTestCases
  = [ e1 ==> e1',
      e2 ==> e2',
      e4 ==> e4',
      e12 ==> e12',
      e13 ==> e13',
      e14 ==> e14',
      e15 ==> e15',
      e16 ==> e16',
      e18 ==> e18',
      e19 ==> e19'
    ]

allTestCases
  = [ TestCase  "isFun"              (isFun)
                                     isFunTestCases

    , TestCase  "splitDefs"          (splitDefs)
                                     splitDefsTestCases           

    , TestCase  "topLevelFunctions"  (topLevelFunctions)
                                     topLevelFunctionsTestCases     

    , TestCase  "unionAll"           (unionAll)
                                     unionAllTestCases      

    , TestCase  "freeVars"           (freeVars)
                                     freeVarsTestCases  

    , TestCase  "buildFVMap"         (buildFVMap)
                                     buildFVMapTestCases    

    , TestCase  "modifyFunctions"    (showE)
                                     modifyFunctionsTestCases    

    , TestCase  "lambdaLift"         (lambdaLift)
                                     lambdaLiftTestCases    
    ]

runTests = mapM_ goTest allTestCases

main = runTests
