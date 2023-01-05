module Tests where

import IC.TestSuite

import ConstantPropagation

instance Reformat Exp where
  reformat = show

updateTestCases
  = [ (("x",3), s1) ==> [("x",3),("y",8)],
      (("z",0), s1) ==> [("z",0),("x",7),("y",8)]
    ]

applyTestCases
  = [ (Add, 7, 5) ==> 12,
      (Gtr, 4, 9) ==> 0
    ]

execTestCases
  = [ (fact, [5]) ==> [("$return",120),("i",0),("prod",120),("n",5)],
      (loop, [4]) ==> [("$return",40),("i",0),("sum",40),("k",0),("n",4)],
      (loopOptimised, [4]) ==> [("$return",40),("i1",0),("sum1",40),("i2",0),
                                ("sum2",40),("m0",2),("b1",2),("a0",1),("i0",4),
                                ("n",4)]
    ]

foldConstTestCases
  = [ e3 ==> Const 2,
      e4 ==> Var "x"
    ]

subTestCases
  = [ ("a", 0, e5) ==> Var "x"
    ]

applyPropagateTestCases
  = [ loopSSA ==> loopSSAPropagated,
      exampleSSA ==> exampleSSAPropagated,
      basicBlockSSA ==> basicBlockSSAPropagated,
      max2SSA ==> max2SSAPropagated
    ]

unPhiTestCases
  = [ loopSSAPropagated ==> loopOptimised,
      exampleSSAPropagated ==> exampleOptimised,
      basicBlockSSAPropagated ==> basicBlockOptimised,
      max2SSAPropagated ==> max2Optimised
    ]

makeSSATestCases
  = [ loop ==> loopSSA,
      example ==> exampleSSA,
      basicBlock ==> basicBlockSSA,
      max2 ==> max2SSA
    ]

allTestCases
  = [ TestCase  "update"               (uncurry update)
                                       updateTestCases

    , TestCase  "apply"                (uncurry3 apply)
                                       applyTestCases     

    , TestCase  "execStatement, execBlock"  (uncurry execFun)
                                             execTestCases    

    , TestCase  "foldConst"            (foldConst)
                                       foldConstTestCases     

    , TestCase  "sub"                  (uncurry3 sub)
                                       subTestCases   

    , TestCase  "propagateConstants"   (applyPropagate)
                                       applyPropagateTestCases    

    , TestCase  "unPhi"                (applyUnPhi)
                                       unPhiTestCases    

    , TestCase  "makeSSA"              (makeSSA)
                                       makeSSATestCases  
    ]

runTests = mapM_ goTest allTestCases

main = runTests
