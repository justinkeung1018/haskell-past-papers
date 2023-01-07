module Tests where

import IC.TestSuite

import Alloc
import Types 
import Examples

import Data.List

countTestCases
  = [ (3, [2,4,1,3,2,3,3]) ==> 3
    ]

degreesTestCases
  = [ ([1], []) ==> [(1,0)],
      fig1Left ==> [(1,2),(2,2),(3,2),(4,2)]
    ]

neighboursTestCases
  = [ (1, ([1], [])) ==> [],
      (2, fig1Left) ==> [1,4]
    ]

removeNodeTestCases
  = [ ("i", factIG) ==> (["n","prod"],[("n","prod")]),
      ("prod", factIG) ==> (["i","n"],[])
    ]

colourGraphTestCases
  = [ 
      -- (2, fig1Middle) ==> [(2,2),(1,0),(3,2),(4,1)],
      (2, fig3IG) ==> [("a",0),("b",0),("c",0),("d",2),("n",1)],
      (3, fig3IG) ==> [("a",3),("b",0),("c",3),("d",2),("n",1)]
    ]

buildIdMapTestCases
  = [ factColouring ==> [("return","return"),("i","R2"),("n","R2"),("prod","R1")],
      fig3Colouring ==> [("return","return"),("a","R3"),("b","b"),("c","R3"),("d","R2"),("n","R1")]
    ]

buildArgAssignmentsTestCase
  = [ (["x","y"], idMap1) ==> [Assign "R6" (Var "x"),Assign "R1" (Var "y")]
    ]

renameExpTestCases
  = [ (e1, idMap1) ==> Apply Add (Var "a") (Var "R1"),
      (e2, idMap1) ==> Apply Mul (Apply Add (Var "R6") (Const 2)) (Var "R1")
    ]

renameBlockTestCases
  = [ (fact, factIdMap) ==> factTransformed,
      (fig3, fig3IdMap) ==> fig3Transformed
    ]

buildIGTestCases
  = [ factLiveVars ==> factIG,
      fig3LiveVars ==> fig3IG,
      fig3LiveVars' ==> fig3IG
    ]

liveVarsTestCases
  = [ factCFG ==> factLiveVars,
      fig3CFG ==> fig3LiveVars
    ]

allTestCases
  = [ TestCase  "count"              (uncurry count)
                                     countTestCases

    , TestCase  "degrees"            (degrees)
                                     degreesTestCases       

    , TestCase  "neighbours"         (uncurry neighbours)
                                     neighboursTestCases 

    , TestCase  "removeNode"         (uncurry removeNode)
                                     removeNodeTestCases 

    , TestCase  "colourGraph"        (uncurry removeNode)
                                     removeNodeTestCases 

    , TestCase  "buildIdMap"         (buildIdMap)
                                     buildIdMapTestCases 

    , TestCase  "buildArgAssignments" (uncurry buildArgAssignments)
                                      buildArgAssignmentsTestCase 

    , TestCase  "renameExp"          (uncurry renameExp)
                                     renameExpTestCases 

    , TestCase  "renameBlock"        (uncurry renameFun)
                                     renameBlockTestCases 

    , TestCase  "buildIG"            (sortGraph . buildIG)
                                     buildIGTestCases 

    , TestCase  "liveVars"           (map sort . liveVars)
                                     liveVarsTestCases 
    ]

runTests = mapM_ goTest allTestCases

main = runTests
