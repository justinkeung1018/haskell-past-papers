module Tests where

import IC.TestSuite

import Functions

getValueTestCases
  = [ ("x", sampleState) ==> I 5
    ]

getLocalsTestCases
  = [ sampleState ==> [("x",(Local,I 5))]
    ]

getGlobalsTestCases
  = [ sampleState ==> [("y",(Global,I 2)),("a",(Global,A [(0,4),(1,2),(2,7)]))]
    ]

assignArrayTestCases
  = [ (getValue "a" sampleState, I 2, I 1) ==> A [(2,1), (0,4), (1,2)],
      (A [], I 0, I 1) ==> A [(0, 1)],
      (A [(0, 1)], I 0, I 2) ==> A [(0, 2)]
    ]

updateVarTestCases
  = [ (("x", I 6), sampleState) ==> [("x",(Local,I 6)),("y",(Global,I 2)),("a",(Global,A [(0,4),(1,2),(2,7)]))],
      (("z", I 3), [("g", (Global, I 8))]) ==> [("g",(Global,I 8)),("z",(Local,I 3))],
      (("x", I 0), [("x", (Local, I 1)), ("x", (Global, I 2))]) ==> [("x", (Local, I 0)), ("x", (Global, I 2))]
    ]

applyOpTestCases
  = [ (Add, I 6, I (-2)) ==> I 4,
      (Mul, I 3, I 4) ==> I 12,
      (Less, I 7, I 0) ==> I 0,
      (Equal, I 2, I 2) ==> I 1,
      (Index, A [(1,1),(0,3)], I 0) ==> I 3,
      (Index, A [(1,1),(0,3)], I 2) ==> I 0
    ]

bindArgsTestCases
  = [ (["x", "a"], [I 6, A [(1,1),(0,3)]]) ==> [("x",(Local,I 6)),("a",(Local,A [(1,1),(0,3)]))]
    ]

evalTestCases
  = [ (e1, [], sampleState) ==> I 1,
      (e2, [], sampleState) ==> I 2,
      (e3, [], sampleState) ==> I 7,
      (e4, [], sampleState) ==> I 5,
      (e5, [fib], sampleState) ==> I 8
    ]

executeStatementTestCases
  = [ (Assign "s" (Const (I 0)), [], [], []) ==> [("s", (Local, I 0))]
    ]

executeBlockTestCases
  = [ ([Assign "s" (Const (I 0)), Assign "i" (Const (I 0))], [], [], []) ==> [("s", (Local, I 0)), ("i", (Local, I 0))],
      ([Call "s" "sumA\'" [sampleArray, intToExp 3]], [], [sumA'], []) ==> [("s",(Local,I 22))],
      ([Call "s" "sumA\'" [sampleArray, intToExp 3]], [], [sumA'], [("s", (Global, I 0))]) ==> [("s",(Global,I 22))],
      ([Call "f" "fibM" [intToExp 35]], [], [fibM, fibTableManager], fibState) ==> 
        [("fibPres",(Global,A [(34,1),(33,1),(32,1),(31,1),(30,1),(29,1),(28,1),(27,1),
          (26,1),(25,1),(24,1), (23,1),(22,1),(21,1),(20,1),(19,1),(18,1),(17,1),(16,1),
          (15,1),(14,1),(13,1), (12,1),(11,1),(10,1),(9,1),(8,1),(7,1),(6,1),(5,1),(4,1),
          (3,1),(1,1),(2,1)])), ("fibTab",(Global,A [(34,5702887),(33,3524578),
          (32,2178309), (31,1346269), (30,832040),(29,514229),(28,317811),(27,196418),
          (26,121393),(25,75025),(24,46368), (23,28657),(22,17711),(21,10946),(20,6765),
          (19,4181),(18,2584),(17,1597),(16,987), (15,610),(14,377),(13,233),(12,144),
          (11,89),(10,55),(9,34),(8,21),(7,13),(6,8),(5,5),(4,3),(3,2),(1,1),(2,1)])),
          ("f",(Local,I 9227465))]
    ]

translateTestCases
  = [ (fib, "fibM", [("fib", "fibManager")]) ==> fibM,
      (("", ([], intToExp 1)), "", []) ==> ("", ([], [Return (intToExp 1)])),
      (("", (["x"], Var "x")), "", []) ==> ("", (["x"], [Return (Var "x")]))
    ]

allTestCases
  = [ TestCase  "getValue"           (uncurry getValue)
                                     getValueTestCases

    , TestCase  "getLocals"          (getLocals)
                                     getLocalsTestCases          

    , TestCase  "getGlobals"         (getGlobals)
                                     getGlobalsTestCases    

    , TestCase  "assignValue"        (uncurry3 assignArray)
                                     assignArrayTestCases    

    , TestCase  "updateVar"          (uncurry updateVar)
                                     updateVarTestCases    

    , TestCase  "applyOp"            (uncurry3 applyOp)
                                     applyOpTestCases    

    , TestCase  "bindArgs"           (uncurry bindArgs)
                                     bindArgsTestCases    

    , TestCase  "eval"               (uncurry3 eval)
                                     evalTestCases    

    , TestCase  "executeStatement"   (uncurry4 executeStatement)
                                     executeStatementTestCases                                   

    , TestCase  "executeBlock"       (uncurry4 executeBlock)
                                     executeBlockTestCases    

    , TestCase  "translate"          (uncurry3 translate)
                                     translateTestCases   
    ]

runTests = mapM_ goTest allTestCases

main = runTests
