module Tests where

import IC.TestSuite

import BinomialHeaps

combineTreesTestCases
  = [ (t5, t6) ==> t7,
      (Node 0 0 [], Node 0 0 []) ==> Node 0 1 [Node 0 0 []],
      (Node 1 1 [Node 0 0 []], Node 1 1 [Node 1 0 []]) ==> Node 1 2 [Node 1 1 [Node 0 0 []], Node 1 0 []]
    ]

extractMinTestCases
  = [ h3 ==> 1
    ]

mergeHeapsTestCases
  = [ (h4, h5) ==> h6
    ]

insertTestCases
  = [ (7, [t1]) ==> [Node 4 1 [Node 7 0 []]]
    ]

deleteMinTestCases
  = [ h6 ==> h7
    ]

binSortTestCases
  = [ ]

allTestCases
  = [ TestCase  "combineTrees"    (uncurry combineTrees)
                                  combineTreesTestCases

    , TestCase  "extractMin"      (extractMin)
                                  extractMinTestCases            
                                  
    , TestCase  "mergeHeaps"      (uncurry mergeHeaps)
                                  mergeHeapsTestCases   

    , TestCase  "insert"          (uncurry insert)
                                  insertTestCases  

    , TestCase  "deleteMin"       (deleteMin)
                                  deleteMinTestCases   
    ]

runTests = mapM_ goTest allTestCases

main = runTests
