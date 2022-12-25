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
  = [ [1, 3, 6, 2, 7, 8, 9, 10, 5, 4] ==> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
      [3, 1, 1, 1, 2] ==> [1, 1, 1, 2, 3],
      [5, 4, 3, 2, 1] ==> [1, 2, 3, 4, 5],
      [] ==> []
      ]

toBinaryTestCases
  = [ h2 ==> [1, 1, 0, 0]
    ]

binarySumTestCases
  = [ ([0], [0]) ==> [0],
      ([0], [1]) ==> [1],
      ([1], [0]) ==> [1],
      ([1], [1]) ==> [1, 0],
      ([1], [1, 1, 1, 1, 1]) ==> [1, 0, 0, 0, 0, 0]
    ]

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

    , TestCase  "binSort"         (binSort)
                                  binSortTestCases   

    , TestCase  "toBinary"        (toBinary)
                                  toBinaryTestCases   

    , TestCase  "binarySum"       (uncurry binarySum)
                                  binarySumTestCases                               
    ]

runTests = mapM_ goTest allTestCases

main = runTests
