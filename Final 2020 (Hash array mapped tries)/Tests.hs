module Tests where

import IC.TestSuite

import Tries
import Types
import Examples
import HashFunctions

countOnesTestCases
  = [ 0 ==> 0,
      65219 ==> 11
    ]

countOnesFromTestCases
  = [ (0, 88) ==> 0,
      (3, 15) ==> 3
    ]

getIndexTestCases
  = [ (53767, 0, 4) ==> 7,
      (53767, 3, 4) ==> 13,
      (53767, 1, 8) ==> 210
    ]

replaceTestCases
  = [ (0, "trie", 'b') ==> "brie",
      (3, "trie", 'p') ==> "trip"
    ]

insertAtTestCases
  = [ (3, 'p', "trie") ==> "tripe",
      (4, 's', "trie") ==> "tries"
    ]

memberTestCases
  = [ (12, 12, figure, 4) ==> False,
      (73, 73, figure, 4) ==> True,
      (206, 206, figure, 4) ==> True,
      (31, 31, figure, 4) ==> False,
      (2521, 2521, figure, 4) ==> True,
      (206, hash 206, figureHashed, 4) ==> True
    ]



allTestCases
  = [ TestCase  "countOnes"          (countOnes)
                                     countOnesTestCases

    , TestCase  "countOnesFrom"      (uncurry countOnesFrom)
                                     countOnesFromTestCases

    , TestCase  "getIndex"           (uncurry3 getIndex)
                                     getIndexTestCases

    , TestCase  "replace"            (uncurry3 replace)
                                     replaceTestCases

    , TestCase  "insertAt"           (uncurry3 insertAt)
                                     insertAtTestCases

    , TestCase  "member"             (uncurry4 member)
                                     memberTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
