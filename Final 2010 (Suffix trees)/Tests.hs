module Tests where

import IC.TestSuite

import SuffixTrees

import Data.List

instance Reformat SuffixTree where
  reformat = show

isPrefixTestCases
  = [ ("has", "haskell") ==> True,
      ("", "haskell") ==> True,
      ("haskell", "has") ==> False,
      ("ask", "haskell") ==> False
    ]

removePrefixTestCases
  = [ ("ja", "java") ==> "va",
      (" ", " java") ==> "java",
      ("", "java") ==> "java"
    ]

suffixesTestCases
  = [ "perl" ==> ["perl","erl","rl","l"],
      "" ==> [],
      "l" ==> ["l"]
    ]

isSubstringTestCases
  = [ ("ho", "python") ==> True,
      ("thong", "python") ==> False,
      ("", "python") ==> True,
      ("", "") ==> True,
      ("python", "") ==> False
    ]

findSubstringsTestCases
  = [ ("an", "banana") ==> [1,3],
      ("s", "mississippi") ==> [2,3,5,6]
    ]

getIndicesTestCases
  = [ Node [("",Leaf 4),("",Leaf 1)] ==> [1,4],
      t1 ==> [0,1,2,3,4,5]
    ]

findSubstrings'TestCases
  = [ ("an", t1) ==> [1,3],
      ("", t1) ==> [0,1,2,3,4,5], 
      ("s", t2) ==> [2,3,5,6]
    ]

insertTestCases
  = [ ("an", buildTree s1) ==> [1,3],
      ("", buildTree s1) ==> [0,1,2,3,4,5],
      ("s", buildTree s2) ==> [2,3,5,6],
      ("lol", buildTree "lolololololol") ==> [0,2,4,6,8,10]
    ]

allTestCases
  = [ TestCase  "isPrefix"           (uncurry isPrefix)
                                     isPrefixTestCases

    , TestCase  "removePrefix"       (uncurry removePrefix)
                                     removePrefixTestCases

    , TestCase  "suffixes"           (suffixes)
                                     suffixesTestCases

    , TestCase  "isSubstring"        (uncurry isSubstring)
                                     isSubstringTestCases

    , TestCase  "findSubstrings"     (uncurry findSubstrings)
                                     findSubstringsTestCases

    , TestCase  "getIndices"         (sort . getIndices)
                                     getIndicesTestCases

    , TestCase  "findSubstrings\'"   (sort . uncurry findSubstrings')
                                     findSubstrings'TestCases

    , TestCase  "insert"             (sort . uncurry findSubstrings')
                                     insertTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
