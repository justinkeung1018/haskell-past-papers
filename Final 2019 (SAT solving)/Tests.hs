module Tests where

import IC.TestSuite

import SAT
import Types
import TestData

instance Reformat Formula where
  reformat = show

varsTestCases
  = [ f1 ==> ["a"],
      f2 ==> ["a"],
      f3 ==> ["a","b"],
      f4 ==> ["a","b"],
      f5 ==> ["a","b","c"],
      f6 ==> ["a","b","c"],
      f7 ==> ["a","b"],
      f8 ==> ["a","b","c","d"],
      f9 ==> ["a","b","c","d"],
      cnf3 ==> ["a","b","c","d","e","f","g"]
    ]

idMapTestCases
  = [ cnf3 ==> [("a",1),("b",2),("c",3),("d",4),("e",5),("f",6),("g",7)]
    ]

toNNFTestCases
  = [ f1 ==> a,
      f6 ==> And (Not b) (Or (Not a) c),
      f8 ==> Or (And a (And (Not b) (Not c))) (Not d)
    ]

toCNFTestCases
  = [ f1 ==> c1,
      f2 ==> c2,
      f3 ==> c3,
      f4 ==> c4,
      f5 ==> c5,
      f6 ==> c6,
      f7 ==> c7,
      f8 ==> c8,
      f9 ==> c9
    ]

flattenTestCases
  = [ cnf0 ==> cnf0Rep,
      cnf1 ==> cnf1Rep,
      cnf2 ==> cnf2Rep,
      cnf3 ==> cnf3Rep,
      cnf4 ==> cnf4Rep
    ]

propUnitsTestCases
  = [ cnf1Rep ==> ([[1,2,3],[1,2,-3],[1,-2,3],[1,-2,-3],[-1,2,3],[-1,2,-3],[-1,-2,3]],[]),
      cnf2Rep ==> ([[2,5],[3,-2],[3,-5],[-3,-7],[7,6],[-3,6]],[4,-1])
    ]

dpTestCases
  = [ cnf1Rep ==> [[1,2,3]],
      cnf2Rep ==> [[4,-1,2,3,-7,6],[4,-1,-2,5,3,-7,6]],
      cnf3Rep ==> [],
      cnf4Rep ==> [[-1,2,-4,3],[1,-2,-3,4]]
    ]

allSatTestCases
  = [ cnf1 ==> [[("a",True),("b",True),("c",True)]],
      cnf2 ==> [[("a",False),("b",True),("c",True),("d",True),("e",False),("f",True),("g",False)],
                [("a",False),("b",True),("c",True),("d",True),("e",True),("f",True),("g",False)],
                [("a",False),("b",False),("c",True),("d",True),("e",True),("f",True),("g",False)]],
      cnf3 ==> [],
      cnf4 ==> [[("a",False),("b",True),("c",True),("d",False)],
                [("a",True),("b",False),("c",False),("d",True)]]
    ]

allTestCases
  = [ TestCase  "vars"               (vars)
                                     varsTestCases

    , TestCase  "idMap"              (idMap)
                                     idMapTestCases

    , TestCase  "toNNF"              (toNNF)
                                     toNNFTestCases

    , TestCase  "toCNF"              (toCNF)
                                     toCNFTestCases

    , TestCase  "flatten"            (flatten)
                                     flattenTestCases

    , TestCase  "propUnits"          (propUnits)
                                     propUnitsTestCases

    , TestCase  "dp"                 (dp)
                                     dpTestCases

    , TestCase  "allSat"             (allSat)
                                     allSatTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
