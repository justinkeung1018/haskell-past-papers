module Tests where

import IC.TestSuite

import BDD

instance Reformat BExp where
  reformat = show

checkSatTestCases
  = [ (bdd2, [(1,True),(2,False)]) ==> True,
      (bdd7, [(3,True),(2,False),(9,True)]) ==> False
    ]

satTestCases
  = [ bdd1 ==> [],
      bdd2 ==> [[(1,False),(2,False)],[(1,False),(2,True)],[(1,True),(2,False)]],
      bdd8 ==> [[(1,False)],[(1,True)]]
    ]

simplifyTestCases
  = [ Not (Prim False) ==> Prim True,
      Or (Prim False) (Prim False) ==> Prim False,
      And (IdRef 3) (Prim True) ==> And (IdRef 3) (Prim True)
    ]

restrictTestCases
  = [ (b7, 2, True) ==> Or (Not (IdRef 3)) (Or (Prim True) (Not (IdRef 9))),
      (restrict b7 2 True, 9, False) ==> Or (Not (IdRef 3)) (Prim True),
      (restrict (restrict b7 2 True) 9 False, 3, True) ==> Prim True
    ]

buildBDDTestCases
  = [ (b1, []) ==> sortBDD bdd1,
      (b2, [1, 2]) ==> sortBDD bdd2,
      (b3, [1])  ==> sortBDD bdd3,
      (b4, [2, 3, 7])  ==> sortBDD bdd4,
      (b5, [2, 3, 7]) ==> sortBDD bdd5,
      (b6, [1, 2, 3, 4]) ==> sortBDD bdd6,
      (b7, [2, 3, 9]) ==> sortBDD bdd7,
      (b8, [1]) ==> sortBDD bdd8
    ]

allTestCases
  = [ TestCase  "checkSat"           (uncurry checkSat)
                                     checkSatTestCases

    , TestCase  "sat"                (sat)
                                     satTestCases

    , TestCase  "simplify"           (simplify)
                                     simplifyTestCases

    , TestCase  "restrict"           (uncurry3 restrict)
                                     restrictTestCases

    , TestCase  "buildBDD"           (sortBDD . uncurry buildBDD)
                                     buildBDDTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
