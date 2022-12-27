module Tests where

import IC.TestSuite

import RegEx

instance Reformat RE where
  reformat = show

transitionsFromTestCases
  = [ (5, ndaFigure) ==> [(5,7,Eps),(5,9,Eps)],
      (10, nda3) ==> [(10,6,C 'b')],
      (10000000, nda3) ==> [] 
    ]

labelsTestCases
  = [ [(1,2,Eps)] ==> [],
      transitions nda3 ==> [C 'a',C 'c',C 'b']
    ]

acceptsTestCases
  = [ (ndaFigure, "c") ==> True,
      (ndaFigure, "") ==> False,
      (ndaFigure, "ac") ==> True,
      (ndaFigure, "bc") ==> True,
      (ndaFigure, "aac") ==> True,
      (ndaFigure, "cc") ==> False,
      (ndaFigure, "ad") ==> False,
      (ndaFigure, "a") ==> False,

      (nda1, "x2") ==> True,
      (nda1, "y1") ==> True,
      (nda1, "x") ==> False,
      (nda1, "x3") ==> False,

      (nda2, "x") ==> True,
      (nda2, "x\'") ==> True,
      (nda2, "x\'\'\'") ==> True,
      (nda2, "y") ==> False,
      (nda2, "\'") ==> False,
      (nda2, "x\'x") ==> False,

      (nda3, "") ==> True,
      (nda3, "c") ==> True,
      (nda3, "abababccab") ==> True,
      (nda3, "d") ==> False,
      (nda3, "ac") ==> False,
      (nda3, "cccb") ==> False
    ]

makeNDATestCases
  = [ Term 'x' ==> (1,[2],[(1,2,C 'x')]),
      reFigure ==> ndaFigure,
      re1 ==> nda1,
      re2 ==> nda2,
      re3 ==> nda3,
      re4 ==> nda4,
      re5 ==> nda5
    ]

getFrontierTestCases
  = [ (1, nda3) ==> [(2,2,Eps),(5,9,C 'a'),(7,8,C 'c')]
    ]

groupTransitionsTestCases
  = [ [(2,2,Eps),(5,9,C 'a'),(7,8,C 'c')] ==> [(C 'a',[9]),(C 'c',[8])],
      [(3,7,C 'a'),(5,9,C 'a')] ==> [(C 'a',[7,9])]
    ]

allTestCases
  = [ TestCase  "transitionsFrom"    (uncurry transitionsFrom)
                                     transitionsFromTestCases

    , TestCase  "labels"             (labels)
                                     labelsTestCases            

    , TestCase  "accepts"            (uncurry accepts)
                                     acceptsTestCases            

    , TestCase  "makeNDA"            (makeNDA)
                                     makeNDATestCases    

    , TestCase  "getFrontier"        (uncurry getFrontier)
                                     getFrontierTestCases  

    , TestCase  "groupTransitions"   (groupTransitions)
                                     groupTransitionsTestCases  
    ]

runTests = mapM_ goTest allTestCases

main = runTests
