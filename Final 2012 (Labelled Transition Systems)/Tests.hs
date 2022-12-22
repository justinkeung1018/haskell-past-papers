module Tests where

import IC.TestSuite

import TransitionSystems

instance Reformat Process where
  reformat = show

statesTestCases
  = [ vendorLTS ==> [0, 1, 2, 3],
      clockLTS ==> [0, 1],
      playLTS ==> [0, 1, 2],
      clockPlayLTS ==> [0, 1, 4, 3, 5, 2],
      makerLTS ==> [0, 1]
    ]

transitionsTestCases
  = [ (0, vendorLTS) ==> [((0,1),"off"),((0,2),"blue"),((0,3),"red")]
    ]

alphabetTestCases
  = [ playLTS ==> ["end", "think", "move"]
    ]

actionsTestCases
  = [ (snd maker) ==> ["make", "ready"],
      (snd play) ==> ["think", "move", "end"]
    ]

acceptsTestCases
  = [ (["on", "off", "on"], [switch, on, off]) ==> True,
      (["use", "use"], [user]) ==> False,
      ([], [user]) ==> True
    ]

m = [((0,0),0),((0,1),1),((1,0),2),((1,1),3)]
composeTransitionsTestCases
  = [ (((0,1),"a"), ((0,1),"c"), ["a"], ["b","c"], m) ==> [((0,2),"a"),((0,1),"c")],
      (((1,1),"a"), ((1,0),"c"), ["a"], ["a","c"], m) ==> [((3,2),"c")],
      (((0,1),"a"), ((1,1),"c"), ["a","c"], ["a","c"], m) ==> []
    ]

pruneTransitionsTestCases
  = [ [((0,1),"d"),((1,4),"a"),((2,5),"a")] ==> [((0,1),"d"),((1,4),"a")]
    ]

composeTestCases
  = [ (clockLTS, playLTS) ==> clockPlayLTS,
      (pLTS, qLTS) ==> pqLTS,
      (makerLTS, userLTS) ==> makerUserLTS,
      (makerLTS, userLTS') ==> makerUserLTS'
    ]

buildLTSTestCases
  = [ [switch, on, off] ==> [((0,1),"off"),((1,2),"on"),((2,1),"off")]
    ]

allTestCases
  = [ TestCase  "states"          (states)
                                  statesTestCases

    , TestCase "transitions"      (uncurry transitions)
                                  transitionsTestCases

    , TestCase "alphabet"         (alphabet)
                                  alphabetTestCases

    , TestCase "actions"          (actions)
                                  actionsTestCases

    , TestCase "accepts"            (uncurry accepts)
                                    acceptsTestCases        

    , TestCase "composeTransitions" (uncurry5 composeTransitions)
                                    composeTransitionsTestCases   

    , TestCase "pruneTransitions"   (pruneTransitions)
                                    pruneTransitionsTestCases  

    , TestCase "compose"            (uncurry compose)
                                    composeTestCases         

    , TestCase "buildLTS"           (buildLTS)
                                    buildLTSTestCases                          
    ]

runTests = mapM_ goTest allTestCases

main = runTests
