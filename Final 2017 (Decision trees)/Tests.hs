module Tests where

import IC.TestSuite

import DecisionTrees

instance Reformat DecisionTree where
  reformat = show

instance Reformat DataSet where
  reformat = show

allSameTestCases
  = [ [] ==> True,
      [9, 9, 9] ==> True,
      [9, 8] ==> False
    ]

removeTestCases
  = [ (1, [(3, 'a'), (1, 'b'), (7, 'a')]) ==> [(3, 'a'), (7, 'a')],
      (6, []) ==> []
    ]

lookUpAttTestCases
  = [ ("temp", header, head table) ==> "hot"
    ]

removeAttTestCases
  = [ ("temp", header, head table) ==> ["sunny","high","calm","bad"]
    ]

addToMappingTestCases
  = [ ((1, 'a'), [(2, "b")]) ==> [(2, "b"), (1, "a")],
      ((5, 'a'), [(2, "b"), (5, "bcd")]) ==> [(2, "b"), (5, "abcd")]
    ]

buildFrequencyTableTestCases
  = [ (result, fishingData) ==> [("good",9),("bad",5)],
      (outlook, fishingData) ==> [("sunny",5),("overcast",4),("rainy",5)],
      (outlook, ([],[])) ==> [("sunny",0),("overcast",0),("rainy",0)]
    ]

nodesTestCases
  = [ fig1 ==> 18,
      fig2 ==> 8
    ]

evalTreeTestCases
  = [ (fig1, header, table !! 5) ==> "bad",
      (fig2, header, table !! 5) ==> "bad",
      (fig1, header, table !! 4) ==> "good",
      (fig2, header, table !! 4) ==> "good"
    ]

partitionDataTestCases
  = [ (fishingData, outlook) ==> outlookPartition
    ]

-- Need to make AttSelector an instance of Show
buildTreeTestCases
  = [ (fishingData, result, nextAtt) ==> fig1,
      (fishingData, result, bestGainAtt) ==> fig2
    ]

entropyTestCases
  = [ (fishingData, result) ==> 0.9402859586706309,
      (fishingData, temp) ==> 1.5566567074628228,
      ((header, []), result) ==> 0.0
    ]

gainTestCases
  = [ (fishingData, outlook, result) ==> 0.2467498197744391,
      (fishingData, temp, result) ==> 2.9222565658954647e-2,
      (fishingData, humidity, result) ==> 0.15183550136234136,
      (fishingData, wind, result) ==> 4.812703040826927e-2
    ]
    
bestGainAttTestCases
  = [ (fishingData, result) ==> ("outlook",["sunny","overcast","rainy"])
    ]

allTestCases
  = [ TestCase  "allSame"              (allSame)
                                       allSameTestCases

    , TestCase  "remove"               (uncurry remove)
                                       removeTestCases     

    , TestCase  "lookUpAtt"            (uncurry3 lookUpAtt)
                                       lookUpAttTestCases    

    , TestCase  "removeAtt"            (uncurry3 removeAtt)
                                       removeAttTestCases   

    , TestCase  "addToMapping"         (uncurry addToMapping)
                                       addToMappingTestCases       

    , TestCase  "buildFrequencyTable"  (uncurry buildFrequencyTable)
                                       buildFrequencyTableTestCases

    , TestCase  "nodes"                (nodes)
                                       nodesTestCases   
                                       
    , TestCase  "evalTree"             (uncurry3 evalTree)
                                       evalTreeTestCases

    , TestCase  "partitionData"        (uncurry partitionData)
                                       partitionDataTestCases        

    , TestCase  "entropy"              (uncurry entropy)
                                       entropyTestCases                   

    , TestCase  "gain"                 (uncurry3 gain)
                                       gainTestCases    

    , TestCase  "bestGainAtt"          (uncurry bestGainAtt)
                                       bestGainAttTestCases  
    ]

runTests = mapM_ goTest allTestCases

main = runTests
