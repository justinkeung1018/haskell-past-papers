module Tests where

import IC.TestSuite

import XML

instance Reformat XML where
  reformat = show

skipSpaceTestCases
  = [ "\n \n\nsome \n \n text" ==> "some \n \n text"
    ]

getAttributeTestCases
  = [ ("x", x2) ==> "1",
      ("x", Text "t") ==> "",
      ("dassaoidjas", x2) ==> ""
    ]

getChildrenTestCases
  = [ ("b", x2) ==> [Element "b" [] [Text "A"],Element "b" [] [Text "B"]],
      ("c", x2) ==> [],
      ("a", Text "d") ==> []
    ]

getChildTestCases
  = [ ("b", x2) ==> Element "b" [] [Text "A"],
      ("c", x2) ==> Text ""
    ]

getValueTestCases
  = [ x1 ==> Text "A",
      x2 ==> Text "AB"
    ]

popAndAddTestCases
  = [ [x1, Element "ab" [("a","1")] [], sentinel] ==> [Element "ab" [("a","1")] [Element "a" [] [Text "A"]],Element "" [] []]
    ]

parseAttributesTestCases
  = [ "x=\"7\">rest of text" ==> ([("x","7")],"rest of text"),
      "a = \"0\" b = \"1\" >rest of text" ==> ([("a","0"),("b","1")],"rest of text")
    ]

parseTestCases
  = [ s1 ==> x1,
      s2 ==> x2,
      s3 ==> x3,
      casablanca ==> casablancaParsed,
      films ==> filmsParsed
    ]

expandXSLTestCases
  = [ -- value-of
      (xsl1Parsed, x1) ==> [Text ""],
      (xsl1Parsed, x3) ==> [Text "text1"],
      (xsl2Parsed, x3) ==> [Text "text1text2"],
      (xsl4Parsed, x3) ==> [Text "att1"],
      (xsl5Parsed, x3) ==> [Text "text1"],

      -- for-each
      (xsl7Parsed, x3) ==> [Text "att1",Text "att2",Text "att3"]
    ]

allTestCases
  = [ TestCase  "skipSpace"          (skipSpace)
                                     skipSpaceTestCases

    , TestCase  "getAttribute"       (uncurry getAttribute)
                                     getAttributeTestCases    

    , TestCase  "getChildren"        (uncurry getChildren)
                                     getChildrenTestCases      

    , TestCase  "getChild"           (uncurry getChild)
                                     getChildTestCases   

    , TestCase  "getValue"           (getValue)
                                     getValueTestCases   

    , TestCase  "popAndAdd"          (popAndAdd)
                                     popAndAddTestCases                 

    , TestCase  "parseAttributes"    (parseAttributes)
                                     parseAttributesTestCases         

    , TestCase  "parse"              (parse)
                                     parseTestCases         

    , TestCase  "expandXSL"          (uncurry expandXSL)
                                     expandXSLTestCases     
    ]

runTests = mapM_ goTest allTestCases

main = runTests
