module Tests where
import IC.TestSuite
import Parser

precedenceTestCases
  = [ '$' ==> 0,
      ')' ==> 1,
      ')' ==> 1,
      '+' ==> 6,
      '-' ==> 6,
      '*' ==> 7,
      '/' ==> 7,
      '^' ==> 8
    ]

associativityTestCases
  = [ '^' ==> R
    ]

supersedesTestCases
  = [ ('+', '-') ==> False,
      ('*', '^') ==> False,
      ('^', '^') ==> True
    ]

allVarsTestCases
  = [ "7*x1+y2-b+x1" ==> ["x1","y2","b"]
    ]

tokeniseTestCases
  = [ "force^2" ==> [TVar "force",TOp '^',TNum 2],
      "5-8*7" ==> [TNum 5,TOp '-',TNum 8,TOp '*',TNum 7],
      "a + b^ 3 \t - \n 6" ==> [TVar "a",TOp '+',TVar "b",TOp '^',TNum 3,
      TOp '-',TNum 6],
      "5*(x-y)" ==> [TNum 5,TOp '*',TOp '(',TVar "x",TOp '-',TVar "y",TOp ')']
    ]

expParserTestCases
  = [ s1 ==> EApp '+' (ENum 1) (EApp '*' (ENum 7) (ENum 9))
    ]

allTestCases
  = [
      TestCase "precedence" precedence
              precedenceTestCases
    , TestCase "associativity" associativity
              associativityTestCases
    , TestCase "supersedes" (uncurry supersedes)
              supersedesTestCases
    , TestCase "allVars" allVars
              allVarsTestCases
    , TestCase "tokenise" tokenise
              tokeniseTestCases
    , TestCase "expParser" expParser
              expParserTestCases
    ]


runTests = mapM_ goTest allTestCases

main = runTests
