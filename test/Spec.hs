import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Tests" [maintests]

maintests :: TestTree
maintests = testGroup "unit tests" [uEq, uShow, uIsOrigin, uDistance]

uEq :: TestTree
uEq =
  testGroup
    "eq"
    [ testCase "same x and y" $ (Point 2 3 == Point 2 3) @?= True,
      testCase "same x, different y" $ (Point 2 3 == Point 2 5) @?= False,
      testCase "same y, different x" $ (Point 2 3 == Point 4 3) @?= False,
      testCase "different x and y" $ (Point 2 3 == Point 4 5) @?= False
    ]

uShow :: TestTree
uShow =
  testGroup "show" [testCase "some point" $ show (Point 2 3) @?= "(2, 3)"]

uIsOrigin :: TestTree
uIsOrigin =
  testGroup
    "isOrigin"
    [ testCase "at origin" $ isOrigin (Point 0 0) @?= True,
      testCase "not at origin, x not 0" $ isOrigin (Point 1 0) @?= False,
      testCase "not at origin, y not 0" $ isOrigin (Point 0 1) @?= False,
      testCase "not at origin, x and y not 0" $ isOrigin (Point 2 3) @?= False
    ]

uDistance :: TestTree
uDistance =
  testGroup
    "distance"
    [testCase "same location" $ distance (Point 2 3) (Point 2 3) @?= 0.0]
