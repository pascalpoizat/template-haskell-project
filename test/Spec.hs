-----------------------------------------------------------------------------
-- |
-- Module      :  Spec
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- Main test file for the library.
-----------------------------------------------------------------------------

import           Lib
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Tests" [maintests]

maintests :: TestTree
maintests = testGroup "unit tests" [uIsOrigin]

uIsOrigin :: TestTree
uIsOrigin =
  testGroup "isOrigin"
            [(testCase "at origin" $ (isOrigin (Point 0 0)) @?= True)
            ,(testCase "not at origin, x not 0" $
              (isOrigin (Point 1 0)) @?= False)
            ,(testCase "not at origin, y not 0" $
              (isOrigin (Point 0 1)) @?= False)
            ,(testCase "not at origin, x and y not 0" $
              (isOrigin (Point 2 3)) @?= False)]
