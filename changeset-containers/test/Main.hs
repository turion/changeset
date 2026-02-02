module Main (main) where

-- base
import Prelude hiding (Foldable (..))

-- tasty
import Test.Tasty

-- changeset-containers-test
import IntMap qualified
import IntSet qualified
import Map qualified
import Sequence qualified
import Set qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "changeset-containers"
      [ IntMap.tests
      , Map.tests
      , IntSet.tests
      , Set.tests
      , Sequence.tests
      ]
