module Main (main) where

-- base
import Data.Proxy (Proxy (..))

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- fused-effects
import Control.Algebra (Has)

-- changeset
import Control.Monad.Changeset.Class (MonadChangeset (..))
import Control.Monad.Trans.Changeset (Changes, Count (Increment), getChangeset, singleChange)

-- changeset-fused-effects
import Control.Effect.Changeset (Changeset, sendChange, sendCurrent)

fusedEffects :: (Has (Changeset Int (Changes Count)) sig m) => m Int
fusedEffects = do
  sendChange (Proxy @Int) $ singleChange Increment
  n <- sendCurrent (Proxy @(Changes Count))
  sendChange (Proxy @Int) $ singleChange Increment
  pure n

transformer :: (MonadChangeset Int (Changes Count) m) => m Int
transformer = do
  change $ singleChange Increment
  n <- current
  change $ singleChange Increment
  pure n

main :: IO ()
main =
  defaultMain
    $ testCase
      "fused-effects"
    $ let
       in getChangeset fusedEffects (0 :: Int) @?= getChangeset transformer (0 :: Int)
