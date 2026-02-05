module Sequence where

-- base

import Control.Monad (when)
import Data.Foldable (length)
import Data.Functor ((<&>))
import Prelude hiding (Foldable (..), sequence)

-- containers

import Data.Sequence (Seq, fromList, singleton, (<|))
import Data.Sequence qualified as Seq

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- falsify

import Test.Falsify.Generator hiding (enum)
import Test.Falsify.Predicate hiding (between)
import Test.Falsify.Range
import Test.Tasty.Falsify

-- data-default
import Data.Default

-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset (changeSingle, changes, execChangeset, runChangeset)
import Data.Monoid.RightAction

-- changeset-containers
import Data.Monoid.RightAction.Sequence

tests :: TestTree
tests =
  testGroup
    "Sequence"
    [ testGroup
        "SeqChange"
        [ testCase "Can insert" $
            runChangeset (changeSingle (Cons 0 :: SeqChange Int)) mempty @?= ((), singleton (0 :: Int))
        , testCase "Can read after insert" $
            let action = do
                  changeSingle (Cons 1 :: SeqChange Int)
                  m <- current
                  changeSingle $ Cons 0
                  return m
             in runChangeset action mempty @?= (singleton (1 :: Int), fromList [0, 1])
        , testCase "Can delete after insert" $
            let action = do
                  changeSingle (Cons 1 :: SeqChange Int)
                  m <- current
                  changeSingle $ Cons 0
                  changeSingle $ Snoc 3
                  changeSingle $ Snoc 99
                  changeSingle Unsnoc
                  return m
             in execChangeset action (singleton (2 :: Int)) @?= fromList [0, 1, 2, 3]
        ]
    , testGroup
        "SeqEdit"
        [ testProperty "shift" $ do
            s <- gen sequence
            edit <- gen seqEdit
            a <- gen string
            assert $
              eq
                .$ ("a <| actRight s edit", a <| actRight s edit)
                .$ ("actRight (a <| s) (shift edit)", actRight (a <| s) (shift edit))
        , testProperty "Torsor law 1 semantic" $ do
            s <- gen sequence
            edit <- gen editScript
            info $ show ("s `differenceRight` (s `actRight` edit)", s `differenceRight` (s `actRight` edit) :: EditScript String)
            assert $
              eq
                .$ ("s `actRight` (s `differenceRight` (s `actRight` edit)", s `actRight` (s `differenceRight` (s `actRight` edit) :: EditScript String))
                .$ ("s `actRight` edit", s `actRight` (edit :: EditScript String))
        , testProperty "Torsor law 2" $ do
            sOrig <- gen sequence
            sActed <- gen sequence
            info $ show ("sOrig `differenceRight` sActed", sOrig `differenceRight` sActed :: EditScript String)
            assert $
              eq
                .$ ("sOrig `actRight` (sOrig `differenceRight` sActed)", sOrig `actRight` (sOrig `differenceRight` sActed :: EditScript String))
                .$ ("sActed", sActed)
        , testProperty "Torsor law 3" $ do
            s <- gen sequence
            assert $
              eq
                .$ ("s `differenceRight` s", s `differenceRight` s :: EditScript String)
                .$ ("mempty", mempty :: EditScript String)
        , testProperty "Subsequence is recognized" $ do
            s <- gen sequence
            insertions <- gen $ changes <$> list (between (0, 100)) insertAt
            let sBigger = s `actRight` insertions
            info $ show ("sBigger", sBigger)
            assert $
              satisfies
                ( "all DeleteAt"
                , all
                    ( \case
                        DeleteAt _ -> True
                        _ -> False
                    )
                    . getEditScript
                )
                .$ ("sBigger `differenceRight` s", sBigger `differenceRight` s :: EditScript String)
            assert $
              satisfies
                ( "s `differenceRight` sBigger"
                , all
                    ( \case
                        InsertAt _ _ -> True
                        _ -> False
                    )
                    . getEditScript
                )
                .$ ("s `differenceRight` sBigger", s `differenceRight` sBigger :: EditScript String)
        , testPropertyWith def {overrideMaxRatio = Just 10000} "Edit script is minimal" $ do
            sOrig <- gen sequence
            sActed <- gen sequence
            edit <- gen editScript
            let minimalEdit = sOrig `differenceRight` sActed
            info $ show ("sOrig `actRight` edit", sOrig `actRight` edit)
            when (sOrig `actRight` edit /= sActed) discard
            assert $
              (le `on` transparent length)
                .$ ("minimalEdit", minimalEdit)
                .$ ("edit", edit)
        ]
    ]

sequence :: Gen (Seq String)
sequence = list (between (0, 10)) string <&> Seq.fromList

string :: Gen String
string = list (between (0, 3)) (inRange (enum ('a', 'z')))

deleteAt :: Gen (SeqEdit String)
deleteAt = DeleteAt <$> int (between (0, 10))

insertAt :: Gen (SeqEdit String)
insertAt = InsertAt <$> int (between (0, 10)) <*> string

seqEdit :: Gen (SeqEdit String)
seqEdit = choose deleteAt insertAt

editScript :: Gen (EditScript String)
editScript = EditScript . changes <$> list (between (0, 10)) seqEdit
