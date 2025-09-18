module Main (main) where

-- base
import Control.Monad (replicateM_)
import Data.Char (toUpper)
import Data.Function ((&))
import Prelude hiding (Foldable (..))

-- transformers
import Control.Monad.Trans.Reader (ReaderT (..), ask)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- containers
import qualified Data.Map as M

-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset
import Data.Monoid (Last (Last))
import Data.Monoid.RightAction (RightAction (..), rEndo, set)
import Data.Monoid.RightAction.Coproduct (inL, (:+:))

type M = Changeset Int (Changes Count)

main :: IO ()
main =
  defaultMain $
    testGroup
      "changeset"
      [ testGroup
          "Changeset"
          [ testGroup
              "commutative monoids"
              [ testGroup
                  "Order of change and current matters"
                  [ testCase "change, current" $
                      evalChangeset (changeSingle Increment >> current) 0 @?= (1 :: Int)
                  , testCase "current, change" $
                      let action = flip evalChangeset 0 $ do
                            n <- current
                            changeSingle Increment
                            return n
                       in action @?= (0 :: Int)
                  ]
              , testGroup
                  "execChangeset"
                  [ testCase "pure doesn't change state" $ execChangeset (pure () :: M ()) 0 @?= 0
                  , testCase "change changes state" $ execChangeset (changeSingle Increment :: M ()) 0 @?= 1
                  ]
              ]
          , testGroup
              "noncommutative monoids"
              [ testGroup
                  "Changes"
                  [ testCase "change is monoid homomorphism" $ do
                      execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= execChangeset (change (singleChange (Cons True) <> singleChange (Cons False))) ([] :: [Bool])
                      execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= execChangeset (change (addChange (Cons False) (singleChange (Cons True)))) ([] :: [Bool])
                      execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= execChangeset (change (changes [Cons True, Cons False])) ([] :: [Bool])
                      execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= ([False, True] :: [Bool])
                  , testCase "execChangeset is monoid homomorphism" $
                      execChangeset (changeSingle (Cons True) >> changeSingle (Cons False)) [] @?= (([] :: [Bool]) & execChangeset (changeSingle (Cons True)) & execChangeset (changeSingle (Cons False)))
                  ]
              ]
          ]
      , testGroup
          "Changes"
          [ testCase "is lawful monoid action" $ do
              [] `actRight` singleChange (Cons True) `actRight` singleChange (Cons False) @?= ([] :: [Bool]) `actRight` singleChange (Cons True) <> singleChange (Cons False)
          ]
      , testGroup
          "MonadChangeset"
          [ testCase "ReaderT lifts changeset operations" $
              let action = flip execChangeset (0 :: Int) $ flip runReaderT (100 :: Int) $ do
                    env <- ask
                    replicateM_ env $ changeSingle Increment
               in action @?= 100
          ]
      , testGroup
          "Coproduct"
          [ testCase ":+: is monoid morphism" $
              (0 :: Int) `actRight` (inL (Last (Just 1)) <> inL (Last (Just 2)) :: Last Int :+: Last Int) @?= 0 `actRight` (inL (Last (Just (1 :: Int)) <> Last (Just 2)) :: Last Int :+: Last Int)
          ]
      , testGroup
          "FilterableChange"
          [ testCase "Can change a map with FilterableChange" $
              M.fromList [(0 :: Int, "hello"), (1, "world")] `actRight` FilterableChange (justChange $ ImapChange (\i -> if i == 2 then rEndo toUpper else mempty))
                @?= M.fromList [(0 :: Int, "heLlo"), (1, "woRld")]
          ]
      , testGroup
          "FilterableChanges"
          [ testCase "Can change a map depending on content" $
              M.fromList [(0 :: Int, "hello"), (1, "world"), (2, "!")]
                `actRight` FilterableChanges
                  ( \case
                      "hello" -> FilterablePositionChange (set "hi")
                      "!" -> FilterablePositionDelete
                      _ -> mempty
                  )
                @?= M.fromList [(0 :: Int, "hi"), (1, "world")]
          ]
      , testGroup
          "AlignChanges"
          [ testCase "Can change a map with another map" $
              M.fromList
                [ (0 :: Int, "hello")
                , (1, "world")
                , (2, "!")
                ]
                `actRight` AlignChanges
                  ( M.fromList
                      [ (0 :: Int, SetAlignPosition "hi")
                      , (1, ChangeAlignPosition $ FmapChange @[] $ rEndo toUpper)
                      , (2, DeleteAlignPosition)
                      , (3, SetAlignPosition "...")
                      ]
                  )
                @?= M.fromList [(0 :: Int, "hi"), (1, "WORLD"), (3, "...")]
          ]
      ]
