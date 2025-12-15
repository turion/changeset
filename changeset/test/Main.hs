module Main (main) where

-- base
import Control.Monad (replicateM_)
import Data.Char (toUpper)
import Data.Function ((&))
import Data.Monoid (Last (Last), Product (..), Sum (..))
import GHC.Generics (Generic)
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
import Data.Monoid.RightAction (RightAction (..), RightTorsor (..), rEndo, set)
import Data.Monoid.RightAction.Coproduct (inL, (:+:))
import Data.Monoid.RightAction.Generic (actRightGGeneric, actRightGeneric, differenceRightGGeneric, differenceRightGenericChanges)

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
      , testGroup
          "Generic"
          [ testGroup
              "actRightGeneric"
              [ testCase "single change left" $ 0 `actRight` IntActionCounts Increment mempty @?= (1 :: Int)
              , testCase "single change product" $ 0 `actRight` IntActionCounts Increment (singleChange Increment) @?= (2 :: Int)
              , testCase "single change right" $ 0 `actRight` IntActionLast (set 100) @?= (100 :: Int)
              , testCase "single change recursive" $ 0 `actRight` IntActionRec (IntActionLast (set 100)) (IntActionCounts Increment mempty) @?= (101 :: Int)
              , testCase "changes" $
                  (0 :: Int)
                    `actRight` changes
                      [ IntActionCounts Increment mempty
                      , IntActionLast (set 10)
                      , IntActionCounts Increment (changes [Increment, Increment])
                      , IntActionRec (IntActionRec (IntActionCounts Increment mempty) (IntActionCounts Increment mempty)) (IntActionCounts Increment (changes [Increment, Increment]))
                      ]
                    @?= 18
              ]
          , testGroup
              "actRightGGeneric"
              [ testCase "Wrapper changes wrapper" $ Bar 0 `actRight` BarChange Increment @?= Bar 1
              , testCase "Sum changes product" $ Foo 0 0 [] `actRight` changes [FooChangeInt Increment, FooChangeList (Cons ()), FooChangeInt2 Increment] @?= Foo 1 1 [()]
              , testCase "Product changes product" $ Foo 0 0 [] `actRight` FooChange2 Increment Nothing (Cons ()) @?= Foo 1 0 [()]
              ]
          , testGroup
              "differenceRightGGeneric"
              [ testGroup "Wrapper" $ torsorTestSuite (TorsorWrapper 9) (TorsorWrapper 1) (TorsorWrapper 10)
              , testGroup
                  "Sum"
                  [ testGroup "All fields change" $ torsorTestSuite (changes [TorsorChangeSum 5, TorsorChangeProduct 6, TorsorChangeSum2 7]) (Torsor 2 3 4) (Torsor 7 18 11)
                  , testGroup "One field changes" $ torsorTestSuite (singleChange (TorsorChangeProduct 6)) (Torsor 2 3 4) (Torsor 2 18 4)
                  , testGroup "No fields change" $ torsorTestSuite (mempty :: Changes TorsorChange) (Torsor 2 3 4) (Torsor 2 3 4)
                  ]
              , testGroup "Product" $ torsorTestSuite (TorsorChange2 5 6 7) (Torsor 2 3 4) (Torsor 7 18 11)
              ]
          ]
      ]

data IntAction
  = IntActionCounts Count (Changes Count)
  | IntActionLast (Last Int)
  | IntActionRec IntAction IntAction
  deriving stock (Generic)

instance RightAction IntAction Int where
  actRight = actRightGeneric

newtype Bar = Bar Int
  deriving stock (Generic, Eq, Show)

newtype BarChange = BarChange Count
  deriving stock (Generic)

instance RightAction BarChange Bar where
  actRight = actRightGGeneric

data Foo = Foo Int Int [()]
  deriving stock (Generic, Eq, Show)

data FooChange = FooChangeInt Count | FooChangeInt2 Count | FooChangeList (ListChange ())
  deriving stock (Generic)

instance RightAction FooChange Foo where
  actRight = actRightGGeneric

data FooChange2 = FooChange2 Count (Maybe Count) (ListChange ())
  deriving stock (Generic)

instance RightAction FooChange2 Foo where
  actRight = actRightGGeneric

newtype TorsorWrapper = TorsorWrapper (Sum Integer)
  deriving stock (Generic, Eq, Show)

instance RightAction TorsorWrapper TorsorWrapper where
  actRight = actRightGGeneric

instance RightTorsor TorsorWrapper TorsorWrapper where
  differenceRight = differenceRightGGeneric

data Torsor = Torsor (Sum Integer) (Product Rational) (Sum Rational)
  deriving stock (Generic, Eq, Show)

data TorsorChange = TorsorChangeSum (Sum Integer) | TorsorChangeProduct (Product Rational) | TorsorChangeSum2 (Sum Rational)
  deriving stock (Generic, Eq, Show)

instance RightAction TorsorChange Torsor where
  actRight = actRightGGeneric

instance RightTorsor (Changes TorsorChange) Torsor where
  differenceRight = differenceRightGenericChanges

data TorsorChange2 = TorsorChange2 (Sum Integer) (Product Rational) (Sum Rational)
  deriving stock (Generic, Eq, Show)

instance RightAction TorsorChange2 Torsor where
  actRight = actRightGGeneric

instance RightTorsor TorsorChange2 Torsor where
  differenceRight = differenceRightGGeneric

torsorTestSuite :: forall w s. (RightAction w s, RightTorsor w s, Eq w, Show w, Eq s, Show s) => w -> s -> s -> [TestTree]
torsorTestSuite w sOrig sActed =
  [ testCase "act" $ sOrig `actRight` w @?= sActed
  , testCase "difference" $ sOrig `differenceRight` sActed @?= w
  , testCase "law 1" $ sOrig `differenceRight` (sOrig `actRight` w :: s) @?= w
  , testCase "law 2" $ sOrig `actRight` (sOrig `differenceRight` sActed :: w) @?= sActed
  ]
