{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

-- base
import Control.Monad (replicateM_)
import Data.Char (toUpper)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Last (Last), Product (..), Sum (..))
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Prelude hiding (Foldable (..))

-- transformers
import Control.Monad.Trans.Reader (ReaderT (..), ask)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- falsify
import Test.Falsify.GenDefault (GenDefault (..), ViaGeneric (..))
import Test.Falsify.GenDefault.Std (Std)
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Generator as Gen
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import Test.Falsify.Range (between)
import Test.Tasty.Falsify (assert, gen, testProperty)

-- containers
import qualified Data.Map as M

-- changeset
import Control.Monad.Changeset.Class
import Control.Monad.Trans.Changeset
import Data.Monoid.RightAction (RightAction (..), RightTorsor (..), rEndo, set)
import Data.Monoid.RightAction.Coproduct (inL, inR, (:+:))
import Data.Monoid.RightAction.Generic (actRightGGeneric, actRightGeneric, differenceRightGGeneric, differenceRightGenericChanges)

type M = Changeset Int (Changes Count)
type MT = Changeset Torsor (Changes TorsorChange)

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
                            pure n
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
          "RightAction"
          [ testCase "Changes is lawful monoid action" $ do
              [] `actRight` singleChange (Cons True) `actRight` singleChange (Cons False) @?= ([] :: [Bool]) `actRight` singleChange (Cons True) <> singleChange (Cons False)
          , testGroup "Last" $
              rightActionMonoidLaws @Int @(Last Int) genInt genLastInt
          , -- RightTorsor (Last s) s does not satisfy differenceRight . actRight = id:
            -- when the action sets the value to the same state, differenceRight returns mempty.
            testGroup "Sum" $
              rightActionMonoidLaws genSumInt genSumInt
          , testGroup "Product" $
              rightActionMonoidLaws (genProduct genNonZeroRational) (genProduct genNonZeroRational)
          , testGroup "()" $
              rightActionMonoidLaws @Int @() genInt (pure ())
          , -- RightTorsor () s cannot satisfy actRight sOrig (differenceRight sOrig sActed) = sActed
            -- when sOrig /= sActed, since () carries no information.
            testGroup "Maybe" $
              rightActionMonoidLaws @Int @(Maybe (Last Int)) genInt (genMaybe genLastInt)
          , testGroup "Tuple" $
              rightActionMonoidLaws @(Int, Int) @(Last Int, Last Int)
                ((,) <$> genInt <*> genInt)
                ((,) <$> genLastInt <*> genLastInt)
          ]
      , testGroup
          "RightTorsor"
          [ testGroup "Sum" $
              allLaws genSumInt genSumInt
          , testGroup "Product" $
              allLaws (genProduct genNonZeroRational) (genProduct genNonZeroRational)
          ]
      , testGroup
          "MonadChangeset"
          [ testCase "ReaderT lifts changeset operations" $
              let action = flip execChangeset (0 :: Int) $ flip runReaderT (100 :: Int) $ do
                    env <- ask
                    replicateM_ env $ changeSingle Increment
               in action @?= 100
          ]
      , testGroup "SetTo" $
          rightActionSemigroupLaw @Int @(SetTo Int) genInt genSetTo
            : rightTorsorLaws genInt genSetTo
      , testGroup
          "Changes"
          [ testGroup "Count" $
              rightActionMonoidLaws @Int @(Changes Count) genInt (genChanges genCount)
          , testGroup "ListChange" $
              rightActionMonoidLaws @[Int] @(Changes (ListChange Int))
                (genSmallList genInt)
                (genChanges genListChangeInt)
          ]
      , testGroup
          "MaybeChange"
          [ testGroup "RightAction" $
              rightActionMonoidLaws @(Maybe Int) @(MaybeChange Int)
                genMaybeInt
                (genMaybeChange genInt)
                -- RightTorsor (MaybeChange a) (Maybe a) does not satisfy differenceRight . actRight = id:
                -- when the action sets the value to the same state, differenceRight returns mempty.
          ]
      , testGroup
          "FmapChange"
          [ testGroup "RightAction" $
              rightActionMonoidLaws @(Maybe Int) @(FmapChange Maybe (Last Int))
                genMaybeInt
                (genFmapChange genLastInt)
          ]
      , testGroup
          "diff and update"
          [ testProperty "diff yields differenceRight from current" $ do
              s0 <- gen genTorsor
              s1 <- gen genTorsor
              assert $
                P.eq
                  .$ ("snd (getChangeset (diff s1) s0)", snd (getChangeset (diff s1 :: MT (Changes TorsorChange)) s0))
                  .$ ("differenceRight s0 s1", differenceRight s0 s1)
          , testProperty "update sets the state" $ do
              s0 <- gen genTorsor
              s1 <- gen genTorsor
              assert $
                P.eq
                  .$ ("execChangeset (update s1) s0", execChangeset (update s1 :: MT ()) s0)
                  .$ ("s1", s1)
          , testProperty "change after update to same state is mempty" $ do
              s <- gen genTorsor
              assert $
                P.expect mempty
                  .$ ("getChange (update s) s", getChange (update s :: MT ()) s)
          ]
      , testGroup
          "Coproduct"
          [ testCase ":+: is monoid morphism" $
              (0 :: Int) `actRight` (inL (Last (Just 1)) <> inL (Last (Just 2)) :: Last Int :+: Last Int) @?= 0 `actRight` (inL (Last (Just (1 :: Int)) <> Last (Just 2)) :: Last Int :+: Last Int)
          , testGroup "RightAction" $
              rightActionMonoidLaws @Int @(Last Int :+: Last Int)
                genInt
                (Gen.choose (inL <$> genLastInt) (inR <$> genLastInt))
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
          , testGroup "RightAction" $
              rightActionMonoidLaws @(M.Map Int (Sum Int)) @(AlignChanges (M.Map Int) (Sum Int) (Sum Int))
                genMapIntInt
                (genAlignChangesMap (genDefault (Proxy @Std)))
          , testGroup
              "RightTorsor"
              [ -- The differenceRight . actRight law does not hold for AlignChanges:
                -- ChangeAlignPosition w on an unchanged entry round-trips to ChangeAlignPosition mempty ≠ w.
                -- Only actRight . differenceRight (law 2) holds.
                testProperty "actRight . differenceRight" $ do
                  sOrig <- gen genMapIntInt
                  sActed <- gen genMapIntInt
                  assert $
                    P.eq
                      .$ ("sOrig `actRight` differenceRight sOrig sActed", sOrig `actRight` differenceRight @(AlignChanges (M.Map Int) (Sum Int) (Sum Int)) sOrig sActed)
                      .$ ("sActed", sActed)
              ]
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
                  , testGroup "Property laws" $
                      changesLaws @Torsor @TorsorChange
                        (flip actRight)
                        differenceRight
                        genTorsor
                        genTorsorChange
                  ]
              , testGroup
                  "Product"
                  ( torsorTestSuite (TorsorChange2 5 6 7) (Torsor 2 3 4) (Torsor 7 18 11)
                      <> rightTorsorLaws @Torsor @TorsorChange2
                        genTorsor
                        genTorsorChange2
                  )
              , testGroup
                  "Newtype"
                  ( torsorTestSuite (TorsorWrapper 9) (TorsorWrapper 1) (TorsorWrapper 10)
                      <> rightTorsorLaws @TorsorWrapper @TorsorWrapper
                        genTorsorWrapper
                        genTorsorWrapper
                  )
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

-- * GenDefault instances via ViaGeneric Std

deriving via ViaGeneric Std Count instance GenDefault Std Count
deriving via ViaGeneric Std (Sum Int) instance GenDefault Std (Sum Int)
deriving via ViaGeneric Std (Last Int) instance GenDefault Std (Last Int)
deriving via ViaGeneric Std (SetTo Int) instance GenDefault Std (SetTo Int)
deriving via ViaGeneric Std (ListChange Int) instance GenDefault Std (ListChange Int)
deriving via ViaGeneric Std (AlignPositionChange (Sum Int) (Sum Int)) instance GenDefault Std (AlignPositionChange (Sum Int) (Sum Int))

torsorTestSuite :: forall w s. (RightAction w s, RightTorsor w s, Eq w, Show w, Eq s, Show s) => w -> s -> s -> [TestTree]
torsorTestSuite w sOrig sActed =
  [ testCase "act" $ sOrig `actRight` w @?= sActed
  , testCase "difference" $ sOrig `differenceRight` sActed @?= w
  , testCase "law 1" $ sOrig `differenceRight` (sOrig `actRight` w :: s) @?= w
  , testCase "law 2" $ sOrig `actRight` (sOrig `differenceRight` sActed :: w) @?= sActed
  ]

-- * Reusable law tests

-- | Tests for 'RightTorsor' laws that don't require 'Monoid' on the change type.
rightTorsorLaws :: forall s w. (Eq s, Show s, Eq w, Show w, RightAction w s, RightTorsor w s) => Gen s -> Gen w -> [TestTree]
rightTorsorLaws genS genW =
  [ rightTorsorLaw1 genS genW
  , rightTorsorLaw2 genS genW
  ]

rightTorsorLaw1 :: forall s w. (Eq s, Show s, Eq w, Show w, RightAction w s, RightTorsor w s) => Gen s -> Gen w -> TestTree
rightTorsorLaw1 genS genW = testProperty "differenceRight . actRight" $ do
  s <- gen genS
  w <- gen genW
  assert $
    P.eq
      .$ ("differenceRight s (s `actRight` w)", differenceRight s (s `actRight` w))
      .$ ("w", w)
rightTorsorLaw2 :: forall s w proxy. (Eq s, Show s, RightAction w s, RightTorsor w s) => Gen s -> proxy w -> TestTree
rightTorsorLaw2 genS _ = testProperty "actRight . differenceRight" $ do
  sOrig <- gen genS
  sActed <- gen genS
  assert $
    P.eq
      .$ ("sOrig `actRight` differenceRight sOrig sActed", sOrig `actRight` differenceRight @w sOrig sActed)
      .$ ("sActed", sActed)

-- | Tests for 'RightAction' laws that require 'Monoid' on the change type.
rightActionMonoidLaws :: forall s w. (Eq s, Show s, Eq w, Show w, Monoid w, RightAction w s) => Gen s -> Gen w -> [TestTree]
rightActionMonoidLaws genS genW = [rightActionSemigroupLaw genS genW, rightActionMemptyLaw genS genW]

rightActionSemigroupLaw :: forall s w. (Eq s, Show s, Eq w, Show w, Semigroup w, RightAction w s) => Gen s -> Gen w -> TestTree
rightActionSemigroupLaw genS genW =
  testProperty "actRight semigroup" $ do
    s <- gen genS
    m1 <- gen genW
    m2 <- gen genW
    assert $
      P.eq
        .$ ("(s `actRight` m1) `actRight` m2", (s `actRight` m1) `actRight` m2)
        .$ ("s `actRight` (m1 <> m2)", s `actRight` (m1 <> m2))

rightActionMemptyLaw :: forall s w proxy. (Eq s, Show s, Eq w, Show w, Monoid w, RightAction w s) => Gen s -> proxy w -> TestTree
rightActionMemptyLaw genS _ = testProperty "actRight mempty" $ do
  s <- gen genS
  assert $
    P.eq
      .$ ("s `actRight` mempty", s `actRight` (mempty @w))
      .$ ("s", s)

-- | Additional 'RightTorsor' law that requires 'Monoid': @differenceRight s s = mempty@.
rightTorsorMemptyLaw :: forall s w. (Eq s, Show s, Eq w, Show w, Monoid w, RightTorsor w s) => Gen s -> Gen w -> [TestTree]
rightTorsorMemptyLaw genS _genW =
  [ testProperty "differenceRight self = mempty" $ do
      s <- gen genS
      assert $
        P.expect (mempty @w)
          .$ ("differenceRight s s", differenceRight @w s s)
  ]

-- | All 'RightAction' and 'RightTorsor' laws for a 'Monoid' change type.
allLaws :: (Eq s, Show s, Eq w, Show w, Monoid w, RightAction w s, RightTorsor w s) => Gen s -> Gen w -> [TestTree]
allLaws genS genW = rightActionMonoidLaws genS genW <> (rightTorsorLaws genS genW <> rightTorsorMemptyLaw genS genW)

{- | All laws for the common pattern where @RightAction w s@ and @RightTorsor (Changes w) s@.

Note: The @differenceRight . actRight@ test checks that the round-tripped diff has the same /effect/
rather than being structurally equal, since 'Changes' is a free monoid and multiple representations
can encode the same action.
-}
changesLaws ::
  forall s w.
  (Eq s, Show s, Eq w, Show w) =>
  (Changes w -> s -> s) ->
  (s -> s -> Changes w) ->
  Gen s ->
  Gen w ->
  [TestTree]
changesLaws act diffFn genS genW =
  [ testProperty "differenceRight . actRight (effect)" $ do
      s <- gen genS
      w <- gen genChangesW
      -- The diff of the round-trip must produce the same effect, not necessarily the same Changes list
      assert $
        P.eq
          .$ ("act (differenceRight s (act w s)) s", act (diffFn s (act w s)) s)
          .$ ("act w s", act w s)
  , testProperty "actRight . differenceRight" $ do
      sOrig <- gen genS
      sActed <- gen genS
      assert $
        P.eq
          .$ ("act (differenceRight sOrig sActed) sOrig", act (diffFn sOrig sActed) sOrig)
          .$ ("sActed", sActed)
  , testProperty "differenceRight self = mempty" $ do
      s <- gen genS
      assert $
        P.expect mempty
          .$ ("differenceRight s s", diffFn s s)
  ]
  where
    genChangesW :: Gen (Changes w)
    genChangesW = genChanges genW

-- * Generators

genInt :: Gen Int
genInt = Gen.int (between (0, 100))

genSetTo :: Gen (SetTo Int)
genSetTo = genDefault (Proxy @Std)

-- | Generate a non-zero rational (for Product torsor, avoiding division by zero).
genNonZeroRational :: Gen Rational
genNonZeroRational = do
  n <- Gen.int (between (1, 100))
  d <- Gen.int (between (1, 100))
  sign <- Gen.bool True
  pure $ (if sign then 1 else -1) * fromIntegral n / fromIntegral d

genSumInt :: Gen (Sum Int)
genSumInt = genDefault (Proxy @Std)

genProduct :: Gen a -> Gen (Product a)
genProduct = fmap Product

genLastInt :: Gen (Last Int)
genLastInt = genDefault (Proxy @Std)

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe = Gen.shrinkToNothing

genChanges :: Gen w -> Gen (Changes w)
genChanges genW = changes <$> Gen.list (between (0, 5)) genW

genListChangeInt :: Gen (ListChange Int)
genListChangeInt = genDefault (Proxy @Std)

genCount :: Gen Count
genCount = genDefault (Proxy @Std)

genMaybeChange :: Gen a -> Gen (MaybeChange a)
genMaybeChange genA = Gen.choose (pure mempty) (Gen.choose (setJust <$> genA) (pure setNothing))

genFmapChange :: Gen w -> Gen (FmapChange f w)
genFmapChange = fmap FmapChange

genMaybeInt :: Gen (Maybe Int)
genMaybeInt = genDefault (Proxy @Std)

genSmallList :: Gen a -> Gen [a]
genSmallList = Gen.list (between (0, 5))

genAlignChangesMap :: Gen (AlignPositionChange w s) -> Gen (AlignChanges (M.Map Int) w s)
genAlignChangesMap genAPC = do
  kvs <- Gen.list (between (0, 5)) $ do
    k <- Gen.int (between (0, 10))
    v <- genAPC
    pure (k, v)
  pure $ AlignChanges $ M.fromList kvs

genSmallMap :: (Ord k) => Gen k -> Gen v -> Gen (M.Map k v)
genSmallMap genK genV = do
  kvs <- Gen.list (between (0, 5)) ((,) <$> genK <*> genV)
  pure $ M.fromList kvs

genMapIntInt :: Gen (M.Map Int (Sum Int))
genMapIntInt = genSmallMap (Gen.int (between (0, 10))) genSumInt

genSumInteger :: Gen (Sum Integer)
genSumInteger = Sum . fromIntegral <$> Gen.int (between (-100, 100))

genProductRational :: Gen (Product Rational)
genProductRational = do
  n <- Gen.int (between (1, 100))
  d <- Gen.int (between (1, 100))
  sign <- Gen.bool True
  pure $ Product $ (if sign then 1 else -1) * fromIntegral n / fromIntegral d

genSumRational :: Gen (Sum Rational)
genSumRational = do
  n <- Gen.int (between (-100, 100))
  d <- Gen.int (between (1, 100))
  pure $ Sum $ fromIntegral n / fromIntegral d

genTorsorWrapper :: Gen TorsorWrapper
genTorsorWrapper = TorsorWrapper <$> genSumInteger

genTorsor :: Gen Torsor
genTorsor = Torsor <$> genSumInteger <*> genProductRational <*> genSumRational

genTorsorChange :: Gen TorsorChange
genTorsorChange =
  Gen.oneof
    ( (TorsorChangeSum <$> genSumInteger)
        :| [ TorsorChangeProduct <$> genProductRational
           , TorsorChangeSum2 <$> genSumRational
           ]
    )

genTorsorChange2 :: Gen TorsorChange2
genTorsorChange2 = TorsorChange2 <$> genSumInteger <*> genProductRational <*> genSumRational
