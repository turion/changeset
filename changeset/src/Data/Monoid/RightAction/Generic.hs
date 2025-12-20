{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Monoid.RightAction.Generic where

-- base
import Control.Monad (guard)
import GHC.Generics (Generic (..), K1 (..), M1 (..), Rec1 (..), (:*:) (..), (:+:) (..))
import Prelude hiding (zipWith)

-- generics-sop
import Generics.SOP (All, All2, AllZip, AllZip2, HExpand (..), HSequence (..), HTrans (..), HTraverse_ (..), NP (..), NS (..), SListI, SListI2, SOP (..), hmap, hzipWith, unSOP, unZ)

-- changeset

import Control.Monad.State.Strict (execState, modify)
import Control.Monad.Trans.Changeset (Changes, singleChange)
import Data.Monoid (Endo (..))
import Data.Monoid.RightAction
import Data.Proxy (Proxy (..))
import Generics.SOP.BasicFunctors
import Generics.SOP.Constraint (Head)
import Generics.SOP.GGP (GCode, GFrom, GTo, gfrom, gto)

class (RightAction w s) => FlipRightAction s w where
  flipActRight :: w -> s -> s

instance (RightAction w s) => FlipRightAction s w where
  flipActRight = flip actRight

actRightSOP :: forall w s. (GFrom w, Generic w, All2 (FlipRightAction s) (GCode w)) => s -> w -> s
actRightSOP s = flip execState s . hctraverse_ (Proxy @(FlipRightAction s)) (\(I w) -> modify (flipActRight w)) . gfrom

-- actRightSOPSOP :: forall ws ss . (GFrom ws, Generic ws, GFrom ss, GTo ss, Generic ss, AllZip2 RightAction (GCode ws) (GCode ss)) => ss -> ws -> ss
-- actRightSOPSOP ss ws = gto $ _

actRightSOPProduct :: forall ws ss. (GFrom ws, Generic ws, GFrom ss, GTo ss, Generic ss, GCode ss ~ '[Head (GCode ss)], AllZip ProductAction (GCode ws) (Head (GCode ss))) => ss -> ws -> ss
actRightSOPProduct ss ws = gto $ SOP $ Z $ hzipWith (\(I s) (Endo f) -> I (f s)) (unZ $ unSOP $ gfrom ss) (hexpand mempty (toEndos ws))

toEndos :: forall ws ss. (GFrom ws, Generic ws, (AllZip ProductAction (GCode ws) ss)) => ws -> NS Endo ss
toEndos = toEndos' . unSOP . gfrom

class (All (FlipRightAction s) wss) => ProductAction wss s
instance (All (FlipRightAction s) wss) => ProductAction wss s

toEndos' :: forall wss ss. (AllZip ProductAction wss ss) => NS (NP I) wss -> NS Endo ss
toEndos' = htrans (Proxy @ProductAction) toEndo

toEndo :: forall ws s. (All (FlipRightAction s) ws) => NP I ws -> Endo s
toEndo = Endo . execState . hctraverse_ (Proxy @(FlipRightAction s)) (\(I w) -> modify $ flipActRight w)

-- FIXME misnomer
actRightSOPSum :: forall wss ss. (GFrom wss, Generic wss, GFrom ss, GTo ss, Generic ss, AllZip2 RightAction (GCode wss) (GCode ss), SListI2 (GCode ss)) => ss -> wss -> ss
actRightSOPSum ss wss = gto $ hzipWith (\(Endo f) -> fmap f) (hexpand mempty $ htrans (Proxy @RightAction) (Endo . flipActRight . unI) $ gfrom wss) $ gfrom ss

data SumChange s w = SummandChange w | Switch s
  deriving (Eq, Show, Ord, Read)

instance (RightAction w s) => RightAction (SumChange s w) s where
  actRight s (SummandChange w) = actRight s w
  actRight _ (Switch s) = s

class RightTorsorTwo s w where
  rightTorsorTwo :: Two s -> w

instance (RightTorsor w s) => RightTorsorTwo s w where
  rightTorsorTwo (Two sOrig sChanged) = differenceRight sOrig sChanged

class RightTorsorDifferenceNP ss ws where
  rightTorsorDifferenceNP :: Difference ss -> NP I ws

instance (AllZip RightTorsorTwo ss ws) => RightTorsorDifferenceNP ss ws where
  rightTorsorDifferenceNP (Difference ssPairs) = htrans (Proxy @RightTorsorTwo) (I . rightTorsorTwo) ssPairs

differenceRightSOPSum :: forall wss ss. (GFrom wss, Generic wss, GFrom ss, GTo wss, Generic ss, AllZip RightTorsorDifferenceNP (GCode ss) (GCode wss), AllZip2 RightAction (GCode wss) (GCode ss), AllZip2 RightAction (GCode ss) (GCode wss), SListI2 (GCode ss)) => ss -> ss -> SumChange ss wss
differenceRightSOPSum ssOrig ssChanged = either Switch (SummandChange . gto . SOP . htrans (Proxy @RightTorsorDifferenceNP) rightTorsorDifferenceNP) $ hctraverse' (Proxy @SListI) (sumChangeSOP ssChanged) $ hzipWith differenceSummand (hexpand WrongSummand $ hmap RightSummand $ unSOP $ gfrom ssOrig) (unSOP $ gfrom ssChanged)

data DifferenceSummand ss = WrongSummand | RightSummand (NP I ss)
data SumChangeSOP ss = SummandSwitch | SummandDifference (NP I ss) (NP I ss)
newtype Difference ss = Difference (NP Two ss)
data Two s = Two s s
differenceSummand :: DifferenceSummand ss -> NP I ss -> SumChangeSOP ss
differenceSummand = \case
  WrongSummand -> const SummandSwitch
  RightSummand ss -> SummandDifference ss

sumChangeSOP :: (SListI ss) => s -> SumChangeSOP ss -> Either s (Difference ss)
sumChangeSOP s = \case
  SummandSwitch -> Left s
  SummandDifference ssOrig ssChanged -> Right $ Difference $ hzipWith (\(I sOrig) (I sChanged) -> Two sOrig sChanged) ssOrig ssChanged

{- | A class to define generic instances of 'RightAction'. You will rarely need it directly.

To derive 'RightAction' for your custom datatype generically, see 'actRightGeneric'.
-}
class GRightAction w s where
  gActRight :: s -> w a -> s

instance (RightAction w s) => GRightAction (K1 i w) s where
  gActRight s (K1 w) = actRight s w

instance (GRightAction f s) => GRightAction (M1 i c f) s where
  gActRight s (M1 f) = gActRight s f

instance (GRightAction f1 s, GRightAction f2 s) => GRightAction (f1 :+: f2) s where
  gActRight s = \case
    L1 f -> gActRight s f
    R1 f -> gActRight s f

instance (GRightAction f1 s, GRightAction f2 s) => GRightAction (f1 :*: f2) s where
  gActRight s (f1 :*: f2) = s `gActRight` f1 `gActRight` f2

instance (GRightAction f s) => GRightAction (Rec1 f) s where
  gActRight s (Rec1 f) = gActRight s f

{- | Derive a 'RightAction' instance generically.

If you have a definition of a datatype that contains fields with a 'RightAction' instance,
you can create an instance for the whole datatype with just two lines of boiler plate
by setting 'actRight' to this function:

@
data IntAction
  = IntActionCounts Count (Changes Count)
  | IntActionLast (Last Int)
  | IntActionRec IntAction IntAction
  deriving stock (Generic)

instance RightAction IntAction Int where
  actRight = actRightGeneric
@

Note: This is not implemented as a default method in 'RightAction', since the default already is the trivial action.
That's usually the desired behaviour as unrelated fields in the datatype are not required to implement an instance.

Note: Since the acting type is not the last parameter in the 'RightAction' type class,
it seems to be impossible to derive an instance with deriving via or 'GHC.Generics.Generically'.
If you know a way around this, please open an issue at https://github.com/turion/changeset/issues/new.
-}
actRightGeneric :: (Generic m, GRightAction (Rep m) s) => s -> m -> s
actRightGeneric s m = gActRight s $ from m

{- | Similar to 'GRightAction', but for situations where both the change type and the state type should have a generic instance.

See 'actRightGGeneric' to define generic instances.
-}
class GGRightAction w s where
  ggActRight :: s a -> w a -> s a

instance (RightAction w s) => GGRightAction (K1 i w) (K1 i' s) where
  ggActRight (K1 s) (K1 w) = K1 $ actRight s w

instance {-# OVERLAPPABLE #-} (GGRightAction w s) => GGRightAction (M1 c i w) s where
  ggActRight s (M1 w) = ggActRight s w

instance {-# OVERLAPPABLE #-} (GGRightAction w s) => GGRightAction w (M1 c i s) where
  ggActRight (M1 s) w = M1 $ ggActRight s w

instance (GGRightAction w s) => GGRightAction (M1 c i w) (M1 c' i' s) where
  ggActRight (M1 s) (M1 w) = M1 $ ggActRight s w

instance (GGRightAction wL sL, GGRightAction wR sR) => GGRightAction (wL :+: wR) (sL :+: sR) where
  ggActRight = \case
    s@(L1 sL) -> \case
      L1 wL -> L1 $ ggActRight sL wL
      R1 _ -> s
    s@(R1 sR) -> \case
      L1 _ -> s
      R1 wR -> R1 $ ggActRight sR wR

instance (GGRightAction wL sL, GGRightAction wR sR) => GGRightAction (wL :+: wR) (sL :*: sR) where
  ggActRight (sL :*: sR) = \case
    L1 wL -> ggActRight sL wL :*: sR
    R1 wR -> sL :*: ggActRight sR wR

instance (GGRightAction wL sL, GGRightAction wR sR) => GGRightAction (wL :*: wR) (sL :*: sR) where
  ggActRight (sL :*: sR) (wL :*: wR) = ggActRight sL wL :*: ggActRight sR wR

instance (GGRightAction w s) => GGRightAction (Rec1 w) (Rec1 s) where
  ggActRight (Rec1 s) (Rec1 w) = Rec1 $ ggActRight s w

{- | Derive a 'RightAction' instance generically over the structure of both change and state type.

You can create 'RightAction' instances with just two lines of boiler plate by setting 'actRight' to this function.

There are several situations where this is useful:

1. Wrapping both change and state in @newtype@s:

    @
    newtype Bar = Bar Int
      deriving stock (Generic, Eq, Show)

    newtype BarChange = BarChange Count
      deriving stock (Generic)

    instance RightAction BarChange Bar where
      actRight = actRightGGeneric
    @

2. When the state is a product type, create a sum type where every constructor acts on one field:

    @
    data Foo = Foo Int Int [()]
      deriving stock (Generic, Eq, Show)

    data FooChange = FooChangeInt Count | FooChangeInt2 Count | FooChangeList (ListChange ())
      deriving stock (Generic)

    instance RightAction FooChange Foo where
      actRight = actRightGGeneric
    @

    The order matters, and the number of fields and constructors must match.

3. When the state is a product type, create a product type where every field of the change type acts on one field of the state type.

    @
    data Foo = Foo Int Int [()]
      deriving stock (Generic, Eq, Show)

    data FooChange2 = FooChange2 Count (Maybe Count) (ListChange ())
      deriving stock (Generic)

    instance RightAction FooChange2 Foo where
      actRight = actRightGGeneric
    @
    The order matters, and the number of fields must match.
-}
actRightGGeneric :: (Generic w, Generic s, GGRightAction (Rep w) (Rep s)) => s -> w -> s
actRightGGeneric s w = to $ ggActRight (from s) (from w)

{- | A class to define generic instances of 'RightTorsor' for product change types. You will rarely need it directly.

To derive 'RightTorsor' for your custom datatype generically, see 'differenceRightGGeneric'.
-}
class GGRightTorsor w s where
  ggDifferenceRight :: s a -> s a -> w a

instance (RightTorsor w s) => GGRightTorsor (K1 i w) (K1 i' s) where
  ggDifferenceRight (K1 sOld) (K1 sNew) = K1 $ differenceRight sOld sNew

instance (GGRightTorsor f s) => GGRightTorsor (M1 i c f) (M1 i' c' s) where
  ggDifferenceRight (M1 sOld) (M1 sNew) = M1 $ ggDifferenceRight sOld sNew

instance {-# OVERLAPPABLE #-} (GGRightTorsor f s) => GGRightTorsor (M1 i c f) s where
  ggDifferenceRight sOld sNew = M1 $ ggDifferenceRight sOld sNew

instance {-# OVERLAPPABLE #-} (GGRightTorsor f s) => GGRightTorsor f (M1 i c s) where
  ggDifferenceRight (M1 sOld) (M1 sNew) = ggDifferenceRight sOld sNew

instance (GGRightTorsor fL sL, GGRightTorsor fR sR) => GGRightTorsor (fL :*: fR) (sL :*: sR) where
  ggDifferenceRight (sLOld :*: sROld) (sLNew :*: sRNew) = ggDifferenceRight sLOld sLNew :*: ggDifferenceRight sROld sRNew

instance (GGRightTorsor f s) => GGRightTorsor (Rec1 f) (Rec1 s) where
  ggDifferenceRight (Rec1 sOld) (Rec1 sNew) = Rec1 $ ggDifferenceRight sOld sNew

{- | Derive a 'RightTorsor' instance generically for product change types.

If you have a definition of a datatype that contains fields with a 'RightTorsor' instance,
you can create an instance for the whole datatype with just two lines of boiler plate
by setting 'differenceRight' to this function:

@
data Torsor = Torsor (Sum Integer) (Product Rational)
  deriving stock (Generic)

data TorsorChange2 = TorsorChange2 (Sum Integer) (Product Rational)
  deriving stock (Generic)

instance RightAction TorsorChange2 Torsor where
  actRight = actRightGGeneric

instance RightTorsor TorsorChange2 Torsor where
  differenceRight = differenceRightGGeneric
@

In many cases where this is sensible, the instance can be derived this way.
Whenever it typechecks, it will be the inverse to the automatically derived instance from 'actRightGGeneric'.
-}
differenceRightGGeneric :: (Generic w, Generic s, GGRightTorsor (Rep w) (Rep s)) => s -> s -> w
differenceRightGGeneric sOld sNew = to $ ggDifferenceRight (from sOld) (from sNew)

{- | A class to define generic instances of 'RightTorsor' for sum change types. You will rarely need it directly.

To derive 'RightTorsor' for your custom datatype generically, see 'differenceRightGenericChanges'.
-}
class GCRightTorsor w s where
  gcDifferenceRight :: s a -> s a -> Changes (w a)

instance (RightTorsor w s) => GCRightTorsor (K1 i w) (K1 i' s) where
  gcDifferenceRight (K1 sOld) (K1 sNew) = singleChange $ K1 $ differenceRight sOld sNew

instance (GCRightTorsor f s) => GCRightTorsor (M1 i c f) (M1 i' c' s) where
  gcDifferenceRight (M1 sOld) (M1 sNew) = M1 <$> gcDifferenceRight sOld sNew

instance {-# OVERLAPPABLE #-} (GCRightTorsor f s) => GCRightTorsor (M1 i c f) s where
  gcDifferenceRight sOld sNew = M1 <$> gcDifferenceRight sOld sNew

instance {-# OVERLAPPABLE #-} (GCRightTorsor f s) => GCRightTorsor f (M1 i c s) where
  gcDifferenceRight (M1 sOld) (M1 sNew) = gcDifferenceRight sOld sNew

instance (GCRightTorsor fL sL, GCRightTorsor fR sR, forall a. Eq (sR a), forall a. Eq (sL a)) => GCRightTorsor (fL :+: fR) (sL :*: sR) where
  gcDifferenceRight (sLOld :*: sROld) (sLNew :*: sRNew) =
    (guard (sLOld /= sLNew) >> (L1 <$> gcDifferenceRight sLOld sLNew))
      <> (guard (sROld /= sRNew) >> (R1 <$> gcDifferenceRight sROld sRNew))

instance (GCRightTorsor f s) => GCRightTorsor (Rec1 f) (Rec1 s) where
  gcDifferenceRight (Rec1 sOld) (Rec1 sNew) = Rec1 <$> gcDifferenceRight sOld sNew

{- | Derive a 'RightTorsor' instance generically for sum change types.

If you have a definition of a datatype that contains fields with a 'RightTorsor' instance,
you can create an instance for the whole datatype with just two lines of boiler plate
by setting 'differenceRight' to this function:

@
data Torsor = Torsor (Sum Integer) (Product Rational)
  deriving stock (Generic, Eq, Show)

data TorsorChange = TorsorChangeSum (Sum Integer) | TorsorChangeProduct (Product Rational)
  deriving stock (Generic, Eq, Show)

instance RightAction TorsorChange Torsor where
  actRight = actRightGGeneric

instance RightTorsor (Changes TorsorChange) Torsor where
  differenceRight = differenceRightGenericChanges
@

In this example, @TorsorChangeSum@ will act on the first field of @Torsor@, and @TorsorChangeProduct@ on the second.
When computing the difference, every field will be compared, and for every difference a change will be created.
All the changes are collected in a 'Changes' container.

In many cases where this is sensible, the instance can be derived this way.
Whenever it typechecks, it will be the inverse to the automatically derived instance from 'actRightGGeneric'.
-}
differenceRightGenericChanges :: (Generic w, Generic s, GCRightTorsor (Rep w) (Rep s)) => s -> s -> Changes w
differenceRightGenericChanges sOld sNew = to <$> gcDifferenceRight (from sOld) (from sNew)
