module Data.Monoid.RightAction.Generic where

-- base
import Control.Monad (guard)
import GHC.Generics (Generic (..), K1 (..), M1 (..), Rec1 (..), (:*:) (..), (:+:) (..))
import Prelude hiding (zipWith)

-- changeset
import Control.Monad.Trans.Changeset (Changes, singleChange)
import Data.Monoid.RightAction

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
