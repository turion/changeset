{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Changeset.Lens.Ixed where

-- base
import Data.Foldable (Foldable)
import Data.Function ((&))
import Data.Monoid (First (..))
import GHC.Generics (Generic)
import Prelude hiding (Foldable (..))

-- lens
import Control.Lens (Index, IxValue, Ixed (..), (%~))

-- containers
import Data.Map.Strict (Map)

-- monoidal-containers
import Data.Map.Monoidal (MonoidalMap, foldlWithKey', singleton)

-- monoid-extras
import Data.Monoid.RightAction

-- indexed-traversal
import Data.Foldable.WithIndex (FoldableWithIndex)
import Data.Functor.WithIndex (FunctorWithIndex)
import Data.Traversable.WithIndex (TraversableWithIndex (..))

-- witherable
import Witherable (Filterable (..), FilterableWithIndex (..), Witherable (..), WitherableWithIndex)

-- changeset
import Control.Monad.Changeset.Class (MonadChangeset (..))

{- | Edit parts of an indexed datastructure.

The state datastructure is usually a container, such as a 'Data.Map.Map'.
Changes to an element of that structure are promoted to a change of the whole structure
by pairing them with an 'Index' that points to a specific position in the structure.

Note: Internally, an 'IxedChangeset' is represented as a map,
and the monoid structure is pointwise.
This is because we assume that all different indices refer to different positions,
so changes on different indices commute.
-}
newtype IxedChangeset s w = IxedChangeset
  {getIxedChangeset :: MonoidalMap (Index s) w}
  deriving stock (Generic)

deriving instance (Ord (Index s), Semigroup w) => Semigroup (IxedChangeset s w)
deriving instance (Ord (Index s), Monoid w) => Monoid (IxedChangeset s w)
deriving instance (Eq (Index s), Eq w) => Eq (IxedChangeset s w)
deriving instance (Ord (Index s), Ord w) => Ord (IxedChangeset s w)
deriving instance (Show (Index s), Show w) => Show (IxedChangeset s w)
deriving instance (Ord (Index s), Read (Index s), Read w) => Read (IxedChangeset s w)
deriving instance Functor (IxedChangeset s)
deriving instance Foldable (IxedChangeset s)
deriving instance Traversable (IxedChangeset s)
deriving instance (Index s ~ i) => FunctorWithIndex i (IxedChangeset s)
deriving instance (Index s ~ i) => FoldableWithIndex i (IxedChangeset s)
instance (Index s ~ i) => TraversableWithIndex i (IxedChangeset s) where
  itraverse f = fmap IxedChangeset . itraverse f . getIxedChangeset
instance Filterable (IxedChangeset s) where
  mapMaybe f = IxedChangeset . mapMaybe f . getIxedChangeset
instance (Index s ~ i) => FilterableWithIndex i (IxedChangeset s)
instance (Ord (Index s)) => Witherable (IxedChangeset s) where
  wither f = fmap IxedChangeset . wither f . getIxedChangeset
instance (Index s ~ i, Ord i) => WitherableWithIndex i (IxedChangeset s)

instance (RightAction w (IxValue s), Ixed s) => RightAction (IxedChangeset s w) s where
  actRight s IxedChangeset {getIxedChangeset} = foldlWithKey' (\s' i w -> s' & ix i %~ flip actRight w) s getIxedChangeset

-- | Create an 'IxedChangeset' by pointing to a position in @s@, and specifying a change @w@ to the value at that position.
ixedChangeset ::
  -- | A position in @s@. For example, @s@ might be a map, and @'Index' s@ a key.
  Index s ->
  -- | A change to the element in @s@ at the given position. Typically, we expect @'RightAction' w ('IxValue' s)@ to hold.
  w ->
  IxedChangeset s w
ixedChangeset i = IxedChangeset . singleton i

-- | Changes to the elements of a 'Map'.
type MapIxedChangeset k a = IxedChangeset (Map k a) a

{- | Change a value at a given index.

Essentially, this applies 'ixedChangeset'.

Example:

@
-- Increments the value at key i
i |>! Increment
@
-}
(|>!) :: (MonadChangeset s (IxedChangeset s w) m) => Index s -> w -> m ()
index |>! w = change $ ixedChangeset index w

{- | Set a value at a given index.

A shorthand for '|>!' in combination with 'First'.

Example:

@
-- Sets the value at key i to a
i .! a
@
-}
(.!) :: (MonadChangeset s (IxedChangeset s (First (IxValue s))) m) => Index s -> IxValue s -> m ()
index .! w = index |>! First (Just w)
