{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Changeset.Lens.At where

-- base
import Data.Foldable
import Data.Function ((&))
import Prelude hiding (Foldable (..))

-- lens
import Control.Lens (At (..), Index, IxValue, (%~))

-- containers
import Data.IntMap (IntMap)
import Data.Map (Map)

-- monoidal-containers
import Data.Map.Monoidal (MonoidalMap, foldlWithKey', singleton)

-- monoid-extras
import Data.Monoid.RightAction

-- indexed-traversal
import Data.Foldable.WithIndex (FoldableWithIndex)
import Data.Functor.WithIndex (FunctorWithIndex)
import Data.Traversable.WithIndex (TraversableWithIndex (itraverse))

-- witherable
import Witherable (Filterable (mapMaybe), FilterableWithIndex, Witherable (..), WitherableWithIndex)

-- changeset
import Control.Monad.Changeset.Class (MonadChangeset (..))
import Control.Monad.Trans.Changeset (FmapChange (FmapChange), JustChange, MaybeChange, setJust)

-- changeset-lens
import Control.Monad.Changeset.Lens.Ixed (IxedChangeset (..))

{- | Edit parts of an indexed datastructure.

The state datastructure is usually a container, such as a 'Data.Map.Map'.
Changes to an element of that structure are promoted to a change of the whole structure
by pairing them with an 'Index' that points to a specific position in the structure.

In contrast to 'Control.Monad.Changeset.Lens.IxedChangeset',
'AtChangeset' can also create or delete values.
See for example 'Control.Monad.Trans.Changeset.MaybeChange':
The operation @i |>\@ 'setJust' a@ will create a value @a@ at key @i.@

Note: Internally, an 'AtChangeset' is represented as a map,
and the monoid structure is pointwise.
This is because we assume that all different indices refer to different positions,
so changes on different indices commute.
-}
newtype AtChangeset s w = AtChangeset
  {getAtChangeset :: MonoidalMap (Index s) w}

deriving instance (Ord (Index s), Semigroup w) => Semigroup (AtChangeset s w)
deriving instance (Ord (Index s), Monoid w) => Monoid (AtChangeset s w)
deriving instance (Eq (Index s), Eq w) => Eq (AtChangeset s w)
deriving instance (Ord (Index s), Ord w) => Ord (AtChangeset s w)
deriving instance (Show (Index s), Show w) => Show (AtChangeset s w)
deriving instance (Ord (Index s), Read (Index s), Read w) => Read (AtChangeset s w)
deriving instance Functor (AtChangeset s)
deriving instance Foldable (AtChangeset s)
deriving instance Traversable (AtChangeset s)
deriving instance (Index s ~ i) => FunctorWithIndex i (AtChangeset s)
deriving instance (Index s ~ i) => FoldableWithIndex i (AtChangeset s)
instance (Index s ~ i) => TraversableWithIndex i (AtChangeset s) where
  itraverse f = fmap AtChangeset . itraverse f . getAtChangeset
instance Filterable (AtChangeset s) where
  mapMaybe f = AtChangeset . mapMaybe f . getAtChangeset
instance (Index s ~ i) => FilterableWithIndex i (AtChangeset s)
instance (Ord (Index s)) => Witherable (AtChangeset s) where
  wither f = fmap AtChangeset . wither f . getAtChangeset
instance (Index s ~ i, Ord i) => WitherableWithIndex i (AtChangeset s)

instance (RightAction w (Maybe (IxValue s)), At s) => RightAction (AtChangeset s w) s where
  actRight s AtChangeset {getAtChangeset} = foldlWithKey' (\s' i w -> s' & at i %~ flip actRight w) s getAtChangeset

-- | Create an 'AtChangeset' by pointing to a position in @s@, and specifying a change @w@ to the value at that position.
atChangeset ::
  -- | A position in @s.@ For example, @s@ might be a map, and @'Index' s@ a key.
  Index s ->
  -- | A change to the element in @s@ at the given position. Typically, we expect @'RightAction' w ('Maybe' ('IxValue' s))@ to hold.
  w ->
  AtChangeset s w
atChangeset i = AtChangeset . singleton i

-- | Changes to the elements of a 'Map'.
type MapAtChangeset k a = AtChangeset (Map k a) (MaybeChange a)

-- | Changes to the elements of an 'IntMap'.
type IntMapAtChangeset a = AtChangeset (IntMap a) (MaybeChange a)

{- | Change a value at a given index.

Essentially, this applies 'atChangeset'.

Example:

@
-- Sets the value at key i to a
i <>\@ 'setJust' a
-- Deletes the value at key i
i <>\@ 'setNothing'
@
-}
(<>@) :: forall s w m. (MonadChangeset s (AtChangeset s w) m) => Index s -> w -> m ()
index <>@ w = change @s $ atChangeset @s index w

{- | Set a value at a given index.

A shorthand for '<>@' in combination with 'setJust'.

Example:

@
-- Sets the value at key i to a
i .@ a
@
-}
(.@) :: forall s m. (MonadChangeset s (AtChangeset s (MaybeChange (IxValue s))) m) => Index s -> IxValue s -> m ()
index .@ w = (<>@) @s index (setJust w)

{- | Lift an 'IxedChangeset' to an 'AtChangeset'.

The action of the resulting changeset is the same,
but 'AtChangeset' is the more expressive type.
-}
ixedToAtChangeset :: IxedChangeset s w -> AtChangeset s (JustChange w)
ixedToAtChangeset = AtChangeset . fmap FmapChange . getIxedChangeset
