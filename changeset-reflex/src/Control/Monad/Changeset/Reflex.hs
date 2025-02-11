{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Control.Monad.Changeset.Reflex where

-- base
import Data.Bifunctor (Bifunctor (second))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Compose
import Data.Functor.Identity (Identity (Identity))
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Tuple (swap)
import GHC.Generics (Generic)

-- containers
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)

-- some
import Data.GADT.Compare (GCompare)

-- reflex
import Reflex.Class ( Reflex(never, Event, mergeG) )

-- dependent-map
import Data.Dependent.Map (DMap, delete, insert, traverseWithKey, mapWithKey, intersectionWithKey, union, alter)
import Data.Functor.Misc (dmapToIntMap, dmapToMap, intMapWithFunctorToDMap, mapWithFunctorToDMap)

-- changeset
import Control.Monad.Trans.Changeset (ChangesetT (..), runChangesetT, AlignPositionChange)
import Data.Monoid.RightAction (RightAction (..))

-- * Changes for 'DMap's

-- | A change to a dependent map 'DMap'.
data DMapChange k f v
  = Insert (k v) (f v)
  | Delete (k v)
  deriving stock (Functor, Foldable, Traversable, Generic)

instance (GCompare k) => RightAction (DMapChange k f v) (DMap k f) where
  actRight s (Insert k v) = insert k v s
  actRight s (Delete k) = delete k s

data DMapWithKeyChange k w = DMapWithKeyChange { getDMapWithKeyChange :: forall v . k v -> w v}

instance (forall v . RightAction (w v) (f v)) => RightAction (DMapWithKeyChange k w) (DMap k f) where
  actRight dmap (DMapWithKeyChange kw) = mapWithKey (\kv fv -> fv `actRight` kw kv) dmap

data DMapAlterChange k w v = DMapIxedChange
  { key :: k v
  , change :: w
  }

instance (GCompare k, RightAction w (Maybe (f v))) => RightAction (DMapAlterChange k w v) (DMap k f) where
  actRight dmap dmapAlterChange = alter (flip actRight dmapAlterChange.change) dmapAlterChange.key dmap

newtype DMapAlignChanges k w = DMapAlignChanges { getDMapAlignChanges :: DMap k (AlignPositionChange w)}

instance (GCompare k, (forall v . RightAction (AlignPositionChange w v) (f v))) => RightAction (DMapAlignChanges k w) (DMap k f) where
  actRight dmap DMapAlignChanges {getDMapAlignChanges} =
    let updated = intersectionWithKey (const actRight) dmap getDMapAlignChanges
    in union updated dmap

-- * Events that carry changes

-- | A functor that creates changes, and performs side effects in @m@ to create an 'Event'.
type ChangesetEventT t s w m = ChangesetT s w (Compose m (Event t))

-- | Create an 'Event' that, given the current state, produces a value and a change.
changesetEventT :: (s -> m (Event t (w, a))) -> ChangesetEventT t s w m a
changesetEventT = ChangesetT . fmap Compose

-- | Like 'changesetEventT', but without the value.
changesetEventT_ :: (Functor m, Reflex t) => (s -> m (Event t w)) -> ChangesetEventT t s w m ()
changesetEventT_ = changesetEventT . fmap (fmap (fmap (,())))

{- | Inspect a 'ChangesetEventT'.

This is the inverse to 'changesetEventT'.
-}
getChangesetEventT :: ChangesetEventT t s w m a -> s -> m (Event t (w, a))
getChangesetEventT = fmap getCompose . getChangesetT

-- | Like 'getChangesetEventT', but discarding the value.
getChangeEventT :: (Functor m, Reflex t) => ChangesetEventT t s w m a -> s -> m (Event t w)
getChangeEventT = fmap (fmap (fmap fst)) . getChangesetEventT

-- | Similar to 'getChangesetEventT', unwrap a 'ChangesetEventT' and perform the state update.
runChangesetEventT :: (RightAction w s, Reflex t, Functor m) => ChangesetEventT t s w m a -> s -> m (Event t (s, a))
runChangesetEventT = fmap (fmap (fmap swap) . getCompose) . runChangesetT

{- | Merge changes from a 'DMap' of 'ChangesetT' computations performing events.

The initial state is the same for every change.

If several change events occur simultaneously, their changes are combined.

See also 'mergeChangesetEventT' for a generalisation.
-}
mergeChangesetEvent :: (Reflex t, GCompare k, Monoid w) => DMap k (ChangesetT s w (Event t)) -> ChangesetT s w (Event t) (DMap k Identity)
mergeChangesetEvent dmap = ChangesetT $ \s -> traverseWithKey (const (second Identity)) <$> mergeG (`getChangesetT` s) dmap

-- | Like 'mergeChangesetEvent', but generalised to include @m@ effects.
mergeChangesetEventT :: (Reflex t, GCompare k, Monoid w, Applicative m) => DMap k (ChangesetEventT t s w m) -> ChangesetEventT t s w m (DMap k Identity)
mergeChangesetEventT dmap = ChangesetT $ \s ->
  Compose $
    fmap (traverseWithKey (const (second Identity)))
      . mergeG getCompose
      <$> traverseWithKey (const (fmap Compose . getCompose . flip getChangesetT s)) dmap

-- | Merge a 'Map' of changes
mergeChangesetMap :: (Reflex t, Monoid w, Applicative m, Ord k) => Map k (ChangesetEventT t s w m a) -> ChangesetEventT t s w m (Map k a)
mergeChangesetMap = fmap dmapToMap . mergeChangesetEventT . mapWithFunctorToDMap

-- | Merge an 'IntMap' of changes
mergeChangesetIntMap :: (Reflex t, Monoid w, Applicative m) => IntMap (ChangesetEventT t s w m a) -> ChangesetEventT t s w m (IntMap a)
mergeChangesetIntMap = fmap dmapToIntMap . mergeChangesetEventT . intMapWithFunctorToDMap

-- | Merge a list of changes
mergeChangesetEventTs :: (Reflex t, Monoid w, Applicative m) => [ChangesetEventT t s w m a] -> ChangesetEventT t s w m (NonEmpty a)
mergeChangesetEventTs [] = ChangesetT $ const $ Compose $ pure never
mergeChangesetEventTs cs =
  cs
    & zip [0 ..]
    & IM.fromDistinctAscList
    & mergeChangesetIntMap
    <&> (fromList . IM.elems)
