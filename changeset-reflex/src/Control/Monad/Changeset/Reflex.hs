module Control.Monad.Changeset.Reflex where

-- base
import Data.Bifunctor (Bifunctor (second))
import Data.Functor ((<&>))
import Data.Functor.Compose
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Functor.Identity (Identity (Identity))
import Data.Tuple (swap)
import Data.Function ((&))

-- monoid-extras
import Data.Monoid.Action

-- containers
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)

-- reflex
import Reflex.Class

-- dependent-map
import Data.Dependent.Map (  DMap,  delete,  insert,  traverseWithKey, )
import Data.GADT.Compare (GCompare)
import Data.Functor.Misc (dmapToIntMap, dmapToMap, intMapWithFunctorToDMap, mapWithFunctorToDMap)

-- changeset
import Control.Monad.Trans.Changeset (ChangesetT (..), runChangesetT)
import Data.Monoid.RightAction (RightAction)

-- | A change to a dependent map 'DMap'.
data DMapChange k f v
  = Insert (k v) (f v)
  | Delete (k v)

instance (GCompare k) => Action (DMapChange k f v) (DMap k f) where
  act (Insert k v) = insert k v
  act (Delete k) = delete k

{- | Merge changes from a 'DMap' of 'ChangesetT' computations performing events.

The initial state is the same for every change.

If several change events occur simultaneously, their changes are combined.
-}
mergeChangesetEvent :: (Reflex t, GCompare k, Monoid w) => DMap k (ChangesetT s w (Event t)) -> ChangesetT s w (Event t) (DMap k Identity)
mergeChangesetEvent dmap = ChangesetT $ \s -> traverseWithKey (const (second Identity)) <$> mergeG (`getChangesetT` s) dmap

-- | A functor that creates changes, and performs side effects in @m@ to create an 'Event'.
type ChangesetEventT t s w m = ChangesetT s w (Compose m (Event t))

changesetEventT :: (s -> m (Event t (w, a))) -> ChangesetEventT t s w m a
changesetEventT = ChangesetT . fmap Compose

changesetEventT_ :: (Functor m, Reflex t) => (s -> m (Event t w)) -> ChangesetEventT t s w m ()
changesetEventT_ = changesetEventT . fmap (fmap (fmap (, ())))

getChangesetEventT :: ChangesetEventT t s w m a -> s -> m (Event t (w, a))
getChangesetEventT = fmap getCompose . getChangesetT

getChangeEventT :: (Functor m, Reflex t) => ChangesetEventT t s w m a -> s -> m (Event t w)
getChangeEventT = fmap (fmap (fmap fst)) . getChangesetEventT

runChangesetEventT :: (RightAction w s, Reflex t, Functor m) => ChangesetEventT t s w m a -> s -> m (Event t (s, a))
runChangesetEventT = fmap (fmap (fmap swap) . getCompose) . runChangesetT


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
