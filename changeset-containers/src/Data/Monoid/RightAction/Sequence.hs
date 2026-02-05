module Data.Monoid.RightAction.Sequence where

-- base
import Data.Foldable (toList)
import GHC.Generics (Generic)

-- transformers
import Control.Monad.Trans.State.Strict

-- containers
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence hiding (deleteAt, empty, insertAt, zipWith)
import Data.Sequence qualified as Seq

-- indexed-traversable
import Data.Foldable.WithIndex (FoldableWithIndex (..))
import Data.Functor.WithIndex (FunctorWithIndex (..))
import Data.Traversable.WithIndex (TraversableWithIndex (..))

-- changeset

import Control.Monad.Trans.Changeset (Changes (..))
import Data.Monoid.RightAction (RightAction (..), RightTorsor (..))

{- | Insert or delete an element at either end of a 'Seq'.

To change an element in a 'Seq', see the indexed changes in [@changeset-lens@](hackage.haskell.org/package/changeset-lens).

The general purpose changes 'Control.Monad.Trans.Changeset.FilterableChange',
'Control.Monad.Trans.Changeset.FilterableChanges', 'Control.Monad.Trans.Changeset.FilterableWithIndexChanges',
and 'Control.Monad.Trans.Changeset.AlignChanges' also apply to 'Seq'uences.
-}
data SeqChange a
  = -- | Prepend an element
    Cons a
  | -- | Append an element
    Snoc a
  | -- | Drop an element from the left
    Uncons
  | -- | Drop an element from the right
    Unsnoc
  deriving stock (Show, Read, Eq, Ord, Functor, Generic)

instance RightAction (SeqChange a) (Seq a) where
  actRight s (Cons a) = a <| s
  actRight s (Snoc a) = s |> a
  actRight s Uncons = case viewl s of
    EmptyL -> Seq.empty
    _ :< as' -> as'
  actRight s Unsnoc = case viewr s of
    EmptyR -> Seq.empty
    as' :> _ -> as'

{- | Change a sequence by deleting and inserting elements at a specified position.

The edit is not minimal or normalized.
For example, @'Control.Monad.Trans.Changeset.changes' ['InsertAt' 0 '()', 'DeleteAt' 0]@ performs a trivial action on a sequence,
but is not equal to the empty 'Changes'.
-}
data SeqEdit a
  = DeleteAt Int
  | InsertAt Int a
  deriving stock (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable)

{- | Shift the position of the edit by 1

The resulting edit applies the same change to a sequence prepended by one element:

@
a '<|' 'actRight' s edit = 'actRight' (a '<|' s) (shift edit)
@
-}
shift :: SeqEdit a -> SeqEdit a
shift (DeleteAt i) = DeleteAt $ i + 1
shift (InsertAt i a) = InsertAt (i + 1) a

instance RightAction (SeqEdit a) (Seq a) where
  actRight as = \case
    DeleteAt i -> Seq.deleteAt i as
    InsertAt i a -> Seq.insertAt i a as

{- | An edit script is a sequence of edits to a 'Seq'.

It is implemented as a 'Seq'uence of 'SeqEdit's.
The 'RightTorsor' instance implements the shortest edit between to sequences.

__Note__: This is not always a lawful 'RightTorsor', but in practice this doesn't matter.
The first law is:

@s `differenceRight` (s `actRight` es) = es@

That is, when an edit script acts on a sequence, it is recovered by 'differenceRight'.
This laws doesn't hold though if @es@ contains positions outside the range of @s@.
In practice, this doesn't matter though, because the action on @s@ of either side of the equation is still the same:

@s `actRight` (s `differenceRight` (s `actRight` es)) = s `actRight` es@
-}
newtype EditScript a = EditScript {getEditScript :: Changes (SeqEdit a)}
  deriving stock (Show, Read, Traversable, Generic, Foldable, Functor)
  deriving newtype (Eq, Ord, Semigroup, Monoid)

instance FunctorWithIndex Int EditScript where
  imap f (EditScript es) = EditScript $ imap (\i e -> f i <$> e) es

instance FoldableWithIndex Int EditScript

instance TraversableWithIndex Int EditScript where
  itraverse handler = fmap EditScript . itraverse (traverse . handler) . getEditScript

instance RightAction (EditScript a) (Seq a) where
  actRight s EditScript {getEditScript} = actRight s getEditScript

{- | Compute the minimum edit script.

Implements a memoized longest common subsequence search.
-}
instance (Ord a) => RightTorsor (EditScript a) (Seq a) where
  differenceRight asOrig0 asActed0 = EditScript $ Changes $ evalState (memoized (toList asOrig0) (toList asActed0)) M.empty
    where
      memoized :: [a] -> [a] -> State (Map ([a], [a]) (Seq (SeqEdit a))) (Seq (SeqEdit a))
      memoized [] asActed = pure $ Seq.fromList $ zipWith InsertAt [0 ..] asActed
      memoized asOrig [] = pure $ Seq.fromList $ DeleteAt 0 <$ asOrig
      memoized asOrig@(aOrig : asOrigTail) asActed@(aActed : asActedTail) =
        if aOrig == aActed
          then fmap shift <$> memoized asOrigTail asActedTail
          else do
            cache <- gets $ M.lookup (asOrig, asActed)
            case cache of
              Just edit -> pure edit
              Nothing -> do
                l <- memoized asOrig asActedTail
                r <- memoized asOrigTail asActed
                let result =
                      if Seq.length l < Seq.length r
                        then InsertAt 0 aActed <| (shift <$> l)
                        else DeleteAt 0 <| r
                modify' $ M.insert (asOrig, asActed) result
                pure result
