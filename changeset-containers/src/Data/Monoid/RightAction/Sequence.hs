module Data.Monoid.RightAction.Sequence where

import Data.Monoid.RightAction (RightAction (..))
import Data.Sequence

{- | Insert or delete an element at either end of a 'Seq'.

To change an element in a 'Seq', see the indexed changes in [@changeset-lens@](hackage.haskell.org/package/changeset-lens).
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
  deriving (Show, Read, Eq, Ord, Functor)

instance RightAction (SeqChange a) (Seq a) where
  actRight s (Cons a) = a <| s
  actRight s (Snoc a) = s |> a
  actRight s Uncons = case viewl s of
    EmptyL -> empty
    _ :< as' -> as'
  actRight s Unsnoc = case viewr s of
    EmptyR -> empty
    as' :> _ -> as'
