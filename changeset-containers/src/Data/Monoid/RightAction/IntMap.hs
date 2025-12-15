module Data.Monoid.RightAction.IntMap where

-- base
import GHC.Generics (Generic)

-- containers
import Data.IntMap

-- changeset
import Data.Monoid.RightAction (RightAction (..))

{- | Insert or delete an element in an 'IntMap'.

To change an element in an 'IntMap', see the indexed changes in [@changeset-lens@](hackage.haskell.org/package/changeset-lens).

The general purpose changes t'Control.Monad.Trans.Changeset.FilterableChange',
t'Control.Monad.Trans.Changeset.FilterableChanges', t'Control.Monad.Trans.Changeset.FilterableWithIndexChanges',
and t'Control.Monad.Trans.Changeset.AlignChanges' also apply to 'IntMap's.
-}
data IntMapChange a
  = Insert Int a
  | Delete Int
  deriving stock (Show, Read, Eq, Ord, Functor, Generic)

instance RightAction (IntMapChange a) (IntMap a) where
  actRight m (Insert k a) = insert k a m
  actRight m (Delete k) = delete k m
