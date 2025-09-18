module Data.Monoid.RightAction.Map where

-- containers
import Data.Map

-- changeset
import Data.Monoid.RightAction (RightAction (..))

{- | Insert or delete an element in a 'Map'.

To change an element in an 'Map', see the indexed changes in [@changeset-lens@](hackage.haskell.org/package/changeset-lens),
or t'Control.Monad.Trans.Changeset.ImapChange'.
-}
data MapChange k a
  = Insert k a
  | Delete k
  deriving (Show, Read, Eq, Ord, Functor)

instance (Ord k) => RightAction (MapChange k a) (Map k a) where
  actRight m (Insert k a) = insert k a m
  actRight m (Delete k) = delete k m
