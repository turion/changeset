module Data.Monoid.RightAction.Map where

-- containers
import Data.Map
import Data.Set (Set)

-- changeset
import Data.Monoid.RightAction (RightAction (..))

{- | Insert or delete an element in a 'Map',
or restrict or delete the keys.

To change an element in an 'Map', see the indexed changes in [@changeset-lens@](hackage.haskell.org/package/changeset-lens),
or t'Control.Monad.Trans.Changeset.ImapChange'.
-}
data MapChange k a
  = Insert k a
  | Delete k
  | RestrictKeys (Set k)
  | WithoutKeys (Set k)
  deriving (Show, Read, Eq, Ord, Functor)

instance (Ord k) => RightAction (MapChange k a) (Map k a) where
  actRight m (Insert k a) = insert k a m
  actRight m (Delete k) = delete k m
  actRight m (RestrictKeys ks) = restrictKeys m ks
  actRight m (WithoutKeys ks) = withoutKeys m ks
