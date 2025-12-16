module Data.Monoid.RightAction.IntSet where

-- base
import GHC.Generics (Generic)

-- containers
import Data.IntSet

-- changeset
import Data.Monoid.RightAction (RightAction (..))

-- | Insert or delete an element in an 'IntSet'.
data IntSetChange
  = Insert Int
  | Delete Int
  deriving stock (Show, Read, Eq, Ord, Generic)

instance RightAction IntSetChange IntSet where
  actRight s (Insert k) = insert k s
  actRight s (Delete k) = delete k s
