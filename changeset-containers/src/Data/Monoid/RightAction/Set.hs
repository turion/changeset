module Data.Monoid.RightAction.Set where

-- containers
import Data.Set

-- changeset
import Data.Monoid.RightAction (RightAction (..))

-- | Insert or delete an element in a 'Set', or calculate the intersection, union, or difference with another set.
data SetChange k
  = Insert k
  | Delete k
  | Intersection (Set k)
  | Union (Set k)
  | Difference (Set k)
  deriving (Show, Read, Eq, Ord)

instance (Ord k) => RightAction (SetChange k) (Set k) where
  actRight s (Insert k) = insert k s
  actRight s (Delete k) = delete k s
  actRight s1 (Intersection s2) = intersection s1 s2
  actRight s1 (Union s2) = s1 `union` s2
  actRight s1 (Difference s2) = difference s1 s2
