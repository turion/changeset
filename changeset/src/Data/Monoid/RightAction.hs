module Data.Monoid.RightAction where

-- base
import Data.Maybe (fromMaybe)
import Data.Monoid (Dual (..), Endo (..), Last (..))
import Data.Void (Void)

-- monoid-extras
import Data.Monoid.Action (Action (..), Regular (Regular))

-- * Right action

{- | A [right action](https://en.wikipedia.org/wiki/Group_action#Right_group_action) of @m@ on @s@.

Imagine @s@ to be a type of states, and @m@ a type of changes to @s@.

Laws:

* When @m@ is a 'Semigroup': @s \`actRight\` m1 \`actRight\` m2 == s \`actRight\` (m1 <> m2)@
* When @m@ is a 'Monoid': @s \`actRight\` 'mempty' == s@

The default implementation is the trivial action which leaves @s@ unchanged.

See also 'Action' from @monoid-extras@, which is a /left/ action.
-}
class RightAction m s where
  actRight :: s -> m -> s
  actRight s _ = s

infixl 5 `actRight`

instance RightAction () s

instance RightAction m ()

instance RightAction Void s

instance RightAction (Last s) s where
  actRight s (Last ms) = fromMaybe s ms

instance (Action m s) => RightAction (Dual m) s where
  actRight s (Dual m) = act m s

instance (Semigroup m) => RightAction m (Regular m) where
  actRight (Regular m1) m2 = Regular $ m1 <> m2

instance (RightAction m s) => RightAction (Maybe m) s where
  actRight s = maybe s (actRight s)

{- | Endomorphism type with reverse 'Monoid' instance.

The standard 'Endo' type has a left action on @s@ since its composition is defined as @Endo f <> Endo g = Endo (f . g).@
The "Right Endomorphism" type, on the other hand, has a right action.
Intuitively, it behaves like the 'Data.Function.&' operator:

@
s & f & g == s \`'actRight'\` rEndo f <> rEndo g
@
-}
type REndo s = Dual (Endo s)

-- | Create an endomorphism monoid that has a right action on @s.@
rEndo :: (s -> s) -> REndo s
rEndo = Dual . Endo

{- | Find the action that changed a state to another.
In other words, @m@ is a general purpose "diff" type for @s@.

The operation is an inverse to 'actRight'.

Laws:

@
s `differenceRight` (s `actRight` w) = w
sOrig `actRight` (sOrig `differenceRight` sActed) = sActed
@
When @m@ is a 'Monoid':
@
s `differenceRight` s = mempty
@

In group theory, this concept is called a [torsor](https://en.wikipedia.org/wiki/Principal_homogeneous_space).
See also https://hackage-content.haskell.org/package/monoid-extras/docs/Data-Monoid-Action.html#t:Torsor for the same concept,
but for left actions.
-}
class RightTorsor m s where
  differenceRight ::
    -- | The original state
    s ->
    -- | The changed, new state
    s ->
    m

instance RightTorsor () s where
  differenceRight _ _ = ()

instance (Eq s) => RightTorsor (Last s) s where
  differenceRight sOrig sActed = Last $ if sOrig == sActed then Nothing else Just sActed
