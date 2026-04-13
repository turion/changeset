{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | A fused-effects algebra for the 'Changeset' effect.

This module provides an effect type and algebra that mirror the API of
'Control.Monad.Changeset.Class.MonadChangeset'.
The 'Changeset' effect allows a computation to observe the current state
and to apply changes to it, using the right action of a monoid @w@ on a state type @s@.

See "Control.Monad.Trans.Changeset" for the full description of the changeset concept.

To use this effect, use 'sendChange' or 'sendCurrent'.
The effect is interpreted by the 'Control.Monad.Trans.Changeset.ChangesetT' transformer via its 'Algebra' instance.
-}
module Control.Effect.Changeset where

-- base
import Data.Bifunctor (first)
import Data.Kind (Type)

-- fused-effects
import Control.Algebra

-- changeset
import Control.Monad.Changeset.Class (MonadChangeset (..))
import Control.Monad.Trans.Changeset (ChangesetT (..))
import Data.Monoid.RightAction (RightAction)

-- | The 'Changeset' effect, parameterised by the state type @s@ and the change monoid @w@.
data Changeset s w (m :: Type -> Type) k where
  {- | Apply a change to the state.

  The 'RightAction' instance is used to mutate the state.
  -}
  Change :: w -> Changeset s w m ()
  -- | Observe the current state.
  Current :: Changeset s w m s

instance (RightAction w s, Monoid w, Algebra sig m) => Algebra (Changeset s w :+: sig) (ChangesetT s w m) where
  alg handler sig ctx = case sig of
    L (Change w) -> ctx <$ change w
    L Current -> (<$ ctx) <$> current
    R other -> ChangesetT $ \s ->
      thread ((\(w, x) -> first (mappend w) <$> getChangesetT x s) ~<~ handler) other (mempty, ctx)

{- | Send a 'Change' to the effect carrier.

A 'proxy' for @s@ is required to fix the state type unambiguously,
since @w@ alone may not determine @s@.
-}
sendChange :: forall s w sig m proxy. (Has (Changeset s w) sig m) => proxy s -> w -> m ()
sendChange _ = send . Change @_ @s

{- | Send a 'Current' to the effect carrier.

A 'proxy' for @w@ is required to fix the change type unambiguously,
since @s@ alone may not determine @w@.
-}
sendCurrent :: forall s w sig m proxy. (Has (Changeset s w) sig m) => proxy w -> m s
sendCurrent _ = send (Current @s @w)
