{-# LANGUAGE UndecidableInstances #-}

-- | A type class generalising the API of 'Control.Monad.Trans.Changeset.ChangesetT'.
module Control.Monad.Changeset.Class where

-- transformers
import Control.Monad.Trans.Class (MonadTrans (..))

-- mmorph
import Control.Monad.Morph (MFunctor (..))

-- changeset
import Data.Monoid.RightAction (RightAction, RightTorsor (differenceRight))

{- | Monads containing changeset state.

This usually implies that the 'Control.Monad.Trans.Changeset.ChangesetT' monad transformer is part of the monad transformer stack of @m.@
See its documentation for details.

Two laws for these methods boil down to the requirement that 'change' and 'current' are special cases of 'changeset':

@
  change w = changeset $ const ((), w)
  current = changeset (, mempty)
@

The central law ensures that future  states are affected by the past changes through right action:

@
forall MonadChangeset s w m
       f :: s -> (a, w)
       g :: a -> s -> (b, w) .
changeset f >>= (changeset . g)
  =
changeset $ \s -> let (a, w) = f s in g a $ s `actRight` w
@

This law has several easier to grasp corollaries:
@
change w >> current = do
  s <- current
  change w
  return $ s 'actRight' w

change w1 >> change w2 = change (w1 <> w2)

current s1 >> current s2 = current s2
@
-}
class (Monad m, Monoid w, RightAction w s) => MonadChangeset s w m | m -> s, m -> w where
  -- | Apply a changeset to the state.
  changeset ::
    -- | Receives the current state and has to output a value and a change.
    (s -> (a, w)) ->
    m a

  -- | Apply a change to the state.
  --
  --   The 'Action' instance is used to mutate the state.
  --
  --   This is a special case of 'changeset' where the current state is disregarded.
  change :: w -> m ()
  change w = changeset $ const ((), w)

  -- | Observe the current state.
  --
  --   This is a special case of 'changeset' where the state is not changed.
  current :: m s
  current = changeset (,mempty)

instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), MonadTrans t, MFunctor t, MonadChangeset s w m) => MonadChangeset s w (t m) where
  changeset = lift . changeset
  change = lift . change
  current = lift current

{- | Calculate the difference from the current state to the explicitly given state, and return it.

With a lawful @'RightTorsor' w s@ instance, it can be expected that after then applying the difference, the state is the explicitly given one:
After @diff s >>= change@, the current state is @s@.
-}
diff :: (RightTorsor w s, MonadChangeset s w m) => s -> m w
diff s = (`differenceRight` s) <$> current
