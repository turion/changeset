{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Trans.Changeset.Examples where

-- base
import Data.Bifunctor (Bifunctor (first))
import Data.Monoid (Endo (..), Last, Dual (..))
import Data.Tuple (swap)

-- monoid-extras
import Data.Monoid.Action

-- mtl
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Writer (MonadWriter (..))

-- changeset
import Control.Monad.Changeset.Class (MonadChangeset (..))
import Control.Monad.Trans.Changeset
import Test.Tasty (testGroup, TestTree)

-- * 'ReaderT'

-- | 'ReaderT' is a special case of 'ChangesetT' when the changes are trivial.
type TrivialChangeReaderT r = ChangesetT r ()

instance {-# OVERLAPPING #-} (Monad m) => MonadReader r (TrivialChangeReaderT r m) where
  ask = current
  local = withCurrent

-- * 'WriterT'

-- | 'WriterT' is a special case of 'ChangesetT' when the current state is trivial.
type TrivialActionWriterT w = ChangesetT () w

instance Action w () where
  act _ _ = ()

instance {-# OVERLAPPING #-} (Monoid w, Monad m) => MonadWriter w (TrivialActionWriterT w m) where
  writer = ChangesetT . pure . pure . swap
  listen = ChangesetT . fmap (fmap (\(w, a) -> (w, (a, w)))) . getChangesetT
  pass = ChangesetT . fmap (fmap (\(w, (a, f)) -> (f w, a))) . getChangesetT

-- * 'StateT'

{- | 'StateT' is a special case of 'ChangesetT' when the changes are whole state values,
and only the last write matters.
-}
type LastWriteT s = ChangesetT s (Last s)

-- FIXME should test these. Maybe even separate package so people don't accidentally import the orphans? Or just chuck em in a test suite?

instance {-# OVERLAPPING #-} (Monad m) => MonadState s (LastWriteT s m) where
  state f = ChangesetT $ \s -> return $ first pure $ swap $ f s

-- * Another state monad

{- | Endomorphism state monad.

There is a further, not so much studied state monad by choosing any state type @s@ and the @Endo s@ monoid.
It behaves a lot like @'State' s@ with one subtle difference:
When combining two computations, the modifications are both applied,
but they will both read from the initial state.
So for example, @modify (+ 1) >> modify (+ 1) = modify (+ 2)@,
but if we define @inc = get >>= (\n -> put n + 1)@, we have @inc >> inc = inc@.
This is because all the calls to 'get' receive the same initial state, and 'put' unconditionally writes the state.
In other words, 'modify' (or 'state' for that matter) is more powerful than 'get' and 'put' combined.
-}
type EndoStateT s = ChangesetT s (Dual (Endo s))

-- FIXME is it right that we use Dual here? Test!

instance {-# OVERLAPPING #-} (Monad m) => MonadState s (EndoStateT s m) where
  state f = ChangesetT $ \s -> return (Dual $ Endo $ snd <$> f, fst $ f s)


tests :: TestTree
tests = testGroup "Examples" [

  ]
