{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Trans.Changeset.Examples where

-- base
import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Monoid (Dual (..), Endo (..), Last)
import Data.Tuple (swap)

-- mtl
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), modify, runState)
import Control.Monad.Writer (MonadWriter (..), runWriter)

-- witherable
import Witherable (mapMaybe)

-- changeset
import Control.Monad.Changeset.Class (MonadChangeset (..))
import Control.Monad.Trans.Changeset
import Data.Monoid.RightAction (RightAction (..))

-- tasty
import Test.Tasty (TestTree, testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- * 'ReaderT'

-- | 'ReaderT' is a special case of 'ChangesetT' when the changes are trivial.
type TrivialChangeReaderT r = ChangesetT r ()

instance {-# OVERLAPPING #-} (Monad m) => MonadReader r (TrivialChangeReaderT r m) where
  ask = current
  local = withCurrent

-- * 'WriterT'

-- | 'WriterT' is a special case of 'ChangesetT' when the current state is trivial.
type TrivialActionWriterT w = ChangesetT () w

instance RightAction w () where
  actRight _ _ = ()

instance {-# OVERLAPPING #-} (Monoid w, Monad m) => MonadWriter w (TrivialActionWriterT w m) where
  writer = ChangesetT . pure . pure . swap
  listen = ChangesetT . fmap (fmap (\(w, a) -> (w, (a, w)))) . getChangesetT
  pass = ChangesetT . fmap (fmap (\(w, (a, f)) -> (f w, a))) . getChangesetT

-- * 'StateT'

{- | 'StateT' is a special case of 'ChangesetT' when the changes are whole state values,
and only the last write matters.
-}
type LastWriteT s = ChangesetT s (Last s)

instance {-# OVERLAPPING #-} (Monad m) => MonadState s (LastWriteT s m) where
  state f = ChangesetT $ \s -> pure $ first pure $ swap $ f s

-- * Another state monad

{- | Endomorphism state monad.

There is a further, not so much studied state monad by choosing any state type @s@ and the @Endo s@ monoid.
-}
type EndoStateT s = ChangesetT s (Dual (Endo s))

instance {-# OVERLAPPING #-} (Monad m) => MonadState s (EndoStateT s m) where
  state f = ChangesetT $ \s -> pure (Dual $ Endo $ snd <$> f, fst $ f s)

type M = Changes (ListChange Int)

writerExample :: (MonadWriter M m) => m ((), M)
writerExample = listen $ pass $ do
  tell $ singleChange $ Cons 0
  tell $ singleChange $ Cons 23
  tell $ singleChange $ Cons 99
  tell $ singleChange Pop
  pure ((), mapMaybe $ \c -> guard (c /= Cons 23) $> c)

stateExample :: (MonadState Int m) => m Int
stateExample = do
  put 0
  put 1
  n <- get
  put 2
  put 3
  pure n

tests :: TestTree
tests =
  testGroup
    "Examples"
    [ testCase "Writer" $ runWriter writerExample @?= swap (getChangeset (writerExample :: TrivialActionWriterT M Identity ((), M)) mempty)
    , testCase "State" $ runState stateExample 99 @?= runChangeset (stateExample :: LastWriteT Int Identity Int) 99
    , testGroup
        "EndoStateT"
        [ testCase "modify" $ execChangeset (modify (+ 1) >> modify (+ 1) :: EndoStateT Int Identity ()) 0 @?= execChangeset (modify (+ 2) :: EndoStateT Int Identity ()) 0
        , testCase "get & put" $
            let inc = do
                  n <- get
                  put $ n + 1
             in execChangeset (inc >> inc :: EndoStateT Int Identity ()) 0 @?= 2
        ]
    ]
