{-# LANGUAGE UndecidableInstances #-}

{- | A general state monad transformer with separate types for the state and the possible changes, updates, commits, or diffs.

A typical example is a large state type (e.g., a user entry in a database of a webshop)
which only allows small changes (e.g., adding or deleting a delivery address):

@
data User = User
  { userName :: Text
  , password :: Hash
  , ...
  , addresses :: Map Text Address
  , ...
  }
@

When we want to be able to /restrict/ to specific changes (e.g., only the addresses should be changed),
and we want to be able to /inspect/ the changes,
then 'ChangesetT' is a good choice.
In our example, a general function on addresses, or even on the whole user, cannot be inspected.
But if we restrict to only adding or deleting addresses,
we can define a custom datatype such as:

@
data ChangeAddress
  -- | Add an address under a given key
  = Add Text Address
  -- | Delete the address for the given key
  | Delete Text
@

Changes for such a type (or rather, for the monoid @'Changes' ChangeAddress@) can be inspected.

'ChangesetT' is a very general state monad transformer.
It has all the standard state monads from @transformers@ as special cases:

+--------------------------+---------------+-------------+---------------------------------------------+
| Transformer special case | State type    | Monoid type | Intuition                                   |
+==========================+===============+=============+=============================================+
| @'WriterT' w@            | '()'          | @w@         | No possibility to observe the current state |
+--------------------------+---------------+-------------+---------------------------------------------+
| @'AccumT' w@             | @'Regular' w@ | @w@         | The state is the same type as the changes   |
+--------------------------+---------------+-------------+---------------------------------------------+
| @'StateT' s@             | @s@           | @First s@   | The change overwrites all previous changes  |
+--------------------------+---------------+-------------+---------------------------------------------+

The @changeset@ ecosystem has support for standard @containers@ and optics from @lens@
by providing the packages [@changeset-containers@](https://hackage.haskell.org/package/changeset-containers) and [@changeset-lens@](https://hackage.haskell.org/package/changeset-lens).

Orphan instances for newer (2.3) @mtl@ classes such as 'Control.Monad.Accum.MonadAccum' and 'Control.Monad.Selet.MonadSelect' can be found in "Control.Monad.Trans.Changeset.Orphan".
These are only provided for GHC >= 9.6.
-}
module Control.Monad.Trans.Changeset where

-- base
import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Coerce (coerce)
import Data.Foldable (Foldable (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Classes (Eq1, Ord1, Read1, Show1)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Kind (Type)
import Data.Monoid (Last (..))
import Data.Tuple (swap)
import Prelude hiding (Foldable ())

-- containers
import Data.Sequence (Seq, fromList, (|>))

-- transformers
import Control.Monad.Trans.Class

-- mtl
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Morph (MFunctor (..), MMonad (..))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Writer.Class (MonadWriter (..))

-- witherable
import Witherable (Filterable (..), FilterableWithIndex (..), Witherable (wither), (<&?>))

-- these
import Data.These (these)

-- semialign
import Data.Semialign (Align (..), Semialign (..), salign)

-- indexed-traversable
import Data.Foldable.WithIndex (FoldableWithIndex (..))
import Data.Functor.WithIndex (FunctorWithIndex (..))
import Data.Traversable.WithIndex (TraversableWithIndex (..))

-- changeset
import Control.Monad.Changeset.Class
import Data.Monoid.RightAction (RightAction (..), RightTorsor (..))

-- * The 'ChangesetT' monad transformer

{- | Hold a state of type @s@, which is allowed to be mutated by an action of a monoid @w.@

The state @s@ has the role of the current state.
An @a@ is computed while performing a side effect in @m@,
and these can depend on the current state.

The type @w@ encodes /changes/ (or updates, edits, commits, diffs, patches ...) to the state @s.@
This relation is captured by the 'RightAction' type class from @monoid-extras.@
It contains a method, @'act' :: w -> s -> s@,
which implements the semantics of @w@ as the type of updates to @s.@

The standard example is that of a big record where we only want to change a small portion:

@
data User = User
  { name :: Text
  , password :: Hash
  , ...
  , addresses :: Map Text Address
  , ...
  }
@

If all changes that our business logic should be able to perform are adding or deleting an address,
it would be cumbersome to work in a @'State' User@ monad, since we only want to modify a small portion.
Instead, we define a type of /changes/ to @User@:

@
data ChangeAddress
  -- | Add an address under a given key
  = Add Text Address
  -- | Delete the address for the given key
  | Delete Text

instance RightAction ChangeAddress User where
  act = ...
@

Now we can conveniently work in the monad @'ChangesetT' User [ChangeAddress] m.@
(Note the list type which gives us a free 'Monoid' instance.)
Here we can perform operations like @'change' [Add "home" homeAddress]@ or @'change' [Delete "work"]@ to modify the addresses,
'current' to view the current state (containing all changes so far),
or apply a more complex function like @'revise' $ const $ filter (/= Delete "default")@ which would remove all changes that attempt to delete the @"default"@ address.

As a further example, if @s@ represents some type of time stamps, then @w@ can be a type of durations:
Two timestamps cannot be added, but two durations can.
A computation in @'ChangesetT' s w@ could then have access to some simulated notion of "current time",
while being able to add symbolic "delays".

Another class of examples arises operation based or commutative Conflict-free Replicated Data Type (CRDT).
Then @s@ is the internal state (the "payload") of the CRDT, and @w@ is the update operation.
For example @s = Int@, and for @w@ we would define @data Count = Increment | Decrement.@

The 'Monad' and 'Applicative' classes are defined by performing the first action,
then 'act'ing with the monoid output onto the state, and then perform the second action with the updated state.
So for example, @'change' Increment >> 'current'@ is different from @'current' >>= (\n -> 'change' Increment >> return n)@:
If we apply @'flip' 'evalChangeset' 0@ to each,
the first one would return 1, while the second returns 0.

So, if at any point in a @do@ notation we want to inspect the current state,
we can assume that all previous changes have been applied.
In that sense, this monad behaves very much like any other state monad transformer.
-}
newtype ChangesetT s w m a = ChangesetT
  { getChangesetT :: s -> m (w, a)
  -- ^ Extract the changeset function without applying it to the state.
  }
  deriving (Functor)

-- ** Running a 'ChangesetT' action

-- | Extract the changes that would be applied.
getChangeT :: (Functor m) => ChangesetT s w m a -> s -> m w
getChangeT ChangesetT {getChangesetT} s = getChangesetT s <&> fst

-- | Run the action with an initial state and apply all resulting changes to it.
runChangesetT :: (Functor m, RightAction w s) => ChangesetT s w m a -> s -> m (a, s)
runChangesetT ChangesetT {getChangesetT} s = getChangesetT s <&> \(w, a) -> (a, actRight s w)

-- | Run the action with an initial state and extract only the value.
evalChangesetT :: (Functor m, RightAction w s) => ChangesetT s w m a -> s -> m a
evalChangesetT = fmap (fmap fst) . runChangesetT

-- | Run the action with an initial state and extract only the state.
execChangesetT :: (Functor m, RightAction w s) => ChangesetT s w m a -> s -> m s
execChangesetT = fmap (fmap snd) . runChangesetT

-- * 'ChangesetT' API with relaxed constraints

{- | See 'changeset'.

The @A@ suffix means that only 'Applicative' is required, not 'Monad'.
-}
changesetA :: (Applicative m) => (s -> (a, w)) -> ChangesetT s w m a
changesetA = ChangesetT . fmap (pure . swap)

{- | See 'change'.

The @A@ suffix means that only 'Applicative' is required, not 'Monad'.
-}
changeA :: (Applicative m) => w -> ChangesetT s w m ()
changeA w = ChangesetT $ const $ pure (w, ())

{- | See 'current'.

The @A@ suffix means that only 'Applicative' is required, not 'Monad'.
-}
currentA :: (Applicative m, Monoid w) => ChangesetT s w m s
currentA = ChangesetT $ \s -> pure (mempty, s)

instance (RightAction w s, Monoid w, Monad m) => MonadChangeset s w (ChangesetT s w m) where
  change = changeA
  current = currentA
  changeset = changesetA

-- | Like 'lift' from the 'MonadTrans' class, but with fewer constraints.
liftF :: (Functor m, Monoid w) => m a -> ChangesetT s w m a
liftF = ChangesetT . const . fmap (mempty,)

instance (RightAction w s, Monoid w) => MonadTrans (ChangesetT s w) where
  lift = liftF

-- ** Transforming 'ChangesetT' operations

{- | Change the action that would be applied.

The function in the second position of the tuple receives the initial state and the change that would be applied.
It has to output the action that will be applied instead.
-}
revise :: (Functor m) => ChangesetT s w m (a, s -> w -> w) -> ChangesetT s w m a
revise ChangesetT {getChangesetT} = ChangesetT $ \s -> getChangesetT s <&> \(w, (a, f)) -> (f s w, a)

-- | Adds the to-be-applied changes to the foreground value.
changelog :: (Functor m) => ChangesetT s w m a -> ChangesetT s w m (a, w)
changelog ChangesetT {getChangesetT} = ChangesetT $ fmap (\(w, a) -> (w, (a, w))) . getChangesetT

-- | Precomposes the current state with a function to  before computing the change.
withCurrent :: (s2 -> s1) -> ChangesetT s1 w m a -> ChangesetT s2 w m a
withCurrent f = ChangesetT . (. f) . getChangesetT

-- | Apply a function to the change.
mapChange :: (Functor m) => (w1 -> w2) -> ChangesetT s w1 m a -> ChangesetT s w2 m a
mapChange f = ChangesetT . fmap (fmap (first f)) . getChangesetT

-- ** Combining 'ChangesetT' operations

{- | Like '(<*>)' from 'Applicative', but ignore the change from the first action in the initial state for the second action.

This only needs an 'Applicative' constraint on @m@, not 'Monad'.
-}
(|*>) :: (Semigroup w, Applicative m) => ChangesetT s w m (a -> b) -> ChangesetT s w m a -> ChangesetT s w m b
ChangesetT mf |*> ChangesetT ma = ChangesetT $ \s -> (\(w1, f) (w2, a) -> (w1 <> w2, f a)) <$> mf s <*> ma s

-- | The @'Monad' m@ constraint is indeed necessary, since we need the log from the first action to change it to the state for the second action.
instance (Monoid w, RightAction w s, Monad m) => Applicative (ChangesetT s w m) where
  pure a = ChangesetT $ const $ pure (mempty, a)

  ChangesetT mf <*> ChangesetT ma = ChangesetT $ \s -> do
    (w1, f) <- mf s
    let !s' = actRight s w1
    (w2, a) <- ma s'
    pure (w1 <> w2, f a)

instance (RightAction w s, Monoid w, Monad m) => Monad (ChangesetT s w m) where
  ChangesetT ma >>= f = ChangesetT $ \s -> do
    (w1, a) <- ma s
    let !s' = actRight s w1
    (w2, b) <- getChangesetT (f a) s'
    return (w1 <> w2, b)

instance (Alternative m, Monoid w, RightAction w s, Monad m) => Alternative (ChangesetT s w m) where
  empty = liftF empty
  ChangesetT ma1 <|> ChangesetT ma2 = ChangesetT $ \s -> ma1 s <|> ma2 s

instance (Alternative m, Monoid w, RightAction w s, Monad m) => MonadPlus (ChangesetT s w m)

instance MFunctor (ChangesetT s w) where
  hoist = hoistF

-- | Like 'hoist' from the @mmorph@ package, but with no constraints.
hoistF :: (forall x. m x -> n x) -> ChangesetT s w m a -> ChangesetT s w n a
hoistF morph ma = ChangesetT $ morph . getChangesetT ma

instance (RightAction w s, Monoid w) => MMonad (ChangesetT s w) where
  embed f (ChangesetT g) = ChangesetT $ \s ->
    s
      & g
      & f
      & flip getChangesetT s
      <&> \(w1, (w2, b)) -> (w1 <> w2, b)

instance (MonadError e m, RightAction w s, Monoid w) => MonadError e (ChangesetT s w m) where
  throwError = lift . throwError
  catchError ma handler = ChangesetT $ \s -> getChangesetT ma s `catchError` (\e -> getChangesetT (handler e) s)

instance (MonadReader r m, RightAction w s, Monoid w) => MonadReader r (ChangesetT s w m) where
  ask = lift ask
  local f = hoist $ local f

instance (MonadRWS r w s m, RightAction w' s', Monoid w') => MonadRWS r w s (ChangesetT s' w' m)

instance (MonadState s m, RightAction w' s', Monoid w') => MonadState s (ChangesetT s' w' m) where
  state = lift . state

instance (MonadWriter w m, RightAction w' s, Monoid w') => MonadWriter w (ChangesetT s w' m) where
  writer = lift . writer
  listen = ChangesetT . fmap (fmap (\((w', a), w) -> (w', (a, w))) . listen) . getChangesetT
  pass = ChangesetT . fmap (pass . fmap (\(w', (a, f)) -> ((w', a), f))) . getChangesetT

-- * Pure changesets

{- | A pure changeset acts in the 'Identity' monad.
The only effects it has are inspecting the current state, and adding a change.

@'Changeset' s w a@ is isomorphic to @s -> (w, a).@
-}
type Changeset s w = ChangesetT s w Identity

-- | Like 'getChangesetT'.
getChangeset :: Changeset s w a -> s -> (w, a)
getChangeset swa s = runIdentity $ getChangesetT swa s

-- | Like 'getChangeT'.
getChange :: Changeset s w a -> s -> w
getChange swa s = runIdentity $ getChangeT swa s

-- | Like 'runChangesetT'.
runChangeset :: (RightAction w s) => Changeset s w a -> s -> (a, s)
runChangeset swa s = runIdentity $ runChangesetT swa s

-- | Like 'evalChangesetT'.
evalChangeset :: (RightAction w s) => Changeset s w a -> s -> a
evalChangeset swa s = runIdentity $ evalChangesetT swa s

-- | Like 'execChangesetT'.
execChangeset :: (RightAction w s) => Changeset s w a -> s -> s
execChangeset swa s = runIdentity $ execChangesetT swa s

-- * 'Changes': container for changes that don't have a 'Monoid' instance

{- | A collection of individual changes.

Often, we only want to define a type for single changes to a state.
In that case, 'Changes' is handy.
It serves as a container for changes that don't have a 'Monoid' or 'Semigroup' instance.
All changes are applied sequentially.

Mathematically, this is the free monoid over the type of single changes.

To inspect or edit 'Changes', see the type classes 'Functor', 'Foldable', 'Traversable', 'Filterable' and 'Witherable'.
-}
newtype Changes w = Changes {getChanges :: Seq w}
  deriving (Show, Read, Eq, Ord)
  deriving newtype (Semigroup, Monoid, Foldable, Functor, FunctorWithIndex Int, FoldableWithIndex Int)
  deriving (Traversable)

instance TraversableWithIndex Int Changes where
  itraverse f = fmap Changes . itraverse f . getChanges

instance Filterable Changes where
  mapMaybe f = Changes . mapMaybe f . getChanges

instance Witherable Changes where
  wither f = fmap Changes . wither f . getChanges

-- | Create 'Changes' from a list of changes.
changes :: [w] -> Changes w
changes = Changes . fromList

{- | Append a single change.

When @'addChange' w cs@ acts on a state with 'actRight', @w@ will be applied last.
-}
addChange :: w -> Changes w -> Changes w
addChange w = coerce (|> w)

-- | Create a 'Changes' from a single change.
singleChange :: w -> Changes w
singleChange = Changes . pure

-- | Apply a single change.
changeSingle :: (MonadChangeset s (Changes w) m) => w -> m ()
changeSingle = change . singleChange

-- | Apply all changes sequentially
instance (RightAction w s) => RightAction (Changes w) s where
  actRight s Changes {getChanges} = foldl' actRight s getChanges

instance (RightTorsor w s) => RightTorsor (Changes w) s where
  differenceRight = (singleChange .) . differenceRight

-- * Change examples

-- ** Changing lists

{- | A list can be changed by prepending an element, or removing one.

To change an element of a list, see the indexed changes from [@changeset-lens@](hackage.haskell.org/package/changeset-lens).
-}
data ListChange a
  = -- | Prepend an element
    Cons a
  | -- | Remove the first element (noop on an empty list)
    Pop
  deriving (Eq, Ord, Show, Read)
  deriving (Functor, Foldable, Traversable)

instance RightAction (ListChange a) [a] where
  actRight as (Cons a) = a : as
  actRight as Pop = drop 1 as

-- ** Changing integers

-- | An integer can be incremented by 1.
data Count = Increment
  deriving (Eq, Ord, Show, Read)

instance RightAction Count Int where
  actRight count Increment = count + 1

-- ** Changing 'Maybe's

-- | Change a 'Maybe' by either deleting the value or forcing it to be present.
newtype MaybeChange a = MaybeChange {getMaybeChange :: Last (Maybe a)}
  deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid)
  deriving (Functor, Foldable, Traversable)

instance RightAction (MaybeChange a) (Maybe a) where
  actRight aMaybe MaybeChange {getMaybeChange} = actRight aMaybe getMaybeChange

instance (Eq a) => RightTorsor (MaybeChange a) (Maybe a) where
  differenceRight Nothing Nothing = mempty
  differenceRight (Just aOrig) (Just aChanged) = if aOrig == aChanged then mempty else setJust aChanged
  differenceRight _ ma = setMaybe ma

-- | Set the state to the given 'Maybe' value.
setMaybe :: Maybe a -> MaybeChange a
setMaybe = MaybeChange . Last . Just

-- | Set the state to 'Just'.
setJust :: a -> MaybeChange a
setJust = setMaybe . Just

-- | Set the state to 'Nothing'.
setNothing :: MaybeChange a
setNothing = setMaybe Nothing

-- ** Changing 'Functor's

-- | Change a 'Functor' structure by applying a change for every element through 'fmap'.
newtype FmapChange (f :: Type -> Type) w = FmapChange {getFmapChange :: w}
  deriving newtype (Eq, Ord, Read, Show, Semigroup, Monoid)
  deriving (Functor, Foldable, Traversable)

instance (Functor f, RightAction w s) => RightAction (FmapChange f w) (f s) where
  actRight fs FmapChange {getFmapChange} = flip actRight getFmapChange <$> fs

-- *** Changing 'Maybe's as 'Functor's

-- | Apply changes only to 'Just' values.
type JustChange = FmapChange Maybe

-- | Apply changes only to 'Just' values.
justChange :: w -> JustChange w
justChange = FmapChange

-- ** Changing 'Filterable's

{- | Change all positions in a 'Filterable' in the same way.

For a 'Filterable' @f s@, the change type must have a 'RightAction' instance on @'Maybe' s@, not @s@,
where 'Nothing' denotes deletion of the position.

(If you don't want to delete positions, you are probably looking for 'FmapChange' or 'JustChange' instead.)
-}
newtype FilterableChange w = FilterableChange {getFilterableChange :: w}
  deriving newtype (Show, Eq, Ord, Read)
  deriving stock (Foldable, Functor, Traversable)

instance (Filterable f, RightAction w (Maybe s)) => RightAction (FilterableChange w) (f s) where
  actRight fs FilterableChange {getFilterableChange} = fs <&?> flip actRight getFilterableChange . Just

{- | Change or delete a position in a 'Filterable'.

This type in itself is not a 'RightAction',
but it is a building block for 'FilterableChange', which in turn is a 'RightAction' for a 'Filterable'.
-}
data FilterablePositionChange w
  = -- | Delete the value at this position
    FilterablePositionDelete
  | -- | Change the value at this position by acting with @w@ on it
    FilterablePositionChange w
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance (Semigroup w) => Semigroup (FilterablePositionChange w) where
  _ <> FilterablePositionDelete = FilterablePositionDelete
  FilterablePositionDelete <> _ = FilterablePositionDelete
  FilterablePositionChange w1 <> FilterablePositionChange w2 = FilterablePositionChange $ w1 <> w2

instance (Monoid w) => Monoid (FilterablePositionChange w) where
  mempty = FilterablePositionChange mempty

-- | For every position in a 'Filterable', change it according to its value, using 'mapMaybe'.
newtype FilterableChanges s w = FilterableChanges {getFilterableChanges :: s -> FilterablePositionChange w}
  deriving newtype (Semigroup, Monoid)
  deriving (Functor)

{- | Depending on the value at a position, change it or delete it.

@'Just' w@ applies the change @w@, while 'Nothing' deletes it.
-}
changeMaybe :: (s -> Maybe w) -> FilterableChanges s w
changeMaybe = FilterableChanges . fmap (maybe FilterablePositionDelete FilterablePositionChange)

instance (Filterable f, RightAction w s) => RightAction (FilterableChanges s w) (f s) where
  actRight fs FilterableChanges {getFilterableChanges} =
    fs <&?> \s -> case getFilterableChanges s of
      FilterablePositionDelete -> Nothing
      FilterablePositionChange w -> Just $ s `actRight` w

-- | For every position in a 'FilterableWithIndex', change it according to its value, using 'imapMaybe'.
newtype FilterableWithIndexChanges i s w = FilterableWithIndexChanges {getFilterableWithIndexChanges :: i -> s -> FilterablePositionChange w}
  deriving newtype (Semigroup, Monoid)
  deriving (Functor)

instance FunctorWithIndex i (FilterableWithIndexChanges i s) where
  imap f = FilterableWithIndexChanges . (\c i s -> f i <$> c i s) . getFilterableWithIndexChanges

instance (FilterableWithIndex i f, RightAction w s) => RightAction (FilterableWithIndexChanges i s w) (f s) where
  actRight fs FilterableWithIndexChanges {getFilterableWithIndexChanges} = flip imapMaybe fs $ \i s -> case getFilterableWithIndexChanges i s of
    FilterablePositionDelete -> Nothing
    FilterablePositionChange w -> Just $ s `actRight` w

-- ** Changing 'Filterable' 'Semialign's

{- | Change, set, or delete a position in a 'Filterable' that is also an instance of 'Semialign'.

This type is not a 'RightAction',
but it is the building block for 'AlignChanges', which is a 'RightAction' on a 'Filterable' 'Semialign'.
-}
data AlignPositionChange w s
  = -- | Delete the value at this position
    DeleteAlignPosition
  | -- | Change the value at this position (if it exists) by applying @w@
    ChangeAlignPosition w
  | -- | Set the value at this position, possibly creating it if it doesn't yet exist
    SetAlignPosition s
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Bifoldable AlignPositionChange where
  bifoldMap _ _ DeleteAlignPosition = mempty
  bifoldMap f _ (ChangeAlignPosition w) = f w
  bifoldMap _ f (SetAlignPosition s) = f s

instance Bifunctor AlignPositionChange where
  bimap _ _ DeleteAlignPosition = DeleteAlignPosition
  bimap f _ (ChangeAlignPosition w) = ChangeAlignPosition $ f w
  bimap _ f (SetAlignPosition s) = SetAlignPosition $ f s

instance Bitraversable AlignPositionChange where
  bitraverse _ _ DeleteAlignPosition = pure DeleteAlignPosition
  bitraverse f _ (ChangeAlignPosition w) = ChangeAlignPosition <$> f w
  bitraverse _ f (SetAlignPosition s) = SetAlignPosition <$> f s

instance (Semigroup w, RightAction w s) => Semigroup (AlignPositionChange w s) where
  _ <> DeleteAlignPosition = DeleteAlignPosition
  _ <> set@(SetAlignPosition _) = set
  DeleteAlignPosition <> ChangeAlignPosition _ = DeleteAlignPosition
  ChangeAlignPosition s1 <> ChangeAlignPosition s2 = ChangeAlignPosition $ s1 <> s2
  SetAlignPosition s <> ChangeAlignPosition w = SetAlignPosition $ s `actRight` w

instance (Monoid w, RightAction w s) => Monoid (AlignPositionChange w s) where
  mempty = ChangeAlignPosition mempty

{- | Change, set, or delete elements in a 'Filterable' that implements 'Semialign'.

Containers implementing 'Filterable' and 'Semialign' are fundamentally those that can be diffed easily.
Therefore, they have a general purpose 'RightTorsor' implementation.

Note that for the more special 'Zip' class, a simpler instance without a custom change type already exists.
-}
newtype AlignChanges (f :: Type -> Type) w s = AlignChanges {getAlignChanges :: f (AlignPositionChange w s)}
  deriving (Functor, Foldable, Traversable)

instance (Functor f) => Bifunctor (AlignChanges f) where
  bimap f g = AlignChanges . fmap (bimap f g) . getAlignChanges

instance (FunctorWithIndex i f) => FunctorWithIndex i (AlignChanges f w) where
  imap f = AlignChanges . imap (fmap . f) . getAlignChanges

instance (Foldable f) => Bifoldable (AlignChanges f) where
  bifoldMap f g AlignChanges {getAlignChanges} = foldMap (bifoldMap f g) getAlignChanges

instance (FoldableWithIndex i f) => FoldableWithIndex i (AlignChanges f w) where
  ifoldMap f = ifoldMap (foldMap . f) . getAlignChanges

instance (Traversable f) => Bitraversable (AlignChanges f) where
  bitraverse f g = fmap AlignChanges . traverse (bitraverse f g) . getAlignChanges

instance (TraversableWithIndex i f) => TraversableWithIndex i (AlignChanges f w) where
  itraverse f = fmap AlignChanges . itraverse (traverse . f) . getAlignChanges

-- The quantified constraints here are only needed for GHC <= 9.4, feel free to remove when support for these is dropped
deriving instance (Show1 f, (forall a. (Show a) => Show (f a)), Show w, Show s) => Show (AlignChanges f w s)

deriving instance (Read1 f, (forall a. (Read a) => Read (f a)), Read w, Read s) => Read (AlignChanges f w s)

deriving instance (Eq1 f, (forall a. (Eq a) => Eq (f a)), Eq w, Eq s) => Eq (AlignChanges f w s)

-- Also the Eq (AlignChanges f w s) superclass is only needed for GHC <= 9.4
deriving instance (Eq (AlignChanges f w s), Ord1 f, (forall b. (Ord b) => Ord (f b)), Ord w, Ord s) => Ord (AlignChanges f w s)

instance (Semialign f, Semigroup w, RightAction w s) => Semigroup (AlignChanges f w s) where
  AlignChanges ac1 <> AlignChanges ac2 = AlignChanges $ salign ac1 ac2

instance (Semigroup w, RightAction w s, Align f) => Monoid (AlignChanges f w s) where
  mempty = AlignChanges nil

instance (Semialign f, Filterable f, RightAction w s) => RightAction (AlignChanges f w s) (f s) where
  actRight fs AlignChanges {getAlignChanges} = catMaybes $ alignWith (these Just maybeCreateNew changeExisting) fs getAlignChanges
    where
      changeExisting sOrig = \case
        (SetAlignPosition sNew) -> Just sNew
        ChangeAlignPosition w -> Just $ sOrig `actRight` w
        DeleteAlignPosition -> Nothing
      maybeCreateNew = \case
        (SetAlignPosition s) -> Just s
        _ -> Nothing

instance (Semialign f, RightTorsor w s) => RightTorsor (AlignChanges f w s) (f s) where
  differenceRight = ((AlignChanges .) .) $ alignWith $ these (const DeleteAlignPosition) SetAlignPosition $ (ChangeAlignPosition .) . differenceRight

-- ** Changing 'FunctorWithIndex'

-- | Change a 'FunctorWithIndex' structure by applying the change to every element through 'imap'.
newtype ImapChange i w = ImapChange {getImapChange :: i -> w}
  deriving newtype (Semigroup, Monoid, Functor)

instance (RightAction w s, FunctorWithIndex i f) => RightAction (ImapChange i w) (f s) where
  actRight fs ImapChange {getImapChange} = imap (flip actRight . getImapChange) fs
