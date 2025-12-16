module Control.Monad.Changeset.Lens.Setter where

-- base
import Data.Foldable (Foldable)
import Data.Function ((&))
import Data.List (foldl')
import Data.Monoid (First (..))
import GHC.Generics (Generic)
import Prelude hiding (Foldable (..))

-- lens
import Control.Lens (Setter', (%~))

-- containers
import Data.Sequence (Seq)

-- monoid-extras
import Data.Monoid.RightAction

-- changeset
import Control.Monad.Changeset.Class (MonadChangeset (..))

{- | A single change focussed through a setter.

A setter may be any of a variety of optics: A 'Data.Lens.Lens', a 'Data.Lens.Prism', a 'Data.Lens.Traversal'.

The change of type @w@ has to apply to @a@,
which is a part of a bigger structure @s.@
It is paired with a setter, which allows it to act on that bigger structure.

If the bigger datastructure is indexed (it has an instance of 'Data.Lens.Ixed' or 'Data.Lens.At'),
then 'Control.Monad.Changeset.Lens.Ixed.IxedChangeset' or 'Control.Monad.Changeset.Lens.At.AtChangeset'
are probably better choices, because these can be inspected and changed better.
-}
data SetterChange s a w = SetterChange
  { setterChangeSetter :: Setter' s a
  , setterChangeChange :: w
  }
  deriving stock (Functor, Foldable, Traversable)

-- | A collection of 'SetterChange's, which are applied consecutively.
newtype SetterChangeset s a w = SetterChangeset
  {getSetterChangeset :: Seq (SetterChange s a w)}
  deriving newtype (Semigroup, Monoid)
  deriving stock (Functor, Foldable, Traversable, Generic)

instance (RightAction w a) => RightAction (SetterChangeset s a w) s where
  actRight s SetterChangeset {getSetterChangeset} = foldl' (\s' SetterChange {setterChangeSetter, setterChangeChange} -> s' & setterChangeSetter %~ flip actRight setterChangeChange) s getSetterChangeset

-- | Create a 'SetterChangeset' with a single change, focussing on a particular setter.
setterChangeset :: Setter' s a -> w -> SetterChangeset s a w
setterChangeset setterChangeSetter setterChangeChange = SetterChangeset $ pure $ SetterChange {setterChangeSetter, setterChangeChange}

{- | Change a value through a setter.

Essentially, this applies 'setterChangeset'.

Example:

@
-- Increments the value through a lens
someLens |>~ Increment
@
-}
(|>~) :: (MonadChangeset s (SetterChangeset s a w) m) => Setter' s a -> w -> m ()
setter |>~ w = change $ setterChangeset setter w

{- | Set a value through a setter.

A shorthand for '|>~' in combination with 'First'.

Example:

@
-- Sets the value behind the prism to a
somePrism .|>~ a
@
-}
(.|>~) :: (MonadChangeset s (SetterChangeset s a (First a)) m) => Setter' s a -> a -> m ()
setter .|>~ a = setter |>~ First (Just a)
