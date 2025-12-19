{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore lensChangeset "Eta reduce" #-}
{-# HLINT ignore prismChangeset "Eta reduce" #-}
{-# HLINT ignore traversalChangeset "Eta reduce" #-}

module Control.Monad.Changeset.Lens where

-- base
import Prelude hiding (Foldable (..))

-- lens
import Control.Lens (Lens', Prism', Traversal', Fold, Iso', iso, to, Setter', over)

-- changeset
import Data.Monoid.RightAction (RightAction (..), RightTorsor (..))

-- changeset-lens
import Control.Monad.Changeset.Lens.Setter

-- | Create a changeset that focusses on a part of the state via a lens.
lensChangeset :: Lens' s a -> w -> SetterChangeset s a w
lensChangeset l w = setterChangeset l w

-- | Create a changeset that only changes some variants of the state, which are specified by a prism.
prismChangeset :: Prism' s a -> w -> SetterChangeset s a w
prismChangeset p w = setterChangeset p w

-- | Create a changeset that changes those parts of a state which are traversed
traversalChangeset :: Traversal' s a -> w -> SetterChangeset s a w
traversalChangeset t w = setterChangeset t w

changing :: RightAction w a => Setter' s a -> s -> w -> s
changing setter s w = over setter (`actRight` w) s

changed :: RightAction w s => w -> Fold s s
changed w = to (`actRight` w)

relativeTo :: (RightTorsor w s) => s -> Iso' s w
relativeTo sReference = iso (differenceRight sReference) (actRight sReference)
