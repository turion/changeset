{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore lensChangeset "Eta reduce" #-}
{-# HLINT ignore prismChangeset "Eta reduce" #-}
{-# HLINT ignore traversalChangeset "Eta reduce" #-}

module Control.Monad.Changeset.Lens where

-- base
import Prelude hiding (Foldable (..))

-- lens
import Control.Lens (Fold, Iso', Lens', Prism', Setter', Traversal', iso, over, to)

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

-- | Given a setter, change the part of the state it focusses.
changing :: (RightAction w a) => Setter' s a -> s -> w -> s
changing setter s w = over setter (`actRight` w) s

-- | Apply a change to each focussed value.
changed :: (RightAction w s) => w -> Fold s s
changed w = to (`actRight` w)

{- | Witness the isomorphism between state and torsor change.

A torsor change is a similar type to the state, but without a base point.
Given such a base point, a state value is converted to the change by 'differenceRight',
and a change is converted to a state value by 'actRight'.

In other words, when you fix a base state value,
this isomorphism will translate to and from the change necessary to apply to the base state to get to the given state.
-}
relativeTo :: (RightTorsor w s) => s -> Iso' s w
relativeTo sReference = iso (differenceRight sReference) (actRight sReference)
