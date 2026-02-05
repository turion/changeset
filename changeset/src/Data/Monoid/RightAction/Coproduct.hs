module Data.Monoid.RightAction.Coproduct where

-- base
import Data.Foldable (Foldable (foldl'), toList)
import Prelude hiding (Foldable (..))

-- containers
import Data.Sequence (Seq)

-- changeset
import Data.Monoid.RightAction

{- | The coproduct of two monoids is a monoid that can contain values of either constituent.

This is useful if you have two different actions on the same state type,
and want to combine them.

Note: The multiplication of this monoid is formal, so the same semantic values may have differing representations.
Therefore it's not advised to inspect the contents of a coproduct.
You should usually want to use 'normaliseCoproduct'.
-}
newtype (:+:) m n = Coproduct {getCoproduct :: Seq (Either m n)}
  deriving newtype (Semigroup, Monoid)

{- | Construct a coproduct value from the left constituent monoid.

Semantically, this is a monoid homomorphism: @inL m1 <> inL m2@ acts the same as @inL (m1 <> m2).@
-}
inL :: m -> m :+: n
inL = Coproduct . pure . Left

{- | Construct a coproduct value from the right constituent monoid.

Semantically, this is a monoid homomorphism: @inR m1 <> inR m2@ acts the same as @inR (m1 <> m2).@
-}
inR :: n -> m :+: n
inR = Coproduct . pure . Right

{- | Brings a coproduct into a canonical form, which is an alternating list of 'Left's and 'Right's.

(The list may start with a 'Left' or a 'Right', or be empty.)
-}
normaliseCoproduct :: (Semigroup m, Semigroup n) => m :+: n -> [Either m n]
normaliseCoproduct = normaliseCoproduct' . toList . getCoproduct
  where
    normaliseCoproduct' (Left m1 : Left m2 : emns) = normaliseCoproduct' $ Left (m1 <> m2) : emns
    normaliseCoproduct' (Right n1 : Right n2 : emns) = normaliseCoproduct' $ Right (n1 <> n2) : emns
    normaliseCoproduct' [] = []
    normaliseCoproduct' (emn : emns) = emn : normaliseCoproduct' emns

-- | Coproducts are compared after normalising
instance (Eq m, Eq n, Semigroup m, Semigroup n) => Eq (m :+: n) where
  mns1 == mns2 = normaliseCoproduct mns1 == normaliseCoproduct mns2

instance (RightAction m s, RightAction n s) => RightAction (m :+: n) s where
  actRight s mns = foldl' (flip $ either (flip actRight) (flip actRight)) s (getCoproduct mns)
