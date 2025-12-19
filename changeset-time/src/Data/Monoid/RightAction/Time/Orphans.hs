{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Monoid.RightAction.Time.Orphans where

-- time
import Data.Time

-- changeset
import Data.Monoid.RightAction

instance RightAction NominalDiffTime UTCTime where
  actRight = flip addUTCTime

instance RightTorsor NominalDiffTime UTCTime where
  differenceRight = diffUTCTime
