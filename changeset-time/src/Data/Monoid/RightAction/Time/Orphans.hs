{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Monoid.RightAction.Time.Orphans where

-- time
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock.TAI
import Data.Time.Local

-- changeset
import Data.Monoid.RightAction

instance RightAction NominalDiffTime UTCTime where
  actRight = flip addUTCTime

instance RightTorsor NominalDiffTime UTCTime where
  differenceRight = diffUTCTime

instance RightAction NominalDiffTime LocalTime where
  actRight = flip addLocalTime

instance RightAction DiffTime AbsoluteTime where
  actRight time diffTime = addAbsoluteTime diffTime time

-- | Uses 'addGregorianDurationRollOver'.
instance RightAction CalendarDiffDays Day where
  actRight time diffTime = addGregorianDurationRollOver diffTime time

-- | Adds a number of days
instance RightAction Integer Day where
  actRight time diffTime = addDays diffTime time
