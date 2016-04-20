{-# LANGUAGE NoImplicitPrelude #-}

module Feelings.Batteries
       ( sDelay
       , Day(..)
       , Text
       , UTCTime
       , getCurrentDirectory
       , getCurrentTime
       , liftIO
       , localDay
       , module BasePrelude
       , module Control.Lens
       , module Data.Text.Strict.Lens
       , serveDirectory
       , sundayStartWeek
       , present
       , dayOfWeek
       , isTimeFriday
       )where

import           Control.Concurrent.Suspend.Lifted (sDelay)
import           Control.Monad.Trans (liftIO)
import           Data.Text (Text)
import           Data.Time.Calendar (Day(..))
import           Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import           Data.Time.Clock (getCurrentTime, UTCTime)
import           Data.Time.LocalTime (localDay)
import qualified Data.Time.Zones as TZ
import qualified Data.Time.Zones.All as TZ
import           Snap.Util.FileServe (serveDirectory)
import           System.Directory (getCurrentDirectory)

import           BasePrelude hiding (lazy, (&), uncons, index)
import           Control.Lens hiding ((.=))
import           Data.Text.Strict.Lens

present :: (Show a) => a -> Text
present = view packed . show

dayOfWeek :: Day -> Int
dayOfWeek = snd . sundayStartWeek

isTimeFriday :: UTCTime -> Bool
isTimeFriday =
  (== 5)
    . dayOfWeek
    . localDay
    . TZ.utcToLocalTimeTZ (TZ.tzByLabel TZ.America__New_York)
