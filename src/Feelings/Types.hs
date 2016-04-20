{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Feelings.Types where

import Data.Aeson
import Data.Aeson.Types
import Feelings.Batteries

-- | A phone number
newtype Phone =
  Phone Text deriving (Show, Eq, FromJSON, ToJSON)

-- | v2 lets you SMS
data Via =
   ViaPhone Phone
 | ViaWeb
 deriving (Show, Eq, Generic)

-- | A record in the database
data Feeling =
  Feeling UTCTime Text Via deriving (Show, Eq)

-- ⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜⚜

instance FromJSON Via where
instance ToJSON Via where

instance FromJSON Feeling where
  parseJSON (Object o) =
    Feeling <$> o .: "time"
            <*> o .: "text"
            <*> o .: "via"
  parseJSON invalid =
    typeMismatch "FromJSON Feeling" invalid

instance ToJSON Feeling where
  toJSON (Feeling time text_ via) =
    object ["time" .= time, "text" .= text_, "via" .= via]
