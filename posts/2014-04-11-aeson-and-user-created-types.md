---
title: Nested user-defined types with Aeson
---

Sometimes one knows only part of the structure to be parsed out of JSON ahead of time, with some of that structure being defined by a user or consumer of the API. The solution to this in general and when using Aeson to make the wrapper type parametric.

``` haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Client where

import Control.Applicative
import Control.Monad (liftM)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Data.Time.Clock as DTC
import GHC.Generics (Generic)
import Network.HTTP.Conduit

data Version = Version { number          :: T.Text
                       , build_hash      :: T.Text
                       , build_timestamp :: DTC.UTCTime
                       , build_snapshot  :: Bool
                       , lucene_version  :: T.Text } deriving (Show, Generic)

data (FromJSON a, ToJSON a) => Status a =
                     Status { ok      :: Bool
                            , status  :: Int
                            , name    :: T.Text
                            , version :: a
                            , tagline :: T.Text } deriving (Show)

instance ToJSON Version
instance FromJSON Version

instance (FromJSON a, ToJSON a) => FromJSON (Status a) where
  parseJSON (Object v) = Status <$>
                         v .: "ok" <*>
                         v .: "status" <*>
                         v .: "name" <*>
                         v .: "version" <*>
                         v .: "tagline"
  parseJSON _          = empty
```

When I decode the parameterized Status type, as long as I provide a type whose structure matches the JSON, I'll get my data. It would look something like:

``` haskell
decode jsonPayload :: Maybe (Status Version)

-- or if you want parse error strings.

eitherDecode jsonPayload :: Either String (Status Version)
```

I used the Aeson generics support to generate the required `FromJSON` instance for `Version`. Aeson generics didn't work with `Status`. I haven't gotten generics or template haskell to work for parametric types. The typeclass instance was trivial anyway.
