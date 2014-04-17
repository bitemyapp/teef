---
title: Parsing data of varying structure in Haskell with Aeson
---

Need to parse data with varying structure? Once again we resort to our old friend for handling non-determinism, the sum type.

``` haskell

module Main where

import Control.Applicative
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as L
import Data.HashMap.Lazy (HashMap, lookup, member)
import Data.Text (Text)
import Prelude hiding (lookup)

data User = User { userName :: String
                 , userAge  :: Int } deriving (Eq, Show)

data Employee = Employee { employeeName     :: String
                         , employeeCompany  :: String } deriving (Eq, Show)

data Manager  = Manager { managerName     :: String
                        , managerCompany  :: String
                        , reports  :: [Employee]} deriving (Eq, Show)

parseUser v = User <$>
              v .: "name" <*>
              v .: "age"

instance FromJSON User where
  parseJSON (Object v) = parseUser v
  parseJSON _          = empty

parseEmployee v = Employee <$>
                  v .: "name" <*>
                  v .: "company"

instance FromJSON Employee where
  parseJSON (Object v) = parseEmployee v
  parseJSON _          = empty


parseManager v = Manager <$>
                 v .: "name"    <*>
                 v .: "company" <*>
                 v .: "reports"

instance FromJSON Manager where
  parseJSON (Object v) = parseManager v
  parseJSON _          = empty

```

So, we're getting an array of objects who can be either User, Employee, or a Manager. I've included the FromJSON instances for parsing JSON data into these individual objects. Now we get to where we handle the non-determinism of, "which type is it?"


``` haskell

-- sum type for handling the possibility of any of the three types

data UmeWrapper = UmeUser User
                | UmeEmployee Employee
                | UmeManager Manager deriving (Eq, Show)

-- our "selector" for creating instances of UmeWrapper from
-- objects that might be user, employee, or manager.
-- <$> in the Applicative typeclass is fmap. We're fmap'ing
-- over the Parser container that Aeson kicks around.

parseUmeWrapper hasReports hasCompany value
  | hasReports = UmeManager  <$> parseManager value
  | hasCompany = UmeEmployee <$> parseEmployee value
  | otherwise  = UmeUser     <$> parseUser value

-- our FromJSON instance that does hashmappy lookups into the object
-- to test for the keys that allow us to discriminate. Specialize for
-- your own use-cases accordingly.

instance FromJSON UmeWrapper where
  parseJSON (Object v) = umeValue
    where hasReports = member "reports" v
          hasCompany = member "company" v
          umeValue   = parseUmeWrapper hasReports hasCompany v
  parseJSON _          = empty

-- test data!

testPayload = L.pack "[{\"name\": \"bite\", \"age\": 10.0}, {\"name\": \"Mortem3r\", \"company\": \"GameGrumps\"}, {\"name\": \"Arin\", \"company\": \"GameGrumps\", \"reports\": [{\"name\": \"Dan\", \"company\": \"GameGrumps\"}]}]"

main = print $ (eitherDecode testPayload :: Either String [UmeWrapper])

```

And the final result when we run main?

``` haskell

λ> main
Right [UmeUser (User {userName = "bite", userAge = 10}),UmeEmployee (Employee {employeeName = "Mortem3r", employeeCompany = "GameGrumps"}),UmeManager (Manager {managerName = "Arin", managerCompany = "GameGrumps", reports = [Employee {employeeName = "Dan", employeeCompany = "GameGrumps"}]})]

```

Winrar.
