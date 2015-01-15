---
title: Strong types and testing - in Haskell
---

This is a respin of [Levi Notik's article](http://levinotik.com/strong-types-and-their-impact-on-testing/), Levi's was in Scala.

Quoting Levi Notik here because I whole-heartedly agree with him

> The basic point that I hope to get across in this post (and the potential follow-ups) is
> that by encoding our domain into strong types and avoiding side effects we can accomplish a few things:

> We can greatly limit the scope of our tests
> For the tests that still make sense to write, we can write very strong tests
> By using meaningful values and separating side-effecting functions from pure ones, we can more easily reason about our program

Okay so what are we doing?

> We’re going to write a simple service that takes a form filled out by a user, sends an email based on that form, and then records that event to a database.

First, let us do this the bad way.

```haskell
data Email = Email {
    toAddress     :: T.Text
  , fromAddress   :: T.Text
  , emailBody     :: T.Text
  , recipientName :: T.Text
} deriving (Eq, Show)
```

Well, this is not great. We can end up making mistakes like:

```haskell
main = do
  sendEmail myEmail
  where to   = "levi@startup.com"
        from = "chris@website.org"
        body = "hi!"
        name = "Levi"
        -- I've mixed up the order of from & to!
        myEmail = from to body name
        -- disastrous.
```

So let us give the type system a little more information.

```haskell
newtype ToAddress     = ToAddress     T.Text deriving (Eq, Show)
newtype FromAddress   = FromAddress   T.Text deriving (Eq, Show)
newtype EmailBody     = EmailBody     T.Text deriving (Eq, Show)
newtype RecipientName = RecipientName T.Text deriving (Eq, Show)

data Email = Email {
    toAddress     :: ToAddress
  , fromAddress   :: FromAddress
  , emailBody     :: EmailBody
  , recipientName :: RecipientName
} deriving (Eq, Show)
```

Now the type system should catch when we've mixed things up. This can
become very important when all your data is `String` this, `Integer`
that, yadda yadda. Newtypes are more informative and enforce things
better.

In the REPL (I'm using `cabal repl` with a throw-away project):

```haskell
Prelude> let to   = ToAddress "levi@startup.com"
Prelude> let from = FromAddress "chris@website.org"
Prelude> let body = EmailBody "hi!"
Prelude> let name = RecipientName "Levi"

Prelude> let myEmail = Email from to emailBody recipientName

<interactive>:13:21:
    Couldn't match expected type ‘ToAddress’
                with actual type ‘FromAddress’
    In the first argument of ‘Email’, namely ‘from’
    In the expression: Email from to emailBody recipientName

<interactive>:13:26:
    Couldn't match expected type ‘FromAddress’
                with actual type ‘ToAddress’
    In the second argument of ‘Email’, namely ‘to’
    In the expression: Email from to emailBody recipientName
```

Cool! Now we don't have to worry about transposition errors.

Now lets talk about validating emails. First a few notes. The original
article uses regex. Haskellers...aren't fond of regex. It tends to be
write-once understand-never. We also have really good parsing
libraries, so we tend to use those instead since they're often very
fast too.

Fortunately, [George Pollard](https://twitter.com/porges) has already
written an
[uncommonly rigorous email validation library](https://hackage.haskell.org/package/email-validate)
so I'm going to use that.

Business-y note: don't consider emails validated until they've been
activated via a link that was emailed. Syntactic validation means
*nada*. Consider email validation an opportunity to *help your user*
not make mistakes, not a punitive "I GATCHA" moment.

Moving right along.

Using George's library, I tested the `validate` function which has
type: `ByteString -> Either String Text.Email.Parser.EmailAddress`

Which is saying, "give me a ByteString and I'll give you either a
String explaining why the parse failed or an email address".

Using that we can hide the constructors for our `Email` data type and
make it an
[abstract data type](http://web.cecs.pdx.edu/~sheard/course/Cs163/Doc/AbstractDataTypes.html).

So I'm going to change my module declaration from:

```haskell
module Email where
```

Which by default will expose everything, to:

```haskell
module Email
       (  Email
       ,  validate
       , ToAddress(..)
       , FromAddress(..)
       , EmailBody(..)
       , RecipientName(..)
       )
        where
```

`(..)` is how you tell Haskell "export all constructors" - in Email's
case, I'm not. Instead, I'm going to write an export a
[smart constructor](https://www.haskell.org/haskellwiki/Smart_constructors).

First we're going to change the `Bool` we get from George's library
to a `Maybe` using the `bool` catamorphism.

```haskell
Prelude> import Data.Bool (bool)
Prelude> :t bool
bool :: a -> a -> Bool -> a

Prelude> bool 0 1 True
1
Prelude> bool 0 1 False
0
```

Here's my somewhat overwrought smart constructor for `Email`:

```haskell
mkEmail :: ToAddress
        -> FromAddress
        -> EmailBody
        -> RecipientName
        -> Maybe Email
mkEmail to@(ToAddress toTxt) from@(FromAddress fromTxt)
        body name = Email <$> mTo <*> mFrom <*> pure body <*> pure name
  where mValid      x = bool Nothing (Just x)
        mTo           = mValid to $ isValid $ encodeUtf8 toTxt
        mFrom         = mValid from $ isValid $ encodeUtf8 fromTxt
```

Now lets test it in my REPL:

```haskell
Prelude> let to   = ToAddress "levi@startup.com"
Prelude> let from = FromAddress "chris@website.org"
Prelude> let body = EmailBody "hi!"
Prelude> let name = RecipientName "Levi"

Prelude> mkEmail to from body name
Just (Email {toAddress = ToAddress "levi@startup.com",
             fromAddress = FromAddress "chris@website.org",
             emailBody = EmailBody "hi!",
             recipientName = RecipientName "Levi"})

Prelude> mkEmail (ToAddress "PLAID") from body name
Nothing
```

Walaaaaaaaa

But lets make this more informative, how do I know what failed? How do
I dispatch on what failed so I can tell the user what to fix?!

[Validation!](http://hackage.haskell.org/package/either-4.3.2/docs/Data-Either-Validation.html)

Behold:

```haskell
mkEmail :: ToAddress
        -> FromAddress
        -> EmailBody
        -> RecipientName
        -> Validation [EmailErrors] Email
mkEmail to@(ToAddress toTxt) from@(FromAddress fromTxt)
        body name = Email <$> toV <*> fromV <*> pure body <*> pure name
  where toB   = isValid $ encodeUtf8 toTxt
        fromB = isValid $ encodeUtf8 fromTxt
        toV   = bool (Failure [ToAddressDidntParse]) (Success to) toB
        fromV = bool (Failure [FromAddressDidntParse]) (Success from) fromB

data EmailErrors = ToAddressDidntParse
                 | FromAddressDidntParse deriving (Eq, Show)
```

From my REPL again:

```haskell
Prelude> let to   = ToAddress "levi@startup.com"
Prelude> let from = FromAddress "chris@website.org"
Prelude> let body = EmailBody "hi!"
Prelude> let name = RecipientName "Levi"

Prelude> mkEmail to from body name
Success (Email {toAddress = ToAddress "levi@startup.com",
                fromAddress = FromAddress "chris@website.org",
                emailBody = EmailBody "hi!",
                recipientName = RecipientName "Levi"})

Prelude> mkEmail (ToAddress "PLAID") from body name
Failure [ToAddressDidntParse]
Prelude> mkEmail (ToAddress "PLAID") (FromAddress "TROLOLOL") body name
Failure [ToAddressDidntParse,FromAddressDidntParse]
```

Bada bing. Now lets do it with JSON.

I'm just going to dump the whole schmiel this time.

```haskell
module Email
       ( Email
       , ToAddress(..)
       , FromAddress(..)
       , EmailBody(..)
       , RecipientName(..)
       ) where

import Control.Applicative
import Control.Monad (join, mzero)
import Data.Aeson (FromJSON, eitherDecode, Value(..), (.:), parseJSON)
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either (either)
import Data.Either.Validation
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Text.Email.Validate (isValid)


newtype ToAddress     = ToAddress     T.Text deriving (Eq, Show)
newtype FromAddress   = FromAddress   T.Text deriving (Eq, Show)
newtype EmailBody     = EmailBody     T.Text deriving (Eq, Show)
newtype RecipientName = RecipientName T.Text deriving (Eq, Show)

data Email = Email {
    toAddress     :: ToAddress
  , fromAddress   :: FromAddress
  , emailBody     :: EmailBody
  , recipientName :: RecipientName
} deriving (Eq, Show)

validateEmail :: T.Text -> Bool
validateEmail = isValid . encodeUtf8

type EmailValidation a = Validation [EmailErrors] a

mkEmail :: ToAddress
        -> FromAddress
        -> EmailBody
        -> RecipientName
        -> EmailValidation Email
mkEmail to@(ToAddress toTxt) from@(FromAddress fromTxt)
        body name = Email <$> toV <*> fromV <*> pure body <*> pure name
  where toB   = validateEmail toTxt
        fromB = validateEmail fromTxt
        toV   = bool (Failure [ToAddressDidntParse]) (Success to) toB
        fromV = bool (Failure [FromAddressDidntParse]) (Success from) fromB

data EmailErrors = ToAddressDidntParse
                 | FromAddressDidntParse
                 | BadJsonForEmail String deriving (Eq, Show)

goodJson :: BL.ByteString
goodJson = pack $
           unlines ["{\"to\":   \"levi@startup.com\",",
                    "\"from\": \"chris@website.org\",",
                    "\"body\": \"hello!\",",
                    "\"name\": \"Levi\"}"]

jsonMissingKey :: BL.ByteString
jsonMissingKey = pack $
          unlines ["{\"to\":   \"levi@startup.com\",",
                   "\"from\": \"chris@website.org\",",
                   "\"body\": \"hello!\"}"]

jsonBadEmail :: BL.ByteString
jsonBadEmail = pack $
          unlines ["{\"to\":   \"levi@startup.com\",",
                   "\"from\": \"chrisLOL\",",
                   "\"body\": \"hello!\",",
                   "\"name\": \"Levi\"}"]

instance FromJSON ToAddress where
  parseJSON (String v) = bool (fail "ToAddress failed validation")
                         (pure (ToAddress v)) (validateEmail v)
  parseJSON _ = mzero

instance FromJSON FromAddress where
  parseJSON (String v) = bool (fail "FromAddress failed validation")
                         (pure (FromAddress v)) (validateEmail v)
  parseJSON _ = mzero

instance FromJSON EmailBody where
  parseJSON (String v) = pure (EmailBody v)
  parseJSON _ = mzero

instance FromJSON RecipientName where
  parseJSON (String v) = pure (RecipientName v)
  parseJSON _ = mzero


instance FromJSON (EmailValidation Email) where
  parseJSON (Object v) = mkEmail     <$>
                         v .: "to"   <*>
                         v .: "from" <*>
                         v .: "body" <*>
                         v .: "name"
  parseJSON _          = mzero

parseEmailJSON :: BL.ByteString -> EmailValidation Email
parseEmailJSON = either (Failure . return . BadJsonForEmail) id . eitherDecode

main :: IO ()
main = do
  let printJSON = print . parseEmailJSON
  printJSON goodJson
  printJSON jsonMissingKey
  printJSON jsonBadEmail
```

Also I got help from a bot in improving my code:

```
05:17 < bitemyapp> @pl parseEmailJSON x = either (\y -> Failure [BadJsonForEmail y]) id $ eitherDecode x
05:17 < lambdabot> parseEmailJSON = either (Failure . return . BadJsonForEmail) id . eitherDecode
```

Here's the result in our REPL:

```haskell
Prelude> parseEmailJSON goodJson
Success (Email {toAddress = ToAddress "levi@startup.com",
                fromAddress = FromAddress "chris@website.org",
                emailBody = EmailBody "hello!",
                recipientName = RecipientName "Levi"})

Prelude> parseEmailJSON jsonMissingKey
Failure [BadJsonForEmail "key \"name\" not present"]

Prelude> parseEmailJSON jsonBadEmail
Failure [BadJsonForEmail "FromAddress failed validation"]
```

Not totally satisfied with the result, I'd like to lift the errors
properly but Aeson really likes it some `Either
String`...sadly. Should've left the `a` in `Either a b` unapplied.


## Update - later that evening


I got the Validation stuff lifted properly. I'm not yet fully
satisfied but this at least demonstrates how to do it.

From my REPL:

```haskell
Prelude> parseEmailJSON goodJson
Success (Email {toAddress = ToAddress "levi@startup.com",
                fromAddress = FromAddress "chris@website.org",
                emailBody = EmailBody "hello!",
                recipientName = RecipientName "Levi"})

Prelude> parseEmailJSON jsonMissingKey
Failure [BadJsonForEmail "key \"name\" not present"]
Prelude> parseEmailJSON jsonBadEmail
Failure [BadJsonFromAddress]

```

Code dump:

```haskell
module Email
       ( Email
       , ToAddress(..)
       , FromAddress(..)
       , EmailBody(..)
       , RecipientName(..)
       ) where

import Control.Applicative
import Control.Monad (join, mzero)
import Data.Aeson (FromJSON, eitherDecode, Value(..), (.:), parseJSON)
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either (either)
import Data.Either.Validation
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Text.Email.Validate (isValid)


newtype ToAddress     = ToAddress     T.Text deriving (Eq, Show)
newtype FromAddress   = FromAddress   T.Text deriving (Eq, Show)
newtype EmailBody     = EmailBody     T.Text deriving (Eq, Show)
newtype RecipientName = RecipientName T.Text deriving (Eq, Show)

data Email = Email {
    toAddress     :: ToAddress
  , fromAddress   :: FromAddress
  , emailBody     :: EmailBody
  , recipientName :: RecipientName
} deriving (Eq, Show)

validateEmail :: T.Text -> Bool
validateEmail = isValid . encodeUtf8

type EmailValidation a = Validation [EmailErrors] a

mkEmailV :: EmailValidation ToAddress
        -> EmailValidation FromAddress
        -> EmailValidation EmailBody
        -> EmailValidation RecipientName
        -> EmailValidation Email
mkEmailV to from body name = Email <$> to <*> from <*> body <*> name

data EmailErrors = ToAddressDidntParse
                 | FromAddressDidntParse
                 | BadJsonForEmail String
                 | BadJsonToAddress
                 | BadJsonFromAddress deriving (Eq, Show)

goodJson :: BL.ByteString
goodJson = pack $
           unlines ["{\"to\":   \"levi@startup.com\",",
                    "\"from\": \"chris@website.org\",",
                    "\"body\": \"hello!\",",
                    "\"name\": \"Levi\"}"]

jsonMissingKey :: BL.ByteString
jsonMissingKey = pack $
          unlines ["{\"to\":   \"levi@startup.com\",",
                   "\"from\": \"chris@website.org\",",
                   "\"body\": \"hello!\"}"]

jsonBadEmail :: BL.ByteString
jsonBadEmail = pack $
          unlines ["{\"to\":   \"levi@startup.com\",",
                   "\"from\": \"chrisLOL\",",
                   "\"body\": \"hello!\",",
                   "\"name\": \"Levi\"}"]

instance FromJSON (EmailValidation ToAddress) where
  parseJSON (String v) = bool (pure . Failure $ [BadJsonToAddress])
                         ((pure . pure) $ (ToAddress v)) (validateEmail v)
  parseJSON _ = mzero

instance FromJSON (EmailValidation FromAddress) where
  parseJSON (String v) = bool (pure . Failure $ [BadJsonFromAddress])
                         ((pure . pure) $ (FromAddress v)) (validateEmail v)
  parseJSON _ = mzero

instance FromJSON (EmailValidation EmailBody) where
  parseJSON (String v) = (pure . pure) $ (EmailBody v)
  parseJSON _ = mzero

instance FromJSON (EmailValidation RecipientName) where
  parseJSON (String v) = (pure . pure) $ (RecipientName v)
  parseJSON _ = mzero


instance FromJSON (EmailValidation Email) where
  parseJSON (Object v) =    mkEmailV <$>
                         v .: "to"   <*>
                         v .: "from" <*>
                         v .: "body" <*>
                         v .: "name"
  parseJSON _          = mzero

parseEmailJSON :: BL.ByteString -> EmailValidation Email
parseEmailJSON = either (Failure . return . BadJsonForEmail) id . eitherDecode

main :: IO ()
main = do
  let printJSON = print . parseEmailJSON
  printJSON goodJson
  printJSON jsonMissingKey
  printJSON jsonBadEmail
```


## Update - later later that evening


After some refactoring, I got to this:

```haskell
module Email
       ( Email
       , ToAddress(..)
       , FromAddress(..)
       , EmailBody(..)
       , RecipientName(..)
       ) where

import Control.Applicative
import Control.Monad (join, mzero)
import Data.Aeson
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either (either)
import qualified Data.Either.Validation as V
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Text.Email.Validate (isValid)


newtype Email         = Email         T.Text deriving (Eq, Show)
newtype ToAddress     = ToAddress     Email  deriving (Eq, Show)
newtype FromAddress   = FromAddress   Email  deriving (Eq, Show)
newtype EmailBody     = EmailBody     T.Text deriving (Eq, Show)
newtype RecipientName = RecipientName T.Text deriving (Eq, Show)

data EmailForm = EmailForm {
    toAddress     :: ToAddress
  , fromAddress   :: FromAddress
  , emailBody     :: EmailBody
  , recipientName :: RecipientName
} deriving (Eq, Show)

validateEmail :: T.Text -> Bool
validateEmail = isValid . encodeUtf8

validEmail :: T.Text -> Maybe Email
validEmail v = if validateEmail v then
                 Just (Email v)
                 else
                 Nothing

type EmailValidation a = V.Validation [EmailErrors] a

mkEmail :: ToAddress
        -> FromAddress
        -> EmailBody
        -> RecipientName
        -> EmailValidation EmailForm
mkEmail to@(ToAddress (Email toTxt)) from@(FromAddress (Email fromTxt))
        body name = EmailForm <$> toV <*> fromV <*> pure body <*> pure name
  where toB   = validateEmail toTxt
        fromB = validateEmail fromTxt
        toV   = bool (V.Failure [ToAddressDidntParse]) (V.Success to) toB
        fromV = bool (V.Failure [FromAddressDidntParse]) (V.Success from) fromB

mkEmailV :: EmailValidation ToAddress
        -> EmailValidation FromAddress
        -> EmailValidation EmailBody
        -> EmailValidation RecipientName
        -> EmailValidation EmailForm
mkEmailV to from body name = EmailForm <$> to <*> from <*> body <*> name

data EmailErrors = ToAddressDidntParse
                 | FromAddressDidntParse
                 | BadJsonForEmail String
                 | BadJsonToAddress
                 | BadJsonFromAddress
                 | BadJsonEmailBody
                 | BadJsonRecipientName deriving (Eq, Show)

goodJson :: BL.ByteString
goodJson = pack $
           unlines ["{\"to\":   \"levi@startup.com\",",
                    "\"from\": \"chris@website.org\",",
                    "\"body\": \"hello!\",",
                    "\"name\": \"Levi\"}"]

jsonMissingKey :: BL.ByteString
jsonMissingKey = pack $
          unlines ["{\"to\":   \"levi@startup.com\",",
                   "\"from\": \"chris@website.org\",",
                   "\"body\": \"hello!\"}"]

jsonBadEmail :: BL.ByteString
jsonBadEmail = pack $
          unlines ["{\"to\":   \"levi@startup.com\",",
                   "\"from\": \"chrisLOL\",",
                   "\"body\": \"hello!\",",
                   "\"name\": \"Levi\"}"]

instance FromJSON Email where
  parseJSON (String v) = bool (fail "Email failed validation")
                         (pure (Email v)) (validateEmail v)
  parseJSON _ = mzero

instance FromJSON ToAddress where
  parseJSON (String v) = case validEmail v of
    (Just _) -> pure (ToAddress (Email v))
    Nothing  -> fail "ToAddress email failed validation"
  parseJSON _ = mzero

instance FromJSON FromAddress where
  parseJSON (String v) = case validEmail v of
    (Just _) -> pure (FromAddress (Email v))
    Nothing  -> fail "FromAddress email failed validation"
  parseJSON _ = mzero

instance FromJSON EmailBody where
  parseJSON (String v) = pure (EmailBody v)
  parseJSON _ = mzero

instance FromJSON RecipientName where
  parseJSON (String v) = pure (RecipientName v)
  parseJSON _ = mzero

getJSON :: (FromJSON a, Applicative f) =>
           e -> (a -> b) -> Value -> f (V.Validation [e] b)
getJSON e f v = case from of
  (Error _) -> pure $ V.Failure [e]
  (Success a) -> pure $ V.Success (f a)
  where from = fromJSON v

instance FromJSON (EmailValidation ToAddress) where
  parseJSON = getJSON BadJsonToAddress ToAddress

instance FromJSON (EmailValidation FromAddress) where
  parseJSON = getJSON BadJsonFromAddress FromAddress

instance FromJSON (EmailValidation EmailBody) where
  parseJSON = getJSON BadJsonEmailBody id

instance FromJSON (EmailValidation RecipientName) where
  parseJSON = getJSON BadJsonRecipientName id


instance FromJSON (EmailValidation EmailForm) where
  parseJSON (Object v) =    mkEmailV  <$>
                         v .: "to"   <*>
                         v .: "from" <*>
                         v .: "body" <*>
                         v .: "name"
  parseJSON _          = mzero

parseEmailJSON :: BL.ByteString -> EmailValidation EmailForm
parseEmailJSON = either (V.Failure . return . BadJsonForEmail) id . eitherDecode

main :: IO ()
main = do
  let printJSON = print . parseEmailJSON
  printJSON goodJson
  printJSON jsonMissingKey
  printJSON jsonBadEmail
```

## Update - later later later that evening

```haskell
module Email
       ( Email
       , ToAddress(..)
       , FromAddress(..)
       , EmailBody(..)
       , RecipientName(..)
       ) where

import Control.Applicative
import Control.Monad (join, mzero)
import Data.Aeson
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either (either)
import qualified Data.Either.Validation as V
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Text.Email.Validate (isValid)


newtype Email         = Email         T.Text deriving (Eq, Show)
newtype ToAddress     = ToAddress     Email  deriving (Eq, Show)
newtype FromAddress   = FromAddress   Email  deriving (Eq, Show)
newtype EmailBody     = EmailBody     T.Text deriving (Eq, Show)
newtype RecipientName = RecipientName T.Text deriving (Eq, Show)

data EmailForm = EmailForm {
    toAddress     :: ToAddress
  , fromAddress   :: FromAddress
  , emailBody     :: EmailBody
  , recipientName :: RecipientName
} deriving (Eq, Show)

validateEmail :: T.Text -> Bool
validateEmail = isValid . encodeUtf8

validEmail :: T.Text -> Maybe Email
validEmail v = if validateEmail v then
                 Just (Email v)
                 else
                 Nothing

type EmailValidation a = V.Validation [EmailErrors] a

mkEmail :: ToAddress
        -> FromAddress
        -> EmailBody
        -> RecipientName
        -> EmailValidation EmailForm
mkEmail to@(ToAddress (Email toTxt)) from@(FromAddress (Email fromTxt))
        body name = EmailForm <$> toV <*> fromV <*> pure body <*> pure name
  where toB   = validateEmail toTxt
        fromB = validateEmail fromTxt
        toV   = bool (V.Failure [ToAddressDidntParse]) (V.Success to) toB
        fromV = bool (V.Failure [FromAddressDidntParse]) (V.Success from) fromB

mkEmailV :: EmailValidation ToAddress
        -> EmailValidation FromAddress
        -> EmailValidation EmailBody
        -> EmailValidation RecipientName
        -> EmailValidation EmailForm
mkEmailV to from body name = EmailForm <$> to <*> from <*> body <*> name

data EmailErrors = ToAddressDidntParse
                 | FromAddressDidntParse
                 | BadJsonForEmail String
                 | BadJsonToAddress String
                 | BadJsonFromAddress String
                 | BadJsonEmailBody String
                 | BadJsonRecipientName String deriving (Eq, Show)

goodJson :: BL.ByteString
goodJson = pack $
           unlines ["{\"to\":   \"levi@startup.com\",",
                    "\"from\": \"chris@website.org\",",
                    "\"body\": \"hello!\",",
                    "\"name\": \"Levi\"}"]

jsonMissingKey :: BL.ByteString
jsonMissingKey = pack $
          unlines ["{\"to\":   \"levi@startup.com\",",
                   "\"from\": \"chris@website.org\",",
                   "\"body\": \"hello!\"}"]

jsonBadEmail :: BL.ByteString
jsonBadEmail = pack $
          unlines ["{\"to\":   \"levi@startup.com\",",
                   "\"from\": \"chrisLOL\",",
                   "\"body\": \"hello!\",",
                   "\"name\": \"Levi\"}"]

instance FromJSON Email where
  parseJSON (String v) = bool (fail "Email failed validation")
                         (pure (Email v)) (validateEmail v)
  parseJSON _ = mzero

maybeEmail eStr f v = maybe (fail eStr) (const (pure (f v))) (validEmail v)

instance FromJSON ToAddress where
  parseJSON (String v) = maybeEmail "ToAddress email failed validation"
                         (ToAddress . Email) v
  parseJSON _ = mzero

instance FromJSON FromAddress where
  parseJSON (String v) = maybeEmail "FromAddress email failed validation"
                         (FromAddress . Email) v
  parseJSON _ = mzero

instance FromJSON EmailBody where
  parseJSON (String v) = pure (EmailBody v)
  parseJSON _ = mzero

instance FromJSON RecipientName where
  parseJSON (String v) = pure (RecipientName v)
  parseJSON _ = mzero

getJSON :: (FromJSON a, Applicative f) =>
           (String -> e) -> (a -> b) -> Value -> f (V.Validation [e] b)
getJSON e f v = case from of
  (Error str)   -> pure $ V.Failure [e str]
  (Success a)   -> pure $ V.Success (f a)
  where from = fromJSON v

instance FromJSON (EmailValidation ToAddress) where
  parseJSON = getJSON BadJsonToAddress ToAddress

instance FromJSON (EmailValidation FromAddress) where
  parseJSON = getJSON BadJsonFromAddress FromAddress

instance FromJSON (EmailValidation EmailBody) where
  parseJSON = getJSON BadJsonEmailBody id

instance FromJSON (EmailValidation RecipientName) where
  parseJSON = getJSON BadJsonRecipientName id


instance FromJSON (EmailValidation EmailForm) where
  parseJSON (Object v) =    mkEmailV  <$>
                         v .: "to"   <*>
                         v .: "from" <*>
                         v .: "body" <*>
                         v .: "name"
  parseJSON _          = mzero


parseEmailJSON :: BL.ByteString -> EmailValidation EmailForm
parseEmailJSON = either (V.Failure . return . BadJsonForEmail) id . eitherDecode

main :: IO ()
main = do
  let printJSON = print . parseEmailJSON
  printJSON goodJson
  printJSON jsonMissingKey
  printJSON jsonBadEmail
```
