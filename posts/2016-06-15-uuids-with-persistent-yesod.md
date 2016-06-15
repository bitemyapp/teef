---
title: How to use UUID values with Persistent and Yesod
---

> Replying to: [Jezen Thomas writing about using UUIDs in Yesod](https://jezenthomas.com/using-uuids-in-yesod/) <br>
> Prior art: [Michael Xavier on UUID columns in Persistent ](http://michaelxavier.net/posts/2015-04-14-Adding-a-UUID-Column-to-a-Persistent-Table.html)

Alternate title: Same as the original, but with: "and no Control.Lens needed" tacked on.

This code is adapted from stuff we've written at work.

# Persistent / UUID integration

## Instances

Radioactive dumping ground for orphan instances. Adding the instances makes Persistent understand how to serialize and deserialize the UUID type. The orphans can be avoided if you use a `newtype`. The partiality is us being somewhat irresponsible and assuming we've guarded for UUID correctness at the boundaries.

```haskell
-- Note we're taking advantage of
-- PostgreSQL understanding UUID values,
-- thus "PersistDbSpecific"
instance PersistField UUID where
  toPersistValue u = PersistDbSpecific . B8.pack . UUID.toString $ u
  fromPersistValue (PersistDbSpecific t) =
    case UUID.fromString $ B8.unpack t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"
```

## Models

This is where we actually use the UUID type in our models.

```haskell
module MyCompany.DB.Models where

share
  [mkPersist sqlSettings,mkMigrate "migration"]
  [persistLowerCase|
User json sql=users
  email                  Email       sqltype=text
  UniqUserEmail        email
  uuid                   UUID        sqltype=uuid   default=uuid_generate_v4()
  UniqUserUuid         uuid
  deriving Show Read Eq Typeable
|]
```

We use the default `JSON` representation generated so that the format is predictable for the datatypes. I was a little queasy with this initially and it does mean we have to watch what happens to Aeson, but I believe net-net it reduces defects that reach production.

# Yesod PathPiece integration

PathPiece is the typeclass Yesod uses to deserialize `Text` data into a more structured type, so that something like the following:

```
!/#Subdomain/#NumberedSlug    SomeRouteR  GET
```

could work. Here `Subdomain` and `NumberedSlug` are domain-specific types we made to represent a _concept_ in our application in a type-safe manner. `PathPiece` often goes unnoticed by people new to Yesod, but it's good to understand. For a super simple example:

```haskell
newtype Subdomain = Subdomain Text
  deriving (Eq, Show, Read)

instance PathPiece Subdomain where
  toPathPiece (Subdomain t) = t
  fromPathPiece = Just . Subdomain
```

The PathPiece class itself isn't terribly complicated or interesting:

```haskell
-- https://hackage.haskell.org/package/path-pieces-0.2.1/docs/Web-PathPieces.html

-- S for "Strict"
class PathPiece s where
    fromPathPiece :: S.Text -> Maybe s
    toPathPiece :: s -> S.Text
```

To address the original post's code, I wouldn't have written that myself. I generally keep DB/IO stuff apart from things like forms. Partly this is because our web app repository is separate from our domain types / DB stuff repo, which sort of forces us to refactor things we might need to do more than once, or in a context that isn't a web app. The use of applicative style and the double-lifting was not idiomatic.

Alternate title rejected for being too snarky: I told the doctor it hurts when I move my arm a certain way. The doctor told me to stop moving my arm like that.
