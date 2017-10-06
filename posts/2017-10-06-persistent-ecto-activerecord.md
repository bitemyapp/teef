---
title: Comparing Persistent with Ecto and ActiveRecord
tags: haskell, databases, education
---

Rejected title: You're not special

I saw this article comparing Ecto and ActiveRecord: [https://www.dailydrip.com/blog/ecto-vs-activerecord.html](https://www.dailydrip.com/blog/ecto-vs-activerecord.html)

I thought I would track alongside that post and show what the equivalent code looks like if you're using the [Persistent](https://github.com/yesodweb/persistent) Haskell library.

<!--more-->

# Some examples from their side-by-side comparison

I'll start by simply translating some small, simple examples linked to at the beginning of their article.

## Get all records

### ActiveRecord

```ruby
Model.all
```

### Ecto

```elixir
Repo.all(App.Model)
```

### Persistent

```haskell
getAllUsers :: DB [Entity User]
getAllUsers = selectList [] []
```

## Search by name

### ActiveRecord

```ruby
Model.find_by(name: name)
```

### Ecto

```elixir
Repo.one(from t in App.Model, where: t.name == ^name, limit: 1)
```

### Persistent

```haskell
getFirstUserByEmail :: Text -> DB (Maybe (Entity User))
getFirstUserByEmail email =
  selectFirst [UserEmail ==. email] []
```

## Fetch a single record based on id=1

### ActiveRecord

```ruby
Model.find(1)
```

### Ecto

```elixir
Model |> Repo.get!(1)
```

### Persistent

```haskell
getIdOneUser :: DB (Maybe (Entity User))
getIdOneUser = getEntity (toSqlKey 1)
```

# Comparing against the rest of the article

I'll do my usual thing and cite what the original article said, then reply with either code or prose.

> Let's talk about the main ideas behind Ecto, and try to compare it with ActiveRecord.

> Main difference
> ActiveRecord: We can represent data using: behaviors + state.

> Ecto: We need to represent data using: functions.

It's just functions and data in Haskell too.

> Active Record pattern
> ActiveRecord has a general pattern of accessing data used in Object-oriented languages. So, this is not specfically the Active Record pattern.

> Using ActiveRecord, we can do:

```ruby
artist = Artist.get(1)
artist.name = "Name"
artist.save
```

>This makes a lot of sense for Object-Oriented languages. Data has behavior and state. This is pretty straightforward. How does Ecto handle that?

> Repository Pattern

> As a functional language we don't have data with state, nor do we have behavior. We only have functions.

> In general, if you want to talk with the database, you need to talk with the Repository first.

```elixir
artist = Repo.get(Artist, 1)
changeset = Artist.changeset(artist, name: "Changed name")
Repo.update(changeset)
```

> If we check side-by-side what Active Record and repository does, we cannot see when Active Record touches the Database. We just do a save and it hits the database implicitly. In Ecto, you always interact with the database explicitly.

>Ecto will not talk to the database without you asking it to. Everything is totally explicit. Any interaction with the database should pass through the Repository.

This is true of Haskell too, it only talks to the database when you ask it to. Most people have a function named `runDB` or similar which makes it easy to audit what code actually talks to the database. It also serves to tell you where your transaction boundaries are which is tremendously helpful for atomicity and correctly using your SQL database. Here's the name-changing example above in Haskell:

```haskell
updateFirstUserName :: Text -> DB ()
updateFirstUserName newName = do
  update (toSqlKey 1) [UserName =. newName]
```

If you wanted something that could do the same for any primary key:

```haskell
updateFirstUserName' :: Key User -> Text -> DB ()
updateFirstUserName' userKey newName = do
  update userKey [UserName =. newName]

updateFirstUserName :: Text -> DB ()
updateFirstUserName = updateFirstUserName' (toSqlKey 1)
```

## Schema

>Schema is normally a map between your types and your database. But not necessarily.

>If we check the documentation:

>An Ecto schema is used to map any data source into an Elixir struct. One of such use cases is to map data coming from a repository, usually a table, into Elixir structs. 
>An interesting thing to mention is that we don't need a schema for using Ecto. We can bypass the use of Schemas by using the table name as a string. Schemas are very flexible.

>Here is an example of Schema definition in Ecto.

```elixir
defmodule SlackPosting.Journals.Post do
  use Ecto.Schema
  import Ecto.Changeset
  alias SlackPosting.Journals.Post

  schema "posts" do
    field :user_slack_id, :string
    field :user_name, :string
    field :text, :string
    many_to_many :tags, SlackPosting.Journals.Tag, join_through: SlackPosting.Journals.PostTag
    has_many :comments, SlackPosting.Journals.Comment

    timestamps()
  end

  @doc false
  def changeset(%Post{} = post, attrs) do
    post
    |> cast(attrs, [:text, :user_slack_id, :user_name])
    |> validate_required([:text, :user_slack_id])
  end
end
```

Then in Persistent, with a little more of the mentioned tables fleshed out:

```haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post
  userSlackId Text
  userName Text
  someText Text
  someOtherText Text
  deriving Show

Comment
  comment Text
  post PostId
  deriving Show
  
Tag
  tagName Text

PostTags
  tag TagId
  post PostId
|]
```

## Migrations

> Migrations
> Ecto also has migrations. This is not really different from what ActiveRecord offers to us.

```elixir
defmodule SlackPosting.Repo.Migrations.CreatePosts do
  use Ecto.Migration

  def change do
    create table(:posts) do
      add :text, :text
      add :user_slack_id, :string
      add :user_name, :string

      timestamps()
    end

  end
end
```

Persistent does too, but approaches it differently by focusing on generating fresh and differential migrations against the database rather than creating a migration DSL. The macros generate the code necessary to see what migrations it recommends or to run the migrations directly automatically. The usual `runDB` function for running a database action against the database works for this.

```haskell
dumpMigration :: DB ()
dumpMigration = printMigration migrateAll

runMigrations :: DB ()
runMigrations = runMigration migrateAll
```

If we were to dump the post/comment/tag schema from earlier for SQLite, the migration would look like:

```sql
CREATE TABLE "post"("id" INTEGER PRIMARY KEY,"user_slack_id" VARCHAR NOT NULL,"user_name" VARCHAR NOT NULL,"some_text" VARCHAR NOT NULL,"some_other_text" VARCHAR NOT NULL);

CREATE TABLE "comment"("id" INTEGER PRIMARY KEY,"comment" VARCHAR NOT NULL,"post" INTEGER NOT NULL REFERENCES "post");

CREATE TABLE "tag"("id" INTEGER PRIMARY KEY,"tag_name" VARCHAR NOT NULL);

CREATE TABLE "post_tags"("id" INTEGER PRIMARY KEY,"tag" INTEGER NOT NULL REFERENCES "tag","post" INTEGER NOT NULL REFERENCES "post");
```

## Changeset

I have no idea what this is about and their post doesn't make it clearer. Validation is orthogonal to Persistent, you usually validate stuff at the edges so that any value of type `MyModel` is only ever a valid value for that database table.

## Associations

We covered this a little earlier with the Post/Comment/Tag example but I'll explain a little:

```haskell
Comment
  comment Text
  post PostId
  deriving Show

Tag
  tagName Text

PostTags
  tag TagId
  post PostId
```

You can reference the primary key column of a model defined elsewhere in the quasiquoter, so `TagId` is something the code understands is primary key of the `Tag` table. From there, it's able to generate the foreign key relationships in the migrations automatically. It also gives you better type-safety with managing keys:

```
Prelude> :t Comment
Comment :: Text -> Key Post -> Comment
Prelude> let tagKey :: Key Tag; tagKey = toSqlKey 1
Prelude> Comment "my comment" tagKey

<interactive>:12:22: error:
    • Couldn't match type ‘Tag’ with ‘Post’
      Expected type: Key Post
        Actual type: Key Tag
    • In the second argument of ‘Comment’, namely ‘tagKey’
      In the expression: Comment "my comment" tagKey
      In an equation for ‘it’: it = Comment "my comment" tagKey
```

When our keys aren't just strings or numbers, we can avoid a lot of unnecessary mistakes!

## Lazy loading

>Ecto does not support Lazy Loading.

Persistent doesn't either. If you want to pull related data together you can do so via separate database actions in a transaction or you can use [Esqueleto](https://github.com/bitemyapp/esqueleto) to do in so:

```elixir
def list_posts do
    Repo.all(Post)
    |> Repo.preload([:comments, :tags])
end
```

Here's a somewhat serious (this is modeled after some production code I wrote) example of how to do this with Esqueleto on top of Persistent, by returning a mapping of posts, the comments on the posts, and all tags associated with the posts.

```haskell
tagsForPosts :: [Key Post] -> DB [(Key Post, Entity Tag)]
tagsForPosts postKeys =
  unValueThePostKeys $
  select $
  from $ \ ( postTag `InnerJoin` tag ) -> do
    on (tag ^. TagId
        E.==. postTag ^. PostTagTag)
    where_ (postTag ^. PostTagPost
            `in_` valList postKeys)
    return (postTag ^. PostTagPost, tag)
  where unValueThePostKeys :: DB [(E.Value (Key Post), Entity Tag)]
                           -> DB [(Key Post, Entity Tag)]
        unValueThePostKeys = (fmap . fmap) (first E.unValue)

updateMap :: (Ord k) => (v -> v) -> k -> v -> Map k v -> Map k v
updateMap f k v map =
  if M.member k map
  then M.adjust f k map
  else M.insert k v map

postsWithCommentsAndTags :: DB (Map
                                (Key Post)
                                (Entity Post, [Entity Comment], [Entity Tag]))
postsWithCommentsAndTags = do
  postsAndComments <- posts
  let postKeys = fmap (entityKey . fst) postsAndComments
  postKeysWithTags <- tagsForPosts postKeys
  let initialMap = postsInitialMap postsAndComments
      postsWithTags = addTagsToMap postKeysWithTags initialMap
  return postsWithTags
  where
    posts =
      select $
      from $ \ ( post
                 `InnerJoin`
                 comment ) -> do
        on (post ^. PostId
            E.==. (comment ^. CommentPost))
        return (post, comment)

    postsInitialMap :: [(Entity Post, Entity Comment)]
                    -> Map (Key Post) (Entity Post, [Entity Comment], [Entity Tag])
    postsInitialMap postsAndComments =
      foldl' insertPostCom M.empty postsAndComments
      where insertPostCom m (post, comment) =
              updateMap
              (\(post, comments, tags) -> (post, comment : comments, tags))
              (entityKey post) (post, [comment], []) m

    addTagsToMap :: [(Key Post, Entity Tag)]
                 -> Map (Key Post) (Entity Post, [Entity Comment], [Entity Tag])
                 -> Map (Key Post) (Entity Post, [Entity Comment], [Entity Tag])
    addTagsToMap postKeysTags initialMap =
      foldl' insertPostKeyTag initialMap postKeysTags
      where insertPostKeyTag :: Map (Key Post)
                                (Entity Post, [Entity Comment], [Entity Tag])
                             -> (Key Post, Entity Tag)
                             -> Map (Key Post)
                                (Entity Post, [Entity Comment], [Entity Tag])
            insertPostKeyTag m (postKey, tagEntity) =
              M.adjust
              (\(post, comment, tags) ->
                  (post, comment, tagEntity : tags))
              postKey
              m
```

Then running this code with some test data:

```haskell
migrateFixturesTest :: IO ()
migrateFixturesTest = do
  runDB $ do
    runMigrations
    pk1 <- insert $ Post "slack1" "name1" "" ""
    pk2 <- insert $ Post "slack2" "name2" "" ""
    pk3 <- insert $ Post "slack2" "name3" "" ""
    _ <- insert $ Comment "pk1 c1" pk1
    _ <- insert $ Comment "pk1 c2" pk1
    _ <- insert $ Comment "pk2 c3" pk2
    tg1 <- insert $ Tag "tag1"
    ptg1 <- insert $ PostTag tg1 pk1
    pwcat <- postsWithCommentsAndTags
    liftIO $ pPrint pwcat
```

We get the following output, which looks right!

```
Prelude> migrateFixturesTest 
Migrating: CREATE TABLE "post"("id" INTEGER PRIMARY KEY,"user_slack_id" VARCHAR NOT NULL,"user_name" VARCHAR NOT NULL,"some_text" VARCHAR NOT NULL,"some_other_text" VARCHAR NOT NULL)
Migrating: CREATE TABLE "comment"("id" INTEGER PRIMARY KEY,"comment" VARCHAR NOT NULL,"post" INTEGER NOT NULL REFERENCES "post")
Migrating: CREATE TABLE "tag"("id" INTEGER PRIMARY KEY,"tag_name" VARCHAR NOT NULL)
Migrating: CREATE TABLE "post_tag"("id" INTEGER PRIMARY KEY,"tag" INTEGER NOT NULL REFERENCES "tag","post" INTEGER NOT NULL REFERENCES "post")
fromList
  [ ( PostKey { unPostKey = SqlBackendKey { unSqlBackendKey = 1 } }
    , ( Entity
          { entityKey =
              PostKey { unPostKey = SqlBackendKey { unSqlBackendKey = 1 } }
          , entityVal =
              Post
                { postUserSlackId = "slack1"
                , postUserName = "name1"
                , postSomeText = ""
                , postSomeOtherText = ""
                }
          }
      , [ Entity
            { entityKey =
                CommentKey { unCommentKey = SqlBackendKey { unSqlBackendKey = 2 } }
            , entityVal =
                Comment
                  { commentComment = "pk1 c2"
                  , commentPost =
                      PostKey { unPostKey = SqlBackendKey { unSqlBackendKey = 1 } }
                  }
            }
        , Entity
            { entityKey =
                CommentKey { unCommentKey = SqlBackendKey { unSqlBackendKey = 1 } }
            , entityVal =
                Comment
                  { commentComment = "pk1 c1"
                  , commentPost =
                      PostKey { unPostKey = SqlBackendKey { unSqlBackendKey = 1 } }
                  }
            }
        ]
      , [ Entity
            { entityKey =
                TagKey { unTagKey = SqlBackendKey { unSqlBackendKey = 1 } }
            , entityVal = Tag { tagTagName = "tag1" }
            }
        ]
      )
    )
  , ( PostKey { unPostKey = SqlBackendKey { unSqlBackendKey = 2 } }
    , ( Entity
          { entityKey =
              PostKey { unPostKey = SqlBackendKey { unSqlBackendKey = 2 } }
          , entityVal =
              Post
                { postUserSlackId = "slack2"
                , postUserName = "name2"
                , postSomeText = ""
                , postSomeOtherText = ""
                }
          }
      , [ Entity
            { entityKey =
                CommentKey { unCommentKey = SqlBackendKey { unSqlBackendKey = 3 } }
            , entityVal =
                Comment
                  { commentComment = "pk2 c3"
                  , commentPost =
                      PostKey { unPostKey = SqlBackendKey { unSqlBackendKey = 2 } }
                  }
            }
        ]
      , []
      )
    )
  ]
```

This stuff could get abstracted away or code-gen'd but I haven't had cause to bother yet. Incidentally, this happens to be a decent example of how to pull together data associated by one-to-many and many-to-many relationships using Esqueleto and Persistent.

If you'd like a working, running git repository of what I did in this article, [take a look here](https://github.com/bitemyapp/persistent-activerecord-ecto).

Some relevant libraries:

- [Persistent](https://github.com/yesodweb/persistent)
- [Persistent SQLite](https://github.com/yesodweb/persistent-sqlite)
- [Persistent code-gen](https://github.com/yesodweb/persistent-template)
- [Esqueleto](https://github.com/bitemyapp/esqueleto), has the ability to perform joins and otherwise compose relational queries, whereas Persistent is limited to basic CRUD.
- [pretty-show](https://github.com/yav/pretty-show) has a nice pretty printer I used in my code.
