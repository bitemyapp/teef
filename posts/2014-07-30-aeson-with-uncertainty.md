---
title: Aeson with types that have lots of "maybes"
---

Aeson is a real joy to use once you get into the swing of things, but
there are some patterns out there that end-users are left to discover for
themselves.

Given a data-type like:

```haskell
data SimpleQueryStringQuery =
  SimpleQueryStringQuery
    { simpleQueryStringQuery             :: QueryString
    , simpleQueryStringField             :: Maybe FieldOrFields
    , simpleQueryStringOperator          :: Maybe BooleanOperator
    , simpleQueryStringAnalyzer          :: Maybe Analyzer
    , simpleQueryStringFlags             :: Maybe [SimpleQueryFlag]
    , simpleQueryStringLowercaseExpanded :: Maybe LowercaseExpanded
    , simpleQueryStringLocale            :: Maybe Locale
    } deriving (Eq, Show)
```

This is a bit of a mess. There's uncertainty slapped all over the data-type. Only
`simpleQueryStringQuery` of type `QueryString` is guaranteed to exist. Everything
else might be `Nothing`.

Rather than break out these cases individually, I started using a pattern combining
`catMaybes` and a function I (inadvisedly) named `mField`.

```haskell
mField :: (ToJSON a, Functor f) => T.Text -> f a -> f (T.Text, Value)
mField field = fmap ((field .=) . toJSON)

-- alternately
mField :: ToJSON a => T.Text -> Maybe a -> Maybe (T.Text, Value)
mField field = fmap ((field .=) . toJSON)

-- or if I'm feeling a smart-arse (thanks lambdabot)
mField :: ToJSON a => T.Text -> Maybe a -> Maybe (T.Text, Value)
mField = fmap . (. toJSON) . (.=)

-- and a reminder for catMaybes
catMaybes :: [Maybe a] -> [a]
```

First, an aside. How is `catMaybes` even possible? e're conflating
the possibility of `[Nothing, Nothing, ...]` with the empty list case `[]`.
Anything `Just a` is kept.

```haskell
λ> catMaybes [Just 1, Nothing, Just 2]
[1,2]
```

Okay, so what does our final instance look like?

```haskell
-- Value is where we want to end up!
instance ToJSON SimpleQueryStringQuery where
  toJSON (SimpleQueryStringQuery sqsQueryString
          sqsFields sqsBoolean sqsAnalyzer
          sqsFlags  sqsLowercaseExpanded sqsLocale) =
    -- type Pair = (T.Text, Value)
    -- object :: [Pair] -> Value
    object conjoined
    where base = [ "query" .= toJSON sqsQueryString ] -- base is just whatever isn't 'Maybe'
          maybeAdd =
            -- the result of catMaybes on the list is [Pair], from [Maybe Pair]
            catMaybes [ mField "fields" sqsFields -- each mField is Maybe Pair
                      , mField "default_operator" sqsBoolean
                      , mField "analyzer" sqsAnalyzer
                      , mField "flags" sqsFlags
                      , mField "lowercase_expanded_terms" sqsLowercaseExpanded
                      , mField "locale" sqsLocale ]
          -- just concatenating two lists of Pair
          conjoined = base ++ maybeAdd
```

Want to see more? - [Check out Bloodhound, the library this code is from](https://github.com/bitemyapp/bloodhound)
