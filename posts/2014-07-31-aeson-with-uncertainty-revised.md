---
title: Aeson with types that have lots of "maybes", v2
---

The previous edition of this post was cleaned up by a suggestion from another Haskeller. I share it with you here.

<!--more-->

# Previously

[The last article this is riffing off of](/posts/2014-07-30-aeson-with-uncertainty.html)

First, a new function [thanks to this comment.](http://www.reddit.com/r/haskell/comments/2c5dr5/aeson_with_types_that_have_lots_of_maybes/cjca34z)

```haskell
omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull where
  notNull (_, Null) = False
  notNull _         = True
```

Here's the old code

```haskell
-- for comments that explain components, see old article, link at top
instance ToJSON SimpleQueryStringQuery where
  toJSON (SimpleQueryStringQuery sqsQueryString
          sqsFields sqsBoolean sqsAnalyzer
          sqsFlags  sqsLowercaseExpanded sqsLocale) =
    object conjoined
    where base = [ "query" .= toJSON sqsQueryString ]
          maybeAdd =
            catMaybes [ mField "fields" sqsFields -- each mField is Maybe Pair
                      , mField "default_operator" sqsBoolean
                      , mField "analyzer" sqsAnalyzer
                      , mField "flags" sqsFlags
                      , mField "lowercase_expanded_terms" sqsLowercaseExpanded
                      , mField "locale" sqsLocale ]
          conjoined = base ++ maybeAdd
```

The new code speaks for itself, I think.

```haskell
instance ToJSON SimpleQueryStringQuery where
  toJSON SimpleQueryStringQuery {..} =
    omitNulls [ "query" .= toJSON simpleQueryStringQuery
              , "fields" .= simpleQueryStringField
              , "default_operator" .= simpleQueryStringOperator
              , "analyzer" .= simpleQueryStringAnalyzer
              , "flags" .= simpleQueryStringFlags
              , "lowercase_expanded_terms" .= simpleQueryStringLowercaseExpanded
              , "locale" .= simpleQueryStringLocale ]
```

# The moral of the story

Post your bad code. People will help you improve and you must not pass up that opportunity!
