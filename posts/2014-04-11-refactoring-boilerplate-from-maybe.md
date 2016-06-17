---
title: Refactoring boilerplate from sum types
---

A simple demonstration of extracting boilerplate from Aeson code handling a `Just` and a `Nothing` case of a `Maybe` value.

<!--more-->

``` haskell
instance ToJSON Query where    
  toJSON (TermQuery (Term termField termValue) (Just boost)) =
    object ["term" .=
            object [termField .=
                    object ["value" .= termValue
                           , "boost" .= boost]]]
  toJSON (TermQuery (Term termField termValue) Nothing) =
    object ["term" .=
            object [termField .=
                    object ["value" .= termValue]]]
```

This needed DRY'd up, so I decided to take advantage of the Monoid instance on lists to merge them after turning the Maybe value into a 1 or 0 kv-pair list. I could probably be more clever about this, so if anybody has suggestions I'd like to hear them.

``` haskell
instance ToJSON Query where    
  toJSON (TermQuery (Term termField termValue) boost) =
    object ["term" .=
            object [termField .= object merged]]
    where
      base = ["value" .= termValue]
      boosted = case boost of
        (Just boostValue) -> ["boost" .= boostValue]
        Nothing           -> []
      merged = mappend base boosted
```
