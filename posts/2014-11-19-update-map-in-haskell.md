---
title: Code refactoring with pointfree style demoed via updateMap
---

Original link:

http://codereview.stackexchange.com/questions/57843/update-map-in-haskell/57850#57850

Their question:

> I wrote a function that inserts or updates a Map key value depending on whether the key is already in the map. If it is, then the value gets added to the already existing map's value.

```haskell
import Data.Map as Map

updateMap :: (Ord k, Num v) => k -> v -> Map k v -> Map k v
updateMap k v map = if member k map then Map.adjust (+ v) k map
                    else Map.insert k v map 
```

> Tests

```haskell
*Main> updateMap 1 1 $ Map.singleton 1 100
fromList [(1,101)]
*Main> updateMap 2 1 $ Map.singleton 1 100
fromList [(1,100),(2,1)]
*Main> updateMap 1 33 $ Map.singleton 1 100
fromList [(1,133)]
```

My answer:

```haskell
updateMapPF :: (Ord k, Num a) => k -> a -> Map k a -> Map k a
updateMapPF = Map.insertWith (+)

λ> updateMapPF 1 1 $ Map.singleton 1 100
fromList [(1,101)]
λ> updateMapPF 2 1 $ Map.singleton 1 100
fromList [(1,100),(2,1)]
λ> updateMapPF 1 33 $ Map.singleton 1 100
fromList [(1,133)]
```

Process:

Notice the use of Map.insert

Notice that you are mapping (+) over the possibility of having or not having a value

Notice that Data.Map has a function for applying functions to values, called Data.Map.insertWith. It's extremely common for collections to have a helper function for "insert data with a helper function / default value"

Notice the shared structure of the types of your function and insertWith

```haskell
insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a

updateMap  :: (Ord k, Num v) =>         k -> v -> Map k v -> Map k v
```

Realize that applying (+) to insertWith will make the types identical

```haskell
λ> :t insertWith

insertWith
  :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a

λ> :t insertWith (+)

insertWith (+) :: (Ord k, Num a) => k -> a -> Map k a -> Map k a
```

Validate assumptions with specific examples, more rigor is available with the use of QuickCheck.

```haskell
λ> updateMapPF 1 1 $ Map.singleton 1 100
fromList [(1,101)]

λ> updateMapPF 2 1 $ Map.singleton 1 100
fromList [(1,100),(2,1)]

λ> updateMapPF 1 33 $ Map.singleton 1 100
fromList [(1,133)]
```

This is a practically golden opportunity to demonstrate how eta reduction can simplify code and kill off redundant logic.
