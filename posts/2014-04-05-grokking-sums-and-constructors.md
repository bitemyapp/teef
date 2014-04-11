---
title: Grokking sum types, value constructors, and type constructors
---

Recently I had an experience where in the course of helping somebody with a problem, I developed an example that I thought would help people understand sum types, value constructors, and type constructors better.

To make this friendly for Haskell-naive folk out there, so here's a quick review.

``` haskell

-- this defines a new data type Car.
data Car = Car { manufacturer :: Manufacturer
               , maxSpeed     :: Int }
-- Car is both the type and value constructor.
-- Rule of thumb: 
-- left-hand side is type constructor
-- right-hand side is value constructor

data Manufacturer = Toyota | Honda | Ford | GM
-- sum type. The type constructor is Manufacturer.
-- The value constructors are Toyota, Honda, Ford, and GM.
```

Remember, types are based on sets. Sets are collections of unique objects. One of the main things one is concerned with, with regard to sets, is cardinality. A set of 3 unique objects is for our purposes equivalent to a type with 3 inhabitants.

`Manufacturer` is a sum type. It has four inhabitants. Only four, because each inhabiting type is a nullary type constructor and therefore represents a singleton, stand-alone value. But why is it a sum type? Because it's `OR`. A `Manufacturer` is `1+1+1+1`. A hypothetical `Manufacturer` that looked like:

``` haskell
data JapaneseMaker = Toyota | Honda
data AmericanMaker = GM | Ford | Chrysler

data Manufacturer = JapaneseManu JapaneseMaker
                    | AmericanManu AmericanMaker
```

would have 5 inhabitants. `JapaneseMaker` has cardinality 2, `AmericanMaker` 3 and Manufacturer is summing them by OR'ing the possibility of them together.

Car is a product type because it's `manufacturer` AND `maxSpeed`. The dual (categorical opposite) of a product type is a sum type. With product types, it's multiplied. So it's the inhabitants of `Manufacturer` MULTIPLIED by the inhabitants of `Int`. Sadly, this is a big number (18 quintillion) so it's that times the inhabitants of `Manufacturer`.

Regulating propagation of inhabitants of types throughout the program at the type level is where dependent types come in, but the point of this post is to show how when cardinality is limited, you avoid needing dependent types with no pragmas used!

So the beginner's problem began with this:

``` haskell
type Tree = [Fruit]
data Fruit = Apple | Orange deriving (Show)
```

How does one guarantee and verify (at the type level) that you can have a list of Apple's `[Apple]` only? Ordinarily, this faculty in the general case requires dependent types, but since our cardinality is constrained (as it was in the problem bringer's case) then we can use a fairly straightforward technique.

Namely, we're going to take the value constructors (`Apple`, `Orange`) in the sum type `Fruit` and lift them into their own types. Then we're going to proxy the types from the sum type and provide a way to escape the sum type.

``` haskell
-- this is handy later :)
import Data.Maybe (catMaybes)

-- new types lifted into being their own datatypes.
-- The string field wasn't obligatory, it was so I could 
-- distinguish values of the same type.

data Apple = Apple String deriving (Show)
data Orange = Orange String deriving (Show)

-- This the sum type with the proxied value constructors.
-- Instead of being nullary, they take a specific type as a parameter.
-- So jamming something that isn't an Apple or an Orange
-- in here still won't work.

data Fruit = FruitApple Apple | FruitOrange Orange deriving (Show)

-- one apple, one orange, these are the independent types embedded
-- in the sum type members. That's why it's type [Fruit]

exampleData :: [Fruit]
exampleData = [FruitApple (Apple "wheeee"), FruitOrange (Orange "I can be ignored")]

-- Maybe is a means of expressing generic non-determinism,
-- which is what we have to cope with when we're plucking
-- arbitrary members of a sum type out.

discriminateApple :: Fruit -> Maybe Apple
discriminateApple (FruitApple apple) = Just apple
discriminateApple _ = Nothing

-- voila!
-- λ> :t (catMaybes $ fmap discriminateApple exampleData)

convertedList :: [Apple]
convertedList = catMaybes $ fmap discriminateApple exampleData

-- this abuses the fail method in Monad, don't use this. The above is cleaner.
-- λ> :t [x | FruitApple x <- exampleData ]

alternateMethod :: [Apple]
alternateMethod = [x | FruitApple x <- exampleData]
```

Okay so how precisely did this work?

We went: `[Fruit] -> [Maybe Apple] -> [Apple]`, the main remaining source of non-determinism is the empty list case.

Nota bene: an alternative to discriminate apple is prisms
from Edward Kmett's wonderful `lens` library. You can avoid
writing your own "discriminators" and just derive the prisms
generically.

In our case, we would use prisms to cherry-pick FruitApple and destructure Apple out of the sum type wrapper, with a short-circuiting Maybe in the alternate case.

This solved the user's original problem, with no need for any additional pragma magic.

Just sum types.
