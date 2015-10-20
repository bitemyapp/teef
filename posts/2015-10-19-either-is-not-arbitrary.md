---
title: Either and (,) in Haskell are not arbitrary
---

Alternate title: Unnecessary particularity considered harmful

Since I'd rather explain this in O(1) rather than O(twitter) time, this is a brief rundown of why the way type constructors and constructor classes work in Haskell is not arbitrary. The post is not a tutorial on higher-kinded types, constructor classes, or functor. Don't know these things? [I write stuff](http://haskellbook.com) so you can [learn 'em](https://github.com/bitemyapp/learnhaskell).

First, the data types we're dealing with:

```haskell
data Either a b =
    Left a
  | Right b

-- sorta fake
data (,) a b =
  (a, b)
```

We'll use Functor to make the point, and Functor looks like this:

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

Some of the [post-FTP](https://www.reddit.com/r/haskell/comments/3okick/foldable_for_nonhaskellers_haskells_controversial/) drama has included people asserting that the way the Foldable instances for `Either` and `(,)` work is arbitrary. Not so. They work on the same principle as the Functor instances:

```
Prelude> fmap (+1) (Right 1)
Right 2
Prelude> fmap (+1) (Left "blah")
Left "blah"

Prelude> fmap (+1) (0, 0)
(0,1)
```

The first thing to recognize is that `Left` and `Right` in `Either` mean nothing to your program and similarly the first and second positions in `(,)` mean nothing in and of themselves. Because type constructors in Haskell work the same way as data constructors and functions in general do, the way their instances work is the _only_ way they could work. `Either` and `(,)` will always have one type argument that gets mapped and one that does not. It doesn't really matter which data constructor that is; we can only benefit by letting the consistent semantics of Haskell pick the type argument for us.

The only useful purpose a Functor for Either can ever have is to have one type which is transformed by the lifted functions and one which is not. If you want to be able to pick arbitrary targets, then you want lenses rather than a typeclass. If you want to able to transform both, then you want [Bifunctor](http://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Bifunctor.html#t:Bifunctor).

Note that with `bimap` in the `Bifunctor` class you have to provide _two_ functions you're mapping rather than one because the types `a` and `b` _could_ vary and be different types. Even if they are the same type, you can't write the Functor instance as if they were because the `Either` and `(,)` are defined with two distinct type arguments.

If you want a "tuple" of values that were all of the same type...well, go ahead. You can write it yourself:

```haskell
-- We use this in the book to demonstrate how
-- type constructors and constructor classes
-- work in Haskell, as it happens.
data Pair a =
  Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')
```

Then to see how the Functor for this behaves:

```
Prelude> fmap (+1) (Pair 1 1)
Pair 2 2
Prelude> fmap show (Pair 1 1)
Pair "1" "1"
Prelude> fmap show (Pair 1 9001)
Pair "1" "9001"
```

A Functor for `(,)` can only ever map over _one_ of the fields of the type. It might as well be the one that occurs naturally from the order of arguments to the type constructor (read: functions). `length` written in terms of Foldable has nothing to do with the contents of the Foldable structure; it has to do with the _structure_ itself. You wouldn't expect length of a list of lists to measure the length of one or more of the sublists:

```
Prelude> length [[], []]
2
```

You would expect it to measure how many cons cells were in the outermost list. Unless you lifted it. If you lifted it, then you could get the measure of all the list values contained within! (this is why we have `fmap`)

```
Prelude> fmap length [[], ["lol"]]
[0,1]

Prelude> (fmap . fmap) length [[], ["lol"]]
[[],[3]]

-- Doesn't change for Maybe
Prelude> length (Just "blah")
1
Prelude> fmap length (Just "blah")
Just 4
Prelude> fmap length (Nothing :: Maybe String)
Nothing
```

Similarly, no matter what food you move around on your plate, `length` for `(,)` is never going to do anything but return 1 because there's always one value of the type you're folding over with `(,)`. Even if you add type lambdas.

```
Prelude> length ("blah", "")
1
-- unless we lift it over the tuple structure.
Prelude> fmap length ("blah", "")
("blah",0)
Prelude> fmap length ("blah", "Papuchon")
("blah",8)
```

Want to map over the left-hand side? Use `Bifunctor`:

```
Prelude> import Data.Bifunctor
Prelude> :t first
first :: Bifunctor p => (a -> b) -> p a c -> p b c
Prelude> :t second
second :: Bifunctor p => (b -> c) -> p a b -> p a c

Prelude> first length ("blah", "Papuchon")
(4,"Papuchon")
Prelude> second length ("blah", "Papuchon")
("blah",8)
```

Or lenses! Whatever you like!

The Functor and Foldable for Either and (,) can only ever do one useful thing. We may as well make it so we know exactly which type is being mapped over by looking at the type. What Functor and Foldable do, how they work, is essentially what the combination of higher kinded types and typeclasses into constructor classes _is for_. This is their purpose for existing. If you want to address more structure than what Functor/Foldable let you talk about, then use Bifunctor or Bifoldable. If you want to choose arbitrary targets, then use lenses and prisms. There's no reason to break the consistent and predictable semantics of the language because the (necessary by construction!) Functor instance for `Either` or `(,)` appears arbitrary to you. In fact, they're the complete opposite of arbitrary or contingent because their instances follow directly from how the datatypes are defined. This uniqueness and necessity is why we can have the `DeriveFunctor` and `DeriveFoldable` extensions which will generate Functor and Foldable instances knowing only the definition of a datatype.

## Addendum

It doesn't matter if the definition of Either was:

```haskell
data Either a b = Left b | Right a
```

It matters that a default exists and is chosen for the Functor because that's the only reason to make something Left or Right. Contrary to developer intuitions, Right doesn't mean "success". The data constructors of `Either` are defined by what the Functor/Applicative/etc. instances do.

I've used `Left` to indicate "success" in situations where I want to stop fmap'ing a computation that might fail. It is the picking-of-a-winner that Haskell's semantics induce that is valuable and not arbitrary. What is arbitrary is what we call left and right and the syntactic position of their type arguments in the type constructor. There's much less utility in an Either that doesn't have a Functor with a default target.

Further, they aren't arbitrary. Following from the definition of arbitrary that Google provided:

> based on random choice or personal whim, rather than any reason or system.

We can break it down as follows:

1. Is there a reason the Either Functor works the way it does? Yes, it makes the datatype more useful in that it gives us a biased-choice Functor which is frequently useful regardless of whether the biased-target represents success or not. The way Functor behaves is useful insofar as its only reason for existing is to pick one of the two exclusive choices. There is no reason for programmers to favor the target being Left or Right. Those words mean nothing and word/name-fetishism kills software reuse and modularity.

2. Is there a systematic cause for why the `Either` Functor works the way it does? Yes, cf. Jones' work on Gofer dating to 1993/1994. The way the Functor behaves is necessary and follows from how the language works in a natural way. You can make a learner predict what the `Either` Functor does if you teach them how HKTs and constructor classes work. I've done this with learners before. This isn't surprising if you know Haskell.

### Summary

It does not matter whether one of your types is going to be in the Left or Right data constructor, all that matters is what you want your Functor-target to be. Not having a universal winner for Left or Right being the Functor target is bizarre and counter-productive. You can not and will not ever have a single Functor that lets you pick either/or of Left or Right because `a != b`. If that's what you want, what you want is Bifunctor or a prism.

We've covered both ways in which the Functor instance is not arbitrary, due to being both necessary and useful. We can also see that the way the Either Functor works is neither random nor based on whim.
