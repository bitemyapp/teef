---
title: Alternatives to Typed Holes for talking to your compiler
tags: haskell, education
---

Rejected title: Type Praxis

I frequently see people recommend that others use typed holes. I think people are more apt to recommend typed holes than the alternatives because it's a bespoke feature intended to enable discovering the type of a sub-expression more easily. Which is fair enough, except it doesn't really have a good use-case! I will demonstrate in this post why.

<!--more-->

I frequently find myself relying on GHC Haskell's features in order to off-load brain effort. The idea behind typed holes is that if you have an incomplete expression and aren't sure what type the remaining part should be, you can ask the compiler! Lets reuse the example from the Haskell Wiki: https://wiki.haskell.org/GHC/Typed_holes

```haskell
pleaseShow :: Show a => Bool -> a -> Maybe String
pleaseShow False _ = Nothing
pleaseShow True a = Just (show _a)
```

The idea here is that we aren't sure what should go at the end of the final line and we're using `_a` to ask GHC what the type of `_a` should be. You get a type error that tries to describe the typed hole as follows:

```
    • Found hole: _a :: a0
      Where: ‘a0’ is an ambiguous type variable
      Or perhaps ‘_a’ is mis-spelled, or not in scope
    • In the first argument of ‘show’, namely ‘_a’
      In the first argument of ‘Just’, namely ‘(show _a)’
      In the expression: Just (show _a)
    • Relevant bindings include
        a :: a
        pleaseShow :: Bool -> a -> Maybe String
```

Okay so here's the problem. There's a `Show` constraint on `a` but the typed hole message doesn't bother saying so:

```
    • Found hole: _a :: a0
```

This represents sort of a problem. Typeclass constraints aren't always as syntactically obvious as they are from the declaration of `pleaseShow` here:

```
pleaseShow :: Show a => Bool -> a -> Maybe String
```

Sometimes they arise from other sub-expressions in your code and aren't manifest in the type of your declaration!

You can't productively point new people to typed holes because they'll get extremely confused about type variables that have no constraints. If they're reading good learning material, they'll know that means they can't actually do anything with something that is parametrically polymorphic. Even that framing aside, they just won't know what terms are available to them for anything polymorphic.

Then we come to the expert. The expert is more likely to be working with code leveraging typeclasses and polymorphism and therefore...typed holes is of less help to them. If they're aware of what typeclass constraints are attached to a type variable, fine, but the compiler is still forcing the programmer to juggle more context in their head than is really necessary.

## In which I offer a better alternative

```
pleaseShow :: Show a => Bool -> a -> Maybe String
pleaseShow False _ = Nothing
pleaseShow True a =
  let x :: z
      x = a
  in Just (show undefined)
```

This time we get an error that mentions where the original type came from along with the relevant typeclass constraints:

```
    • Couldn't match expected type ‘z’ with actual type ‘a’
      ‘a’ is a rigid type variable bound by
        the type signature for:
          pleaseShow :: forall a. Show a => Bool -> a -> Maybe String
      ‘z’ is a rigid type variable bound by
        the type signature for:
          x :: forall z. z
```

Keep in mind, this isn't perfect! It's not strictly the same as typed holes either as it's more about contradicting the compiler about what type `a` had in order to discover what its type is. However, at least this way, we get a more complete picture of what the type of `a` is. Also note how I used undefined in order to ignore the parts of my code I wasn't interested in getting errors about. This isn't a perfect fit here as it results in GHC wanting to know which type it's meant to expect from `undefined`, but in more typical circumstances, it works great for positing hypotheticals without bothering to write the actual code.

We're about to do something more gnarly looking in the next section, the tl;dr is this:

### TL;DR

Use let expressions, `undefined`, impossible types and the like instead of typed holes. And don't recommend Typed Holes to new people, they're more confusing than helpful and the facilities of typed holes don't scale well to more complicated contexts anyway.

## Tackling slightly more complicated situations

<hr>

*Warning: If you haven't worked through about 2/3s of the [Haskell Book](http://haskellbook.com) or possess the equivalent practice and knowledge, you are unlikely to grok this section.*
<hr>

Sometimes you want to be able to posit something or lay down types for sub-expressions in a situation where you have a polymorphic type arising from a typeclass instance or function declaration. In those situations, knowing how to combine ScopedTypeVariables, InstanceSigs, and let expressions can be very valuable!

What if we're stumped on something like this?

```haskell
doubleBubble :: ( Applicative f1
                , Applicative f2 )
             => f1 (f2 (a -> b))
             -> f1 (f2 a)
             -> f1 (f2 b)
doubleBubble f ffa =
  undefined
```

So we try to start by assigning a type to a sub-expression:

```haskell
doubleBubble :: ( Applicative f1
                , Applicative f2 )
             => f1 (f2 (a -> b))
             -> f1 (f2 a)
             -> f1 (f2 b)
doubleBubble f ffa =
  let x :: z
      x = f
  in undefined
```

And get the following type error:

```
    • Couldn't match expected type ‘z’
                  with actual type ‘f1 (f2 (a -> b))’
```

Fair enough, what if we try to make the types agree?

```haskell
doubleBubble :: ( Applicative f1
                , Applicative f2 )
             => f1 (f2 (a -> b))
             -> f1 (f2 a)
             -> f1 (f2 b)
doubleBubble f ffa =
  let x :: f1 (f2 (a -> b))
      x = f
  in undefined
```

We get a type error?!

```
    • Couldn't match type ‘f1’ with ‘f4’
      ‘f1’ is a rigid type variable bound by
        the type signature for:
          doubleBubble :: forall (f1 :: * -> *) (f2 :: * -> *) a b.
                          (Applicative f1, Applicative f2) =>
                          f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
      ‘f4’ is a rigid type variable bound by
        the type signature for:
          x :: forall (f4 :: * -> *) (f5 :: * -> *) a1 b1. f4 (f5 (a1 -> b1))
```

The issue is that types usually only last the scope of a single type signature denoted by `::`, so the variables `f1`, `a`, `b`, and the like can only be referenced in our declaration. That kinda sucks, how do we keep referring to the same type variables under our declaration? `ScopedTypeVariables`!

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

doubleBubble :: forall f1 f2 a b
              . ( Applicative f1
                , Applicative f2 )
             => f1 (f2 (a -> b))
             -> f1 (f2 a)
             -> f1 (f2 b)
doubleBubble f ffa =
  let x :: f1 (f2 (a -> b))
      x = f
  in undefined
```

This now type-checks because we used `forall` to tell GHC that we wanted those variables to be lexically scoped! Now we're really cooking with gas. Lets follow a chain of experiments and how they change our type errors:

```haskell
doubleBubble :: forall f1 f2 a b
              . ( Applicative f1
                , Applicative f2 )
             => f1 (f2 (a -> b))
             -> f1 (f2 a)
             -> f1 (f2 b)
doubleBubble f ffa =
  let x :: z
      x = fmap (<*>) f
  in undefined
```

```
    • Couldn't match expected type ‘z’
                  with actual type ‘f1 (f2 a -> f2 b)’
```

```haskell
doubleBubble :: forall f1 f2 a b
              . ( Applicative f1
                , Applicative f2 )
             => f1 (f2 (a -> b))
             -> f1 (f2 a)
             -> f1 (f2 b)
doubleBubble f ffa =
  let x :: z
      x = (fmap (<*>) f) <*> ffa
  in undefined
```

```
    • Couldn't match expected type ‘z’ with actual type ‘f1 (f2 b)’
```

```haskell
-- this typechecks.
doubleBubble :: forall f1 f2 a b
              . ( Applicative f1
                , Applicative f2 )
             => f1 (f2 (a -> b))
             -> f1 (f2 a)
             -> f1 (f2 b) -- <---
doubleBubble f ffa =      ------ hmm
  let x :: f1 (f2 b) -- <--------
      x = (fmap (<*>) f) <*> ffa
  in undefined
```

And now we're done:

```haskell
doubleBubble :: forall f1 f2 a b
              . ( Applicative f1
                , Applicative f2 )
             => f1 (f2 (a -> b))
             -> f1 (f2 a)
             -> f1 (f2 b) -- <---
doubleBubble f ffa =      ------ hmm
  let x :: f1 (f2 b) -- <--------
      x = (fmap (<*>) f) <*> ffa
  in x
```

The intuition here is that we have to applicatively (monoidal functor, remember?) combine the `* -> *` kinded structure twice,

```haskell
   f1 (f2 (a -> b))
-- <>  <>
-> f1 (f2     a)

```

Once for `f1` of the function and `f1` of the value, once for `f2` of the function and `f2` of the value.

```
Prelude> :t (\f a -> f <*> a)
(\f a -> f <*> a) :: Applicative f => f (a -> b) -> f a -> f b
Prelude> :t (\f a -> (fmap (<*>) f) <*> a)
(\f a -> (fmap (<*>) f) <*> a)
  :: (Applicative f, Applicative f1) =>
     f1 (f (a -> b)) -> f1 (f a) -> f1 (f b)
```

The following doesn't fit because we end up triggering the `Reader` (function type) Applicative:

```
Prelude> :t (\f a -> ((<*>) f) <*> a)
(\f a -> ((<*>) f) <*> a)
  :: (a1 -> a -> b) -> ((a1 -> a) -> a1) -> (a1 -> a) -> b
```

Rewriting the working solution a little:

```haskell
apply = (<*>)
doubleAp f a = apply (fmap apply f) a
```

```
Prelude> let apply = (<*>)
Prelude> let doubleAp f a = apply (fmap apply f) a
Prelude> :t doubleAp
doubleAp
  :: (Applicative f1, Applicative f) =>
     f1 (f (a -> b)) -> f1 (f a) -> f1 (f b)
```

Then breaking down:

```haskell
doubleAp f a = apply (fmap apply f) a
--             [1]    [2]  [3]
```

1. This `apply` grafts in the pre-lifted `apply`, cf.

```
Prelude> import Data.Void
Prelude> let v :: Void; v = undefined
Prelude> let doubleAp f a = v (fmap apply f) a

<interactive>:104:20: error:
    • Couldn't match expected type ‘f1 (f a -> f b) -> t1 -> t’
                  with actual type ‘Void’
```

2. This `fmap` lifts a regular `apply` into a type that can graft together two values embedded in `f` such that the type is: `f a -> f b`, cf.

```
Prelude> let doubleAp f a = apply (v apply f) a

<interactive>:105:27: error:
    • Couldn't match expected type ‘(f0 (a0 -> b0) -> f0 a0 -> f0 b0)
                                    -> t -> f (a -> b)’
                  with actual type ‘Void’
```

3. This is the `apply` lifted by `fmap`, transformed from:

`(f0 (a0 -> b0)` into `f0 a0 -> f0 b0`

(The void error here is less useful)

Kicking in the contradiction we get for `a` if we replace it with the `Void` typed `v` variable:

```
Prelude> let doubleAp f a = apply (fmap apply f) v

<interactive>:107:41: error:
    • Couldn't match expected type ‘f1 (f a)’ with actual type ‘Void’
    • In the second argument of ‘apply’, namely ‘v’
      In the expression: apply (fmap apply f) v
```

Not bad eh? I find it's better to teach people these techniques than to point them to typed holes, but reasonable minds disagree. Even when a learner is relatively early in the learning process, these techniques can be made approachable/digestible.

That's all folks. Below is just a demonstration of the missing-constraint problem with an example from the Haskell Wiki.

## Re-demonstration of the missing constraint problem using the Haskell Wiki's example

```haskell
module FreeMonad where
 
data Free f a
  = Pure a
  | Free (f (Free f a))

-- These are just to shut the compiler up, we
-- are not concerned with these right now.
instance Functor f => Functor (Free f) where
  fmap = undefined

instance Functor f => Applicative (Free f) where
  pure = undefined
  (<*>) = undefined

-- Okay, we do care about the Monad though.
instance Functor f => Monad (Free f) where
  return a     = Pure a
  Pure a >>= f = f a
  Free f >>= g = Free _a
```

```
code/FreeMonad.hs:20:23: error:
    • Found hole: _a :: f (Free f b)
      Where: ‘f’ is a rigid type variable bound by
               the instance declaration at code/FreeMonad.hs:17:10
             ‘b’ is a rigid type variable bound by
               the type signature for:
                 (>>=) :: forall a b. Free f a -> (a -> Free f b) -> Free f b
               at code/FreeMonad.hs:19:10
      Or perhaps ‘_a’ is mis-spelled, or not in scope
    • In the first argument of ‘Free’, namely ‘_a’
      In the expression: Free _a
      In an equation for ‘>>=’: (Free f) >>= g = Free _a
    • Relevant bindings include
        g :: a -> Free f b (bound at code/FreeMonad.hs:20:14)
        f :: f (Free f a) (bound at code/FreeMonad.hs:20:8)
        (>>=) :: Free f a -> (a -> Free f b) -> Free f b
          (bound at code/FreeMonad.hs:19:3)
Failed, modules loaded: none.
```

^^ Look ma, no `Functor`.

```
_a :: f (Free f b)
```


```
instance Functor f => Monad (Free f) where
```

Not consistently, but I'm more likely to get better type errors when I create contradictions manually via let expressions than I am using typed holes.
