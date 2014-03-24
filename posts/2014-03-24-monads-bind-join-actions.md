---
title: Monads, lifting, join, and side-effecting actions.
---

While playing around with querying [ElasticSearch](http://www.elasticsearch.org/) I bumped into something that I hadn't really understood explicitly before about monads, nesting, and IO. Rather than blather on, I'm going to share a "literate" ghci session that demonstrates the point. Main editing change I made was to remove duplication in the output from querying the type `:t` in ghci.

``` haskell
import Network.HTTP

let url = "http://localhost:9200/events/event/_search?q=port:3000&size=1"
```

Importing the HTTP client library, a simple one based on IO. Also binding the query I want to perform, "find all documents with the field port equal to 3000, return 1 document".

``` haskell
simpleHTTP (getRequest url) >>= (liftM putStrLn) . getResponseBody
```

But that doesn't print anything. It type-checks too! Lets break it down.

``` haskell
λ> :t (simpleHTTP (getRequest url))
  :: IO (Network.Stream.Result (Network.HTTP.Response String))

λ> :t (simpleHTTP (getRequest url) >>= getResponseBody)
  :: IO String
```

Okay, so we've got a String inside the IO monad.

For our purposes, `fmap` and `liftM` mean the same thing. Lifting a function to work on data inside the IO monad.

``` haskell
λ> fmap head (simpleHTTP (getRequest url) >>= getResponseBody)
'{'
λ> liftM head (simpleHTTP (getRequest url) >>= getResponseBody)
'{'
λ> :t (liftM head (simpleHTTP (getRequest url) >>= getResponseBody))
  :: IO Char
```

So far, everything about how we expect this stuff lines up with reality, except for that one case where the body of the response doesn't print when we lift putStrLn to operate on the IO String returned by getResponseBody.

On a hunch, I broke out putStrLn into a separate bind (`>>=`) call, allowing me to eliminate the lifting (`liftM`).

``` haskell
λ> simpleHTTP (getRequest url) >>= getResponseBody >>= putStrLn
{"took":11,"timed_out":false ...
```

Then on another hunch:

``` haskell
λ> simpleHTTP (getRequest url) >>= (liftM putStrLn) . getResponseBody >>= id
{"took":9,"timed_out":false ...
```

So at this point I felt a little dumb, if you do too, it's okay.

`>>= id` is just join. As demonstrated with lists:

``` haskell
λ> [[1, 2, 3], [4, 5, 6]] >>= id
[1,2,3,4,5,6]

λ> import Control.Monad (join)
λ> join [[1, 2, 3], [4, 5, 6]]
[1,2,3,4,5,6]

λ> :t join
join :: Monad m => m (m a) -> m a
```

Note that in this case, `m` is `[]`, so `m (m a)` is `[[Int]]` in our list example.

`[[1, 2, 3], [4, 5, 6]]` is `[[Int]]`, `[1,2,3,4,5,6]` is `[Int]`

If this seems like a garden path, just bear with me, I'm tying it back to the original misunderstanding earlier.

Let us query some types in our REPL again:

``` haskell
--- this is the one that didn't work
λ> :t (simpleHTTP (getRequest url) >>= (liftM putStrLn) . getResponseBody)
  :: IO (IO ())

--- These two did
λ> :t (simpleHTTP (getRequest url) >>= getResponseBody >>= putStrLn)
  :: IO ()

λ> :t (simpleHTTP (getRequest url) >>= (liftM putStrLn) . getResponseBody >>= id)
  :: IO ()
```

Now for the grand reveal.

``` haskell
--- this works as well :)

λ> join $ simpleHTTP (getRequest url) >>= (liftM putStrLn) . getResponseBody
{"took":4,"timed_out":false ...

λ> :t (join $ simpleHTTP (getRequest url) >>= (liftM putStrLn) . getResponseBody)
  :: IO ()
```

`IO (IO ())` is usually a sign of a mistake in Haskell code and should be a build/lint failure `:)`. We don't force evaluation of the nested functions until we join.

Reminders/type cheat sheet:

``` haskell

λ> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
λ> :t (join)
(join) :: Monad m => m (m a) -> m a
λ> :t (fmap)
(fmap) :: Functor f => (a -> b) -> f a -> f b
λ> :t (liftM)
(liftM) :: Monad m => (a1 -> r) -> m a1 -> m r

--- note how this looks a lot like >>= (bind) ?

λ> :t (join .) . fmap
(join .) . fmap
  :: (Monad m, Functor m) => (a1 -> m a) -> m a1 -> m a

--- or simply flipped around:
  
 λ> :t (flip ((join .) . fmap))
(flip ((join .) . fmap))
  :: (Monad m, Functor m) => m a1 -> (a1 -> m a) -> m a

λ> (concat [[1, 2, 3], [4, 5, 6]]) == (join [[1, 2, 3], [4, 5, 6]])
True

```

So in the case of lists, >>= is `concatMap`, or `flatMap` if you like. Beware, Monads are very general and widely applicable to things far different than nesting IO actions or flatMap'ing lists!

Don't let intuition and a vague feeling suffice, keep learning!
