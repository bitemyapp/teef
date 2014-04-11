---
title: Mutable closures in Haskell and nested IO
---

In my [last post](/posts/2014-03-24-monads-bind-join-actions.html), I described IO (IO ()) as being a sign you might've made a mistake unless you knew it was what you wanted. There are patterns which involve using a closed-over mutable reference for things like counters. This naturally leads to nested IO actions.

Note for the new Haskell users: you probably don't need this and there are more thread-safe ways to do mutable counters than IORef.

Setting things up:

``` haskell
import Control.Monad
import Data.IORef

counter :: IO (IO (), IO Int)
counter = do
  ref <- newIORef 0
  let writer = modifyIORef ref (+1)
  return (writer, readIORef ref)
```

So now we have our counter generator. The left-hand side of the tuple is the writer that bumps the mutable reference, the right-hand side is going to be the value of the reference at whatever time we accessed it.

Now to mangle it over and over a bit.

``` haskell
incAndRead :: (IO (), IO Int) -> IO Int
incAndRead cntr = do
  blah <- fst cntr
  snd cntr

main = do
  myCounter <- counter
  replicateM_ 1000 $ incAndRead myCounter >>= print
```

This successfully print 1-1000.

But there's a danger lurking deep...a dreaded space leak! Jack up n to a million something and watch ghc churn heap like a farmer baling hay.

Not a big deal though.

If we examine the [Hackage documentation for Data.IORef](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-IORef.html), we see that this very use-pattern has a warning attached. Namely:

<hr>
Mutate the contents of an IORef.

Be warned that modifyIORef does not apply the function strictly. This means if the program calls modifyIORef many times, but seldomly uses the value, thunks will pile up in memory resulting in a space leak. This is a common mistake made when using an IORef as a counter. For example, the following will likely produce a stack overflow:

To avoid this problem, use modifyIORef' instead.
<hr>

So with that in mind we make one simple change from:

``` haskell
  let writer = modifyIORef ref (+1)
```

To

``` haskell
  let writer = modifyIORef' ref (+1)
```

And we get constant space usage. Delightful.

Fun Haskell note: the `'` (prime) version of a function in base will usually be the strict version. `_` means we're throwing the result away.

So for those Lispers considering using Haskell, you can in fact reuse the side-effecty closure patterns you had before, you'll just get judged for it.
