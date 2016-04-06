---
title: A URL shortener made literate
---

### This is a literate respin of an older post of mine

<a href="/posts/2014-08-22-url-shortener-in-haskell.html">The original code listing is here</a>

<div class="left">
## Explanation
</div>

<div class="right">
## The code
</div>

<div class="pair">

<div class="left">

First we declare our module name to be `Main` as that is required for anything exporting a `main` function to be invoked when the executable runs.

</div>

<div class="right">

```haskell
module Main where
```

</div>

</div>


<div class="pair">

<div class="left">

Next we have a series of imports. Where we import something `qualified...as`, we are doing two things: qualifying the import means that we can only refer to values in the module with the full module path. One example of that would be `Control.Monad.mapM`, that's a fully qualified reference to `mapM`. Second, we use `as` to give the values that we want in scope a name. We qualify the import so that we don't import values that would conflict with values that already exist in Prelude. By specifying a name using `as`, we can give the value a shorter, more convenient name. Where we import the module name followed by parentheses, such as with `replicateM` or `liftIO`, we are saying we only want to import the functions or values of that name and nothing else. In the case of `import Web.Scotty`, we are importing everything `Web.Scotty` exports. An unqualified and unspecific import should be avoided except in cases where it will be *very* obvious where a function came from or because it's a toolkit you must use all together like Scotty.

</div>

<div class="right">

```haskell
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.List.NonEmpty as NE
import Data.Semigroup
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (parseURI)
import qualified System.Random as SR
import Web.Scotty
```

</div>

</div>


<div class="pair">

<div class="left">

Next we need to generate our shortened URLs that will refer to the links people post to the service. `alphaNum` is a `String` of the characters we want to select from - an alphabet if you will.

</div>

<div class="right">

```haskell
alphaNum :: NE.NonEmpty Char
alphaNum = NE.fromList ['A'..'Z'] <> NE.fromList ['0'..'9']
```

</div>

</div>


<div class="pair">

<div class="left">

`randomElement` lets us get a random element from a non-empty list. It first gets the length of the list to determine what range it is selecting from, then gets a random number in that range using IO to handle the randomness. Nota bene - I could have written `randomElement` to return IO (Maybe a) instead of demanding a `NonEmpty`; however, I prefer to be more clear about my inputs if that cleans up my outputs. Being more clear about my inputs also provides the user more information than "maybe I won't give you anything" does.

</div>


<div class="right">

```haskell
randomElement :: NE.NonEmpty a -> IO a
randomElement xs = do
  let maxIndex = (NE.length xs) - 1
  randomDigit <- SR.randomRIO (0, maxIndex)
  return (xs NE.!! randomDigit)
```

</div>

</div>


<div class="pair">

<div class="left">

Here we apply `randomElement` to `alphaNum` to get a single random letter or number from our alphabet. Then we use `replicateM 7` to repeat this action 7 times, giving a list of 7 random letters or numbers. For additional fun, see what `replicateM 2 [1, 3]` does and see if you can figure out why.

</div>

<div class="right">

```haskell
shortyGen :: IO [Char]
shortyGen = replicateM 7 (randomElement alphaNum)
```

</div>

</div>


<div class="pair">

<div class="left">

With `saveURI`, we pass our connection to Redis, the key we are setting in Redis, and the value we are setting the key to as arguments. We also perform side effects in IO to get `Either R.Reply R.Status`. The key in this case is the randomly generated URI we created and the value is the URL the user wants the shortener to provide at that address. Redis is a key-value datastore which can be very convenient for some common use-cases like caching...or when you want persistence without a lot of ceremony, as was the case here.

</div>

<div class="right">

```haskell
saveURI :: R.Connection  -> BC.ByteString
        -> BC.ByteString -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri = R.runRedis conn $ R.set shortURI uri
```

</div>

</div>


<div class="pair">

<div class="left">

In the case of `getURI`, we just pass it the connection to Redis and the shortened URI key so that we can get the URI associated with that short URL and show the users where they're headed.

</div>

<div class="right">

```haskell
getURI  :: R.Connection -> BC.ByteString -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI
```

</div>

</div>


<div class="pair">

<div class="left">

`main` is a function like any other. Here it returns `IO ()` and acts as the entry point for our webserver when we start the executable. We begin by invoking `scotty 3000`, a helper function from Scotty which, given a port to run on and a Scotty application, will listen for requests and respond to them.

</div>

<div class="right">

```haskell
main :: IO ()
main = scotty 3000 $ do
```

</div>

</div>


<div class="pair">

<div class="left">

Outside of any specific route handlers, we are cheating a bit and binding the database connection to Redis to the name `rConn`. `R.defaultConnectInfo` points at an instance of Redis on my local machine, and `liftIO` lifts the IO action into the Scotty monad. `liftIO` is a typeclass method from MonadIO; monads which implement it can have IO actions lifted over them. Writing a trivial instance of this can be a good way to get a feel for how this works and why it's desirable. Briefly - it saves us a bunch of `lift . lift . lift` etc.

</div>

<div class="right">

```haskell
  rConn <- liftIO (R.connect R.defaultConnectInfo)
```
</div>

</div>


<div class="pair">

<div class="left">

`get` has type `RoutePattern -> ActionM () -> ScottyM ()`. It takes a RoutePattern to decide which request paths to dispatch on and an action to perform when a request matches that path. It (purely) modifies the enclosing `ScottyM` to register itself.

</div>

<div class="right">

```haskell
  get "/" $ do
```

</div>

</div>

<div class="pair">

<div class="left">

Here we use `param` to get the input parameter "uri". Where `param` gets its parameters depends on the HTTP method of the action. With a `get` it will check the HTTP get arguments, whereas with a `post` it will check the POST form body. In all cases, it will check for parameters in the URI itself, such as with "/my/uri/:argument".

</div>

<div class="right">

```haskell
    uri <- param "uri"
```

</div>

</div>

<div class="pair">

<div class="left">

Since we want to reject things that aren't URIs or might've been accidentally malformed, we're using `parseURI` which has type `String -> Maybe Network.URI.URI` to discriminate between bad and good URIs. We are using `TL.unpack` because param returned a lazy Text object and not the `String` parseURI expects, so we unpack it into a `String`. Then we case match on the `Just` and `Nothing` case of the `Maybe` `parseURI` returns to handle the "yep, a proper URL" and "nope, not a proper URL" cases separately.

</div>

<div class="right">

```haskell
    case parseURI (TL.unpack uri) of
```

</div>

</div>

<div class="pair">

<div class="left">

Here we match on the `Just` constructor of Maybe and throw away the contents because we only used `parseURI` to check if the URI was correct; we don't actually care to use the parsed URI object.

</div>

<div class="right">

```haskell
      Just _  -> do
```

</div>

</div>

<div class="pair">

<div class="left">

Now we call on our old friend `shortyGen` to get a randomized URI to assign a shortened URI for the input URL the user gave us. Once again, `shortyGen` returns `IO` so we must lift that over our ActionM. Previously, we were lifting over ScottyM. Both, conveniently, implement MonadIO. One of the nice things about liftIO is that we don't have to care how many monads the IO action got lifted over. This can save some maintenance and headache down the road.

</div>

<div class="right">

```haskell
        shawty <- liftIO shortyGen
```

</div>

</div>

<div class="pair">

<div class="left">

We use a `let` expression with `let shorty = BC.pack shawty` because it's not participating in the enclosing monad. It just packs the `String` into a `ByteString`.

</div>

<div class="right">

```haskell
        let shorty = BC.pack shawty
```

</div>

</div>

<div class="pair">

<div class="left">

Here we're encoding the uri the user passed us as a `ByteString` as that's what the Redis client library wants. Before it can be encoded, we have to change it from a lazy to a strict Text object. We're using the Redis connection in scope from earlier and the shortened URI ByteString to save the user's data at that key in the Redis database. We lift the IO action into the Action monad transformer and bind over the result in ActionT. The response has type (after monadic binding) `Either R.Reply R.Status`.

</div>

<div class="right">

```haskell
        resp <- liftIO (saveURI rConn shorty (encodeUtf8 (TL.toStrict uri)))
```

</div>

</div>

<div class="pair">

<div class="left">

We stringify `resp` which is `Either R.Reply R.Status`. It'll be `Right Ok ...yadda yadda` if it succeeded, `Left ...` if it failed. We concatenate the stringified response with some additional text to show the user what the shortened URI is.

</div>

<div class="right">

```haskell
        text $ TL.concat [(TL.pack (show resp)), " shorty is: ", TL.pack shawty]
```

</div>

</div>

<div class="pair">

<div class="left">

Now we need to handle the `Nothing` case from earlier. The `Nothing` case happens if `parseURI` was given something that wasn't a correct URL. We just return a text response saying the uri provided wasn't a url.

</div>

<div class="right">

```haskell
      Nothing -> text (TL.concat [uri, " wasn't a url"])
```

</div>

</div>

<div class="pair">

<div class="left">

This is another http get request handler, but this time with a parameterized URL component which has the name `short`. This is so a request against `/EFG3YLB` will parse `EFG3YLB` as the param `short`.

</div>

<div class="right">

```haskell
  get "/:short" $ do
```

</div>

</div>


<div class="pair">

<div class="left">

As before, `param` does a bit of magic for us. `param` checks multiple sources of arguments for us. Where previously we were getting a parameter from the HTTP GET arguments, this time it's a component of the request path.

</div>

<div class="right">

```haskell
    short <- param "short"
```

</div>

</div>


<div class="pair">

<div class="left">

Here we lift an IO action into ActionT. This time we're taking the shortened URI
path that we got and asking Redis if it has a matching value for us to give back to
the user at that key. We bind over the result, giving us `uri` which has type
`Either R.Reply (Maybe BC.ByteString)`.

</div>

<div class="right">

```haskell
    uri <- liftIO (getURI rConn short)
```

</div>

</div>

<div class="pair">

<div class="left">

Next we case match on `uri` over the `Left` and `Right` constructors of the Either
sum type. If we get `Left`...well, we're done. We show the user what Redis had to say
about it and give up. If we got `Nothing` that means Redis didn't have
a value saved at the key we queried - nothing stored at that location. In that case,
we return the text to the user "uri not found".

</div>

<div class="right">

```haskell
    case uri of
      Left reply -> text (TL.pack (show reply))
```

</div>

</div>

<div class="pair">

<div class="left">

If we got `Right`, we're going to proceed.

We proceed because the `Right` value means the Either value was a "success" idiomatically speaking. Here `mbBS` is the `Maybe BC.ByteString` that was inside of `Either R.Reply (Maybe BC.ByteString)`.

</div>

<div class="right">

```haskell
      Right mbBS -> case mbBS of
```

</div>

</div>

<div class="pair">

<div class="left">

If we got `Just`, we take the `ByteString` Redis returned and pack it into an HTML
anchor tag so it is a clickable link for the user to follow. If that happened, the
request was a success and we're done. We use the `html` instead of `text` Scotty
function to indicate the content type we're returning is HTML and not plain text.

</div>

<div class="right">

```haskell
        Nothing -> text "uri not found"
        Just bs -> html $ TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]
          where tbs = TL.fromStrict (decodeUtf8 bs)
```

</div>

</div>
