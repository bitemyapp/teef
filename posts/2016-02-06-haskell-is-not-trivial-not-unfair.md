---
title: Haskell is not trivial, but it's not unfair like Dark Souls either
---

Alternate title: Another brutal fisking for which I'll forever be remembered as a monster

### Don't be a dick and don't harass the original author please. They were just sharing their experience.

It [took me five years to stop struggling with Haskell](https://www.youtube.com/watch?v=Bg9ccYzMbxc), I know a lot of the UX stuff sucks. I think there's a lot of value in using Haskell for everyday work so it's disappointing when things fall apart for a learner for unnecessary reasons.

I've since talked to the author of [the original post](http://deliberate-software.com/haskell-is-the-dark-souls-of-programming/) and they're cool with this post. Also they agree with me that [wreq](http://www.serpentine.com/wreq/tutorial.html) should get more airplay. On with the show.

## Getting the ball rolling on talking to HTTP APIs

>I want to collect some statistics from the GitHub API. Watch as I retrace my steps attempting the Tomb of the Dread HTTPS GET Request.

Okay, is there a reason we're not going to use the Haskell [Github](https://github.com/phadej/github) client for that? Anyway, we'll follow along with what the author's doing for now.

>Now I need to query the GitHub API. Not my first time to the rodeo, I generate a personal access token from GitHub and copy it to a local file. What query should I run first? How about the count for all ASM tetris repositories? Poking around the docs comes up with:

```
GET https://api.github.com/search/repositories?q=tetris+language:assembly&sort=stars&order=desc
User-Agent: victim
Authorization: token PUT_TOKEN_HERE
```

Cool so far. Think the author figured out Github's docs more easily than I did.

>Easy life. Now how do you GET a resource in Haskell? Ah, Network.HTTP! I copy the front page sample into src/Lib.hs

Okay first mistake. I know it sucks, but you want to be careful about using Hackage to find libraries for things unless you're good at sniff-testing APIs. It's generally better to ask what a good library to use is. The library named "HTTP" is a bit creaky and there are nicer, more up to date ways of doing HTTP in Haskell. Entirely not the author's fault, but it's pretty hard to get Hackage to do anything useful anyway. I know this is sucky implicit crap nobody should have to care about, but Haskell just [doesn't](http://hackage.haskell.org/) have the [culture](https://xingframework.com/home) of design-focused self-promotion that [other communities](https://www.totaljs.com/) have. Not a value judgment, in the end it's probably better for end-users if they can use design as a signal of how up to date or nice a library is, but that's just how it is right now. It would probably help if there were more Haskellers that didn't sneer at web devs.

Anyhoodle,

```haskell
-- the article's version
module Lib
    ( someFunc
    ) where

x = simpleHTTP (getRequest "https://www.github.com/") >>= fmap (take 100) . getResponseBody

someFunc :: IO ()
someFunc =
   print x
```

Well. Yes that sucks. It's also a weird way to write it. It's like the code people write the first time they figure out how do syntax desugars into `>>=`, then they just start using `>>=` and point-free all over the place for the fuck of it. We'll re-do it in wreq:

```haskell
-- first.hs
-- this is my version

module FirstExample
  ( someFunc )
  where

-- Don't give me any shit about lens.
-- You don't need to understand lens
-- to know the ^. is for accessing a
-- record field. The wreq tutorial
-- lays out the common use-cases.

import Control.Lens
import qualified Data.ByteString.Lazy as BL
import Network.Wreq

-- Brand X
-- simpleHTTP (getRequest "https://www.github.com/") >>= fmap (take 100) . getResponseBody
-- someFunc :: IO ()
-- someFunc =
--    print x

-- our version
someFunc :: IO ()
someFunc = do
  response <- get "https://www.github.com/"
  print $ BL.take 100 (response ^. responseBody)
```

To load this beast up:

```
-- yes this'll take a moment, but then you won't
-- have to do it again because it's Stack.
$ stack build lens wreq
$ stack ghci
Prelude> :l first.hs 
[1 of 1] Compiling FirstExample     ( first.hs, interpreted )
Ok, modules loaded: FirstExample.
Prelude> someFunc
"<!DOCTYPE html>\n<html lang=\"en\" class=\"\">\n  <head prefix=\"og: http://ogp.me/ns# fb: http://ogp.me/ns"
```

Right-o, moving along.

>Doesn’t compile. Durp, hackage is a package library, I need to add this to my cabal.

If you want to and you wanted a package for this, sure. I'll typically use a [stack template](https://github.com/commercialhaskell/stack-templates) for new projects, but for initial exploration I'll build the libraries as above I want and use them in `stack ghci`.

>...author struggles with HTTP only supporting HTTP...

[wreq](http://www.serpentine.com/wreq/tutorial.html) and [http-client-tls](https://hackage.haskell.org/package/http-client-tls) support HTTPS out of the box. YMMV. There's a reason I don't really recommend older Haskell libraries even if they're maintained. The foundation of them is http-client and it's a pretty popular library to use. It's used in [http-conduit](https://hackage.haskell.org/package/http-conduit) and [pipes-http](https://hackage.haskell.org/package/pipes-http) as well. The latter of which is a single 130 line module that has required almost zero maintenance in the past two years to add pipes streaming support to http-client.

>Author moves on to use http-conduit, which uses http-client-tls under the hood

```haskell
query :: IO String
query = do
    initReq <- parseUrl "https://api.github.com/search/repositories"
    let r = initReq
                   { method = "GET"
                    , requestHeaders = [(hUserAgent, "victim")
                                      , (hAuthorization, "token PUT_TOKEN_HERE")]}
    let request = setQueryString [("q", Just "tetris+language:assembly")
                                 ,("order", Just "desc")
                                 ,("sort", Just "stars")] r
    manager <- newManager tlsManagerSettings
    res <- httpLbs request manager
    return . show . responseBody $ res

someFunc :: IO ()
someFunc = do
  query >>= putStrLn
```

They're not using the streaming, might as well use `wreq`. Normally you'd have, such as in a web framework, an HTTP client pool which gets initialized with the web app once and then shared with the handlers. So the initial setup code would happen once. I do think the API staging out the parseUrl part is a bit pointless for the common-case but w/e. For the record, I wouldn't consider this code to be bad.

As it happens, wreq has an example of how to talk to Github's API _in the tutorial_ [here](http://www.serpentine.com/wreq/) if you ctrl-f "most popular implementations of Tetris" you'll find it.

```haskell
Prelude> let opts = defaults & param "q" .~ ["tetris"]
                             & param "language" .~ ["haskell"]
Prelude> r <- getWith opts "https://api.github.com/search/repositories"
```

As it happens, we can just skip past the explicit params thing and just do this:

```
Prelude> response <- get "https://api.github.com/search/repositories?q=tetris+language:assembly&sort=stars&order=desc"
Prelude> response ^. responseBody
```

But uh, we'll get back to what they're trying to do.

>Time to parse this mega JSON string. Aeson seems to be the biggest contender. To use Aeson and get the total_count value from the return, I needed the following additions:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson

data ResultCount = ResultCount {
  total_count :: Int }
  deriving (Generic, Show)

instance ToJSON ResultCount
instance FromJSON ResultCount
```

Huh? No you don't! `Int` already has a FromJSON instance.

Just to make the point, I'll do it in GHCi again with no module.

```
$ stack build aeson lens-aeson
$ stack ghci
Prelude> import Network.Wreq
Prelude> import Control.Lens
Prelude> import Data.Aeson
Prelude> import Data.Aeson.Lens
Prelude> :set -XOverloadedStrings
Prelude> response <- get "https://api.github.com/search/repositories?q=tetris+language:assembly&sort=stars&order=desc"

Prelude> response ^? responseBody . key "total_count"
Just (Number 354.0)
Prelude> response ^? responseBody . key "total_count" . _Number
Just 354.0
```

Don't make it harder than it has to be. _Ask for help!_
