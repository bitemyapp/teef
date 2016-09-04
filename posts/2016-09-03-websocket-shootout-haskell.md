---
title: The Hashrocket websocket shootout in Haskell
tags: practical, haskell, websockets
---

I [recently PR'd](https://github.com/hashrocket/websocket-shootout/pull/14) a Haskell entry to Hashrocket's [websocket shootout](https://hashrocket.com/blog/posts/websocket-shootout). <strike>Haskell seemed to do a lot better than C++, Rust, Golang, Elixir, Erlang, NodeJS, Ruby MRI, and JRuby.</strike> Although the Haskell version has been since fixed, so I can no longer run the benchmark reliably on my machine, so any final results will have to come from Hashrocket running the unagi-chan variant.

<!--more-->

## How the benchmark works

The idea is to test how many concurrent clients a single websocket server (process?) can serve and how efficiently it can broadcast messages to all the clients.

The constraints of the benchmark are that your 95th percentile round-trip time cannot exceed 250ms. This is a better measurement/restriction for concurrency benchmarks than the usual "how many can it handle before it crashes" or throughput metrics, so props to Hashrocket on that point.

The client as-designed will increase the number of clients connected in the step-size specified and send test events at each step. If the 95th percentile round trip time exceeds 250ms, the benchmark client disconnects all client connections and halts. So, the last "line" of output you see from the client is essentially where you peaked before failing the SLA constraint.

<!-- ## Working version (no dropped messages) with unagi-chan -->

<!-- This was contributed by [Sebastian Graf](https://github.com/sgraf812), thank you Sebastian! This also means this version _should_ have similar performance to [this test](https://blog.wearewizards.io/a-lot-of-websockets-in-haskell). -->

<!-- ```haskell -->

<!-- ``` -->


## What follows is the flawed Broadcast implementation I wrote that drops messages, so caveat lector

Everything below is retracted for now as [Broadcast](http://hackage.haskell.org/package/concurrent-extra) was dropping messages, which wasn't explicitly permitted in the benchmark. I'm currently kicking around an [unagi-chan](http://hackage.haskell.org/package/unagi-chan) based variant [PR'd by Sebastian Graf](https://github.com/bitemyapp/websocket-shootout/pull/3), but I don't think `unagi-chan` was designed for broadcasting across many thousands of channels.

For some context, [this benchmark using Tsung](https://blog.wearewizards.io/a-lot-of-websockets-in-haskell) is roughly what I expected in terms of results modulo hardware differences, which is why I wasn't that surprised when I saw the initial results. Currently the Go websocket client seems to behave very differently from Tsung's, so I don't have a reproduction of what Tom Hunger's benchmark did.

Before I retracted the results, I was peaking at 45,000 concurrent clients and a very low/flat latency with this version that uses `Broadcast`. However, `Broadcast` was dropping messages, so it's not a valid comparison when the other benchmark servers weren't dropping any messages. Incidentally, load-shedding is a great strategy for consistent server performance when it's permissible ;)

Here's the source to the Haskell version at time of writing:

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Broadcast as BC
import Control.Lens hiding ((.=))
import Control.Monad (forever)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.Char as DC
import Data.Functor (void)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import Network.HTTP.Types (status400)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Text.RawString.QQ
```

The above is just the usual preamble/noise. I had quasiquotes for a test/example I didn't use in the actual server.

```haskell
type Broadcaster = BC.Broadcast ByteString
```

Hedging my bets in case I switched again after changing the broadcast type from `Text` to a lazy `ByteString`.

```haskell
amendTest :: Maybe Value
amendTest = decode $ [r|
{"type":"broadcast","payload":{"foo": "bar"}}
|]

amendBroadcast :: Value -> Value
amendBroadcast v =
  v & key "type" . _String .~ "broadcastResult"
```

Above was just test code.

```haskell
broadcastThread :: Broadcaster -> Connection -> IO ()
broadcastThread bc conn = forever $ do
  t <- BC.listen bc
  sendTextData conn t
```

That's all I do to relay broadcasted data to the listeners. Under the hood, `Broadcast` is:

```haskell
MVar (Either [MVar a] a)
```

I used `broadcast` from `concurrent-extra` because I knew I wanted the propagation/thread wake to happen via the MVar machinery in the GHC runtime system.

```haskell
wtf conn =
  sendTextData conn ("<img src=\"http://bit.ly/1kmRC7Q\" />" :: Text)
```

Error return method borrowed from [ocharles](https://ocharles.org.uk/blog/posts/2013-12-19-websockets.html).

```haskell
mkPayload :: Text -> Value -> ByteString
mkPayload type_ payload = encode $
  object [ "type" .= String type_
         , "payload" .= payload
         ]
```

Constructing a JSON value fitting the format expected by the test client and then `encode`-ing it into a `ByteString`.

```haskell
bidiHandler :: Broadcaster -> Connection -> IO ()
bidiHandler bc conn = do
  _ <- C.forkIO (broadcastThread bc conn)
  --   [               1                ]
  forever $ do
  -- [2]
    msg <- receiveDataMessage conn
    --     [3]
    case msg of
      Text t -> do
        let Just payload = t ^? key "payload"
        --                 [       4       ]
        case t ^? key "type" . _String of
        --   [           5           ]
          Just "echo" -> sendTextData conn (mkPayload "echo" payload)
          --             [                   6                      ]
          Just "broadcast" -> BC.signal bc (mkPayload "broadcastResult" payload)
          --                  [                       7                        ]
          _ -> wtf conn
      _ -> do
        wtf conn
```

I hate reading overly chopped-up code, so I annotated this one in the mode of the [haskell book](http://haskellbook.com).

1. We run the broadcast listener that relays data to the websocket client in a separate thread

2. Running the client listener that (potentially) broadcasts data or just echoes back to the client in a `Control.Monad.forever` block.

3. Block on receiving a data message (sum type, Text or Bytes)

4. Pluck the payload value out of the JSON body because I'm too lazy to make a datatype for this.

5. Get the event type out of the JSON body to dispatch on. We're going to either echo or broadcast.

6. If the event type was echo, kick the JSON data back to the client, but with the event type amended to `echo`.

7. If the event type was broadcast, signal the broadcast handle to propagate the new JSON body with the payload and a `broadcastResult` event type.

```haskell
wsApp :: Broadcaster -> ServerApp
wsApp bc pending = do
  conn <- acceptRequest pending
  bidiHandler bc conn
```

Passing on the `Broadcast` handle and `Connection` to the handler.

```haskell
main :: IO ()
main = do
  bc <- BC.new
  runServer "127.0.0.1" 3000 (wsApp bc)
```

Spawn a `Broadcast`, pass the handle on to `wsApp`, run it with the provided server from the `wai-websockets` library. That's it.

### Some thoughts

- [A reddit conversation on the tradeoffs of Rust's stack model vs. GHC's](https://www.reddit.com/r/haskell/comments/1wm9n4/question_about_stacks_in_haskell_and_rust/)

Erlang is the only runtime competitive on per-thread (process in their lingo) overhead, but they bite the dust on message send. MVar take/put pairing is ~25-40ns, you're eating at least 1,000 ns in Erlang. It's possible a custom Erlang implementation (Cowboy?) could do a better job here, but I'm not sure how to do broadcast especially efficiently in Erlang.

Asking how to efficiently broadcast to many Erlang processes on the mailing list [gets you smarmy answers](http://erlang.org/pipermail/erlang-questions/2011-May/058307.html).

I was initially disappointed I didn't get an excuse to optimize any of the Haskell code. It was limited only by the number of TCP connections I could bind, I had 2/3s of my 95th percentile RTT to burn yet. I messed with ulimit and the like a bit, but to really uncap it I'd need to change the client to connect to multiple IP addresses so I can use more TCP connections. Now I know it was because `Broadcast` was dropping messages and not tickling the slow parts as much as an implementation that forces broadcasts to all clients.
