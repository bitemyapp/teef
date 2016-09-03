---
title: The Hashrocket websocket shootout in Haskell
tags: practical, haskell, websockets
---

I [recently PR'd](https://github.com/hashrocket/websocket-shootout/pull/14) a Haskell entry to Hashrocket's [websocket shootout](https://hashrocket.com/blog/posts/websocket-shootout). Haskell seemed to do a lot better than C++, Rust, Golang, Elixir, Erlang, NodeJS, Ruby MRI, and JRuby.

<!--more-->

The constraints of the benchmark are that your 95th percentile round-trip time cannot exceed 250ms. This is a better measurement/restriction for concurrency benchmarks than the usual "how many can it handle before it crashes" or throughput metrics, so props to Hashrocket on that point.

I didn't do anything special, this was the first pass version. First, the results:

- Rust: ![Rust benchmark results](/images/rust-ws-results.png)

- Golang: ![Golang benchmark results](/images/golang-ws-results.png)

- C++: ![C++ benchmark results](/images/cpp-ws-results.png)

- And now Haskell:

![Haskell benchmark results](/images/haskell-ws-results.png)

I would like to particularly note how _flat_ and consistent the RTT times for Haskell were. Compare to Rust's 10-244ms stretch at the max connection count. For memory usage, Haskell's memory usage was more average at ~1.2gb, but still quite good.

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

If I haven't misunderstood or misbenchmarked something, it's going to be extremely hard for any language runtime to beat GHC Haskell on an I/O heavy benchmark like this.

- [A reddit conversation on the tradeoffs of Rust's stack model vs. GHC's](https://www.reddit.com/r/haskell/comments/1wm9n4/question_about_stacks_in_haskell_and_rust/)

Erlang is the only runtime competitive on per-thread (process in their lingo) overhead, but they bite the dust on message send. MVar take/put pairing is ~25-40ns, you're eating at least 1,000 ns in Erlang. It's possible a custom Erlang implementation (Cowboy?) could do a better job here, but I'm not sure how to do broadcast especially efficiently in Erlang.

Asking how to efficiently broadcast to many Erlang processes on the mailing list [gets you smarmy answers](http://erlang.org/pipermail/erlang-questions/2011-May/058307.html).

Mostly I'm just disappointed I didn't get an excuse to optimize any of the Haskell code. It was limited only by the number of TCP connections I could bind, I had 2/3s of my 95th percentile RTT to burn yet. I messed with ulimit and the like a bit, but to really uncap it I'd need to change the client to connect to multiple IP addresses so I can use more TCP connections :)
