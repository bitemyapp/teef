---
title: Parsing and rendering templates in Haskell
---

yogthos' broken code

```clojure
(ns parser)

(defn read-char [rdr]
  (let [ch (.read rdr)]
    (if-not (== -1 ch) (char ch))))

(def expr-tags
  {:if #(str "if tag args: " (clojure.string/join ", " %1))})

(defn expr-tag [{:keys [tag-name args] :as tag} rdr]
  (if-let [handler (get expr-tags tag-name)]
    (handler args)
    (throw (Exception. (str "unrecognized tag: " tag-name)))))

(defn filter-tag [{:keys [tag-value]}]
  (str "filter tag value: " tag-value))

(defn read-tag-info [rdr]
  (let [buf (StringBuilder.)
        tag-type (if (= \{ (read-char rdr)) :filter :expr)]
    (loop [ch1 (read-char rdr)
           ch2 (read-char rdr)]
      (when-not (and (or (= \} ch1) (= \% ch1))
                     (= \} ch2))
        (.append buf ch1)
        (recur ch2 (read-char rdr))))
    (let [content (->>  (.split (.toString buf ) " ") (remove empty?) (map (memfn trim)))]
      (merge {:tag-type tag-type}
             (if (= :filter tag-type)
               {:tag-value (first content)}
               {:tag-name (keyword (first content))
                :args (rest content)})))))

(defn parse-tag [{:keys [tag-type] :as tag} rdr]
  (if (= :filter tag-type)
    (filter-tag tag)
    (expr-tag tag rdr)))

(defn handle-tag [rdr]
  (let [tag (read-tag-info rdr)]
    (parse-tag tag rdr)))

(defn parse [file]
  (with-open [rdr (clojure.java.io/reader file)]
      (let [template (transient [])
            buf      (StringBuilder.)]
        (loop [ch (read-char rdr)]
          (when ch
            (if (= \{ ch)
              (do
                (conj! template (.toString buf))
                (.setLength buf 0)
                (conj! template (handle-tag rdr))
                (recur (read-char rdr)))
              (do
                (.append buf ch)
                (recur (read-char rdr))))))
        (conj! template (.toString buf))
        (persistent! template))))

(defn render [template args]
  (let [buf (StringBuilder.)]
    (doseq [element template]      
      (when element
        (.append buf (if (string? element) element (element args)))))
    (.toString buf)))

(defn render-file [filename args]
  (render (parse filename) args))
```

Note this Clojure code doesn't actually work:

<img src="/images/clojureparsebroken.png"/>
<br>
<img src="/images/clojureparsebroken2.png"/>
<br>
<img src="/images/clojureparsebroken3.png"/>
<br>

The Haskell code below works. I included an alternate version of one of the parsers.

```haskell
module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (parseURI)
import qualified System.Random as SR
import Web.Scotty

alphaNum = ['A'..'Z'] ++ ['0'..'9']
randomElement l = SR.randomRIO (0, ((length l) - 1)) >>= \d -> return (l !! d)

shortyGen = replicateM 7 (randomElement alphaNum)

saveURI conn shortURI uri = R.runRedis conn $ R.set shortURI uri
getURI  conn shortURI     = R.runRedis conn $ R.get shortURI

main = scotty 3000 $ do
  rConn <- liftIO (R.connect R.defaultConnectInfo)
  get "/" $ do
    uri <- param "uri"
    case parseURI (TL.unpack uri) of
      Just _  -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
        resp <- liftIO (saveURI rConn shorty (encodeUtf8 (TL.toStrict uri)))
        text $ TL.concat [(TL.pack (show resp)), " shorty is: ", TL.pack shawty]
      Nothing -> text (TL.concat [uri, " wasn't a url"])
  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI rConn short)
    case uri of
      Left reply -> text (TL.pack (show reply))
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html $ TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]
          where tbs = TL.fromStrict (decodeUtf8 bs)
```

Success:

<img src="/images/haskellparsesuccess.png"/>
