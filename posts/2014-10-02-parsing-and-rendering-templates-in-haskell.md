---
title: Parsing and rendering templates in Clojure &amp; Haskell
---

I rewrote somewhat difficult to understand templating code that was originally in Clojure into much simpler Haskell and yielded a large performance benefit for my trouble.

<!--more-->

[yogthos'](https://github.com/yogthos) code for a parser I was to replicate in Haskell:

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
    (let [content (->>  (.split (.toString buf ) " ")
                        (remove empty?) (map (memfn trim)))]
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
{-# LANGUAGE OverloadedStrings #-}

module Text.Parser.Selmer where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

newtype Var = Var Text deriving Show

data Node = VarNode Var | TextNode Text deriving Show
type Context = (Map Text Text)

parseDoubleCurly :: Parser a -> Parser a
parseDoubleCurly p = string "{{" *> p <* string "}}"

-- parseVar :: Parser Var
-- parseVar = do
--   string "{{"
--   name <- takeWhile1 isAlpha
--   string "}}"
--   return $ Var name

parseVar :: Parser Var
parseVar = parseDoubleCurly $ (Var <$> takeWhile1 isAlpha)

parseNode :: Parser Node
parseNode = TextNode <$> takeWhile1 (/= '{')

parseStream :: Parser [Node]
parseStream = many $ (parseNode <|> (VarNode <$> parseVar))

renderNode :: Context -> Node -> Text
renderNode ctx (VarNode (Var name)) = fromMaybe "" (M.lookup name ctx)
renderNode ctx (TextNode txt) = txt

render :: Context -> [Node] -> Text
render context nodes = foldr
       (\node extant ->
         mappend (renderNode context node) extant)
         "" nodes

main = do
  let context  = M.fromList [("blah", "1")]
  let parser   = parseOnly parseStream
  let template = "{{blah}} woot"
  let maybeRendered = (render context <$> (parser template))
  putStrLn (show maybeRendered)

```

Success:

<img src="/images/haskellparsesuccess.png"/>

If I get a working version of the Clojure parser, I'll do a performance comparison.
