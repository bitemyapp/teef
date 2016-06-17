---
title: Using drip for Clojure scripts (non-Leiningen)
---

Using drip to run Clojure code like a script.

<!--more-->

``` bash
#^:shebang '[
exec drip -cp "$HOME/bin/clojure.jar" clojure.main -i $0
]
```

``` clojure
(println "Hello, world!")
```

That's it. Use lein-exec (2.0 compatible) to write scripts that can use dependencies. Also this is pretty fast since there's no bootstrap stage.
