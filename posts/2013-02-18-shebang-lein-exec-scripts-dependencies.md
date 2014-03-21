---
title: Using shebang and lein-exec to write Clojure scripts that can use dependencies
---

``` bash
#!/home/callen/bin/lein exec
```
```clojure
(use '[leiningen.exec :only (deps)])
(deps '[[clj-ssh "0.5.5"]])
(use 'clj-ssh.ssh)

(println "Hello!")

(let [agent (ssh-agent {})]
  (add-identity agent {:private-key-path "/user/.ssh/id_rsa"})
  (let [session (session agent "localhost" {:strict-host-key-checking :no})]
    (with-connection session
      (let [result (ssh session {:in "ssh root@hostname ls" :agent-forwarding true})]
        (println (result :out))))))
```

```bash
lein exec my-script.clj
```
