---
title: Node.js, Requests, Connections, ending, and closing
---

You close connections, you end requests.

``` javascript
request.end();
```

Works.

``` javascript
request.close();
```

Will produce a type error such as:

```
request Object ... has no method 'close'
```

Read the docs!
