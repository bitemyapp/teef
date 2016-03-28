---
title: Speeding up CI builds of our Haskell application
---

I work for a company that uses Haskell for its primary application. We're quite fond of having automatic builds fired off for each push to a branch and in our Github pull requests, so we were using [CircleCI](http://circleci.com) for our builds. Our circle.yml looked a bit like:

```yaml
```

However, I'm also a bit of a speed demon and impatient, so I was getting tired of the slow builds. 

```yaml
```
