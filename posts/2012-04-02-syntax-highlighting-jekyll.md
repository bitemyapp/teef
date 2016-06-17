---
title: Fixing syntax highlighting in Jekyll
---

When I was getting the Jekyll version of my site rolling, I ran into some problems with how RedCloth, liquid, and pygments were interacting. What follows is the error that was caused.

<!--more-->

```
Liquid error: undefined method `join'
```

Here is how I solved it:

``` bash
sudo gem uninstall liquid
sudo gem install liquid --version '2.2.2'
```

2.3.0 of liquid is incompatible.
