---
title: How to resolve "Could not find module XMonad.Actions.Volume"
---

You need to install `xmonad-extras` from Hackage as the volume control functions are not included by default in any standard XMonad install. Below is the script for getting it installed.

<!--more-->

``` bash
sudo apt-get install cabal-install
sudo cabal update && sudo cabal install xmonad-extras
```
