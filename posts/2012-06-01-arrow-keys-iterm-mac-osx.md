---
title: Fixing arrow keys in iTerm and Terminal.app for Mac OS X
---

I am a user of irssi over GNU Screen + ssh as well as locally, and I tend to use Emacs style text movements in bash, so need my ctrl and alt/option keys to work more or less as they do in Linux.

To traverse by word (ctrl-arrow) in bash you need to set the following escape sequences in your key/keyboard settings for your terminal:

```
^[b back-word
^]f forward-word
```

To switch between irssi channels successfully you need alt-arrow to work properly, these are the escape sequences:

```
^[[1;3D alt/option left
^[[1;3C alt/option right
```

The above works for iTerm, for Terminal.app:

```
\033[1;3D alt/option right
\033[1;3C alt/option left
```

The `\033` sequence is equivalent to iTerm's more commonly known `^[`, you can produce this in Terminal.app's key assignment menu by pressing `esc`.
