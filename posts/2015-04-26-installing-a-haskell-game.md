---
title: Building a Haskell game
---

This is as much about demonstrating how to show your process as it is about how to install a game.

So our objective is to build and run a game named [Shu-thing](http://hackage.haskell.org/package/Shu-thing) on Hackage.

![](/images/shu-thing-1.png)

First, I used `cabal unpack` to download the game's sources. Then I changed into the project directory and created my Cabal sandbox. To see more about how to use sandboxes, see my [HowIStart tutorial](//howistart.org/posts/haskell/1).

![](/images/shu-thing-2.png)

Then, with the sandbox ready to go I started to install the dependencies for the project.

![](/images/shu-thing-3.png)

But it failed, something about needing a C library. My next step was to Google search "Missing C Library: GL" so I could find the resolution.

![](/images/shu-thing-4.png)

I chose the first link because it mentioned Gloss, which I know to be a Haskell library and thus likely relevant to my problem rather than a generic OpenGL issue. Realistically, any language would need to install the same OpenGL dependencies, but starting with the most specific questions/answers first is usually best.

![](/images/shu-thing-5.png)

There I saw instructions to install the appropriate GLUT dependencies for Debian/Ubuntu-alike distributions. I was using Ubuntu, so this suited me fine. This also matched what I knew about Shu-thing because its only dependency other than base was in fact GLUT.

![](/images/shu-thing-6.png)

This is me firing off those instructions.

![](/images/shu-thing-7.png)

Then I re-ran the dependencies install with `cabal install --only-dependencies` because they was the last thing to fail, and it succeeded. Next step was to build the executable for Shu-thing itself with `cabal build`. This also succeeded and reached the linking stage, which also didn't fail.

![](/images/shu-thing-8.png)

Then I checked the game out which had a nifty wireframe aesthetic :)

The point here is that I didn't know how to build this game or get it working 15 minutes before I started on this blog post and it wouldn't have done as much good to just dump a listing of instructions. Rather, it was more important "show my work", even if that entailed just googling for what dependencies I needed.

Like the way I share stuff? I'm working on a book with [Julie Moronuki](https://superginbaby.wordpress.com/) who is also testing our book with her 10 year old son. Check out the [Haskell Programming book site](http://haskellbook.com/) to keep posted on what's going on. We've got a mailing list you can sign up for there.
