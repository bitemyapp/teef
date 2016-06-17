---
title: Why we don't chuck our readers into web apps
---

Haskell programmers that often forget how hard it was to learn and use Haskell. They also forget that without the benefit of a code example that does precisely what one wants, it can be nearly impossible for someone to make forward progress unless they have a solid foundation in the language itself. We justify how this reality influences the way we write the [Haskell Programming from first principles](http://haskellbook.com) book.

<!--more-->

Alternate title: Making web apps considered harmful

My co-author, Julie, and I are writing a book for learning Haskell. We're writing it with the goal of getting anyone, no matter their background in programming, from no knowledge of Haskell to an intermediate level with as much real, solid knowledge of how to use and read the language as possible, so that they can go on to use Haskell for whatever purposes they are most interested in.

## Just show me how to make a web app already!

Well, we could [cargo-cult](http://c2.com/cgi/wiki?CargoCultProgramming) you through it, but you won't learn Haskell.  You'll thrash around and give up when you need to do something you can't copy from previous demonstrations. This isn't hypothetical -- I've seen a _lot_ of people give up on Haskell because this is how they tried to learn the language.

What you want is to understand the language well enough so that when you reach the point where you can't copypasta, you have the knowledge to do what you need to do. 

## What you're saying is you _could_ just show me how to write a web app?

Well, let's take the example of the ScottyT monad in the Scotty web framework, a very simple library for making web apps in Haskell. It's actually my favorite for showing people how to make their first web app or API.

To understand ScottyT, you'd need to understand ReaderT because that's basically what it is -- it's providing the request information and parameters via that mechanism.

So now you need to understand ReaderT, which means needing to understand monad transformers and Reader. Understanding monad transformers requires understanding monads. Understanding Reader requires understanding the functor, applicative, and monad of functions. Understanding monads, applicative, and functor requires understanding higher-kinded types, constructor classes. Understanding constructor classes requires understanding typeclasses, kinds, and type constructors. Understanding higher-kinded types requires understanding type constructors and algebraic datatypes.

Understanding kinds requires understanding types, which requires understanding terms and a teensy bit of how mathematical sets work. Understanding algebraic datatypes requires understanding products and sums. Understanding products and sums requires understanding addition, multiplication, types, cardinality, type constructors, and data constructors. Understanding type constructors requires understanding types and data constructors. Understanding data constructors requires understanding terms/expressions, functions, and values. Understanding terms, expressions, functions, and values requires understanding the lambda calculus.

And that's just for the Reader bit in Scotty, not for a more comprehensive web framework like Yesod.

That's why the book starts from understanding the lambda calculus and expressions and works its way up from there.

## OK, maybe what I need is to start with a monad tutorial. The internet has so many of them!

The internet does, indeed, have many monad tutorials. People struggle with monads, often because they don't understand those basic foundations we just outlined very well. Once they finally feel like they have a good intuition for how to use monads, they want to share that intuition so they try to put their intuition into words. It isn't always very successful, because sometimes putting algebras into words is hard. Even when it is pretty decent, it often leads to questionable intuitions on the part of the student reader because the student reader (that would be you) hasn't yet experimented with practical examples and implemented monads on their own. 

In our book, by the time you get to monads, you understand all those foundations well enough that monads themselves aren't that hard. When you understand the monads, then you can work up to understanding monad transformers and, ultimately, Scotty. But the awesome part is, you won't _only_ be able to understand and work with Scotty then. Having that real understanding gives you the ability to understand _so many_ Haskell libraries that rely on monads, Reader, and monad transformers. It's a whole new universe, because you didn't cargo cult to start with.

## Why can't I learn Haskell the way I learned $BOGOLANG?

Because `$BOGOLANG` is just like every other language you've already learned. Hash-mappy/stringy/numby/mutatey whatevers.

## But why does Haskell take so long to learn?

It doesn't, and it isn't really hard. You just have more to learn upfront than you would jumping between Ruby/Python/Go/JavaScript/etc.

Haskell really requires less learning to understand than most imperative languages. Don't believe me? See if you can predict [what these Java programs do without recourse to the Java Language Standard](https://github.com/tonymorris/java-trivia). With Haskell more (not all) of how the language works is a fairly natural and necessary byproduct of the core lambda calculus. If you can understand the lambda calculus and how basic mathematical functions -- arithmetic, low-level algebra -- work, you can understand Haskell. 

If you want another example, consider how many undefined behaviors you have to learn to identify (often involving non-local effects) to write a correct C program which will behave the same across different compilers and compiler versions.

Haskell can be learned quickly, especially if you are able or willing to put aside a lot of what you know about other languages and meet Haskell on its own terms. I could train somebody in a week or two of pair programming like IMVU did with their Haskell hires, and I've given people 3-4 hour accelerated tours of Haskell in the past. 

## Okay, but this book seems _long_ yo.

We're trying _very hard_ to make it simultaneously accessible to non-programmers and still take everyone (experienced programmers included) far enough that they can begin to use Haskell for their own projects. Heck, there are experienced _Haskellers_ who tell us their understanding of Haskell has been enriched by what we've covered. When you're writing a book for self-learners you have to go slower and be much more careful about covering everything.

The combination of a meticulous pedagogy, assuming very little about the reader, and wanting to cover enough idioms in Haskell code that you'll be able to pick up the rest lends itself to a longer book. We think the book reads relatively breezily and most of the "length" is actually examples and exercises, _not_ dense prose.

## But I wanna learn Haskell by doing projects!

That's fine, but you still have to learn the language before that even remotely makes sense. If you haven't learned Haskell, then you do not _by definition_ know how to learn Haskell. The approach we take is based on experience teaching people, and plunging into projects prematurely is a strong correlate for burning out and giving up.

## So you're saying there aren't any projects in [Haskell Programming](http://haskellbook.com/)?

No no, there *absolutely* are projects and as we complete more of the book, we'll add more! But we take a gradual and careful approach to them, introducing them only after you've learned the basics of the language. This makes it so you can focus on each step of the process. This also minimizes the amount of cargo-culting required to get a working project as well.

## Is there anything you could cut?

We could cut the chapters on non-strictness and IO and it wouldn't bite into the core pedagogical goals, but I really wanted to address those topics as they are common but unnecessary sources of confusion and misunderstanding.

Not much else really. A compactness edit would only shave 5-10% off at best.

I guess we _could_ cut some exercises but you don't really want that, now do you?
