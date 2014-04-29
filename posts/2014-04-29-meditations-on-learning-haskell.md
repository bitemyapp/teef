---
title: Meditations on learning Haskell
---

This is an extraction of a conversation between multiple Haskellers in an IRC channel. I'm not identifying who said what and the ordering will be more topical than temporal. I'll add annotations for context as appropriate. I edited liberally.

The letters are individual people so you can follow the conversation. I did this less for anonymity and more to emphasize the ideas and experiences rather than the people sharing them.

## C
One concern I have is some people I'm exposing to Haskell will hear of your exploits and then turn it into an "intrinsic smarts" rather than "effort and hard work" thing.

I don't doubt you're intelligent, but to emphasize that is to disempower & demotivate.


## E

Said he'd rather hire a bunch of 'dumb mathematicians' to work on a problem than one brilliant one who could do everything he needed to do in his head.

As I'd always been the guy who could do everything in my head, and he was indirectly commenting on something I'd done.

Well, you see, the guy who can do it all in his head never had to learn how to break a problem down into smaller parts. The less intelligent guys _had_ to as a survival trait. Learn how to break off some piece and solve it to make progress.


## B

I sometimes get that attitude from the rubyists & javascripters I know, that anyone
who does haskell is some kind of big-brained freak.

Keep trying to convince them i use haskell because i'm fucking dumb.


## E

I routinely write code in Haskell that I am not smart enough to write.

I just break it down into simple enough pieces and make the free theorems strong enough by using sufficiently abstract types that there is only one definition.


## B

Right - so just small steps and steady progress.


## E

I don't write these stupidly general things because I'm smart. I write them that way so that they write themselves. It is just a case of setting up the questions and asking 'is this thing an instance of Foo?' 'what about that thing?' and just asking all the questions one after another until all the instances are there.

That is why my code is so meticulous about ensuring all the instances are defined. It is how I convince myself they should or shouldn't exist. Write them or disprove them one after another.

The plus side of that is that by the time you're done you're damn good at going through those motions.


## B

Right - so just small steps and steady progress.


## S

I often worry proficient haskellers have a form of survivorship bias. Through
big-brains they succeed and with good-nature they think "if i can do it, anyone can".
But they're wrong and it is just the big-brained freak thing, and the rest of us
are destined to fail.


## C

This attitude towards leverage would scare some of them though. They want to feel like
artisans that holistically understand their precious code. Mechnical reasoning is
alien to them.


Holistic understanding of a system doesn't scale. This is easy to prove just on the basis of how we interact with operating systems via APIs and guarantees.

Incidentally, typeclasses are an interface (API) and some guarantees (laws).

## E

I figured the answer to the smart vs. dumb mathematician dilemma was to take a smart
mathematician and give him the dumb mathematicians' tools.

Really that is a smart mathematician, the other guy is just a kid who is good
at calculus.


## C

I'm terrible at programming. I'm a bad programmer and not very bright. Haskell
covers for my dumb ass. It required learning some new stuff, but none of it
was, in isolation, all that difficult.

It's the process and unfamiliarity that alienates people, not
intrinsic difficulty. Most peoples' problem with learning Haskell is they
think too much about the wrong things.

If they emptied their minds they'd have an easier time. Analogy will betray you.



## B

It might have to do with being ok with feeling stupid/uneducated. I know a lot of
senior-dev types who don't like the idea they don't know it all already.


## E

What i don't get is the rush. People diving into haskell going
'i must write a web app today!' before they have any idea what is going on.


## B

That's been the typical pattern for every new language/framework.

If you can't write your own blog code in five minutes what good is
this stupid thing anyway.


## C

Some other communities kinda set that as a benchmark.

Which is reasonable when your languages are 95% similar across the board.

When you encounter something alien, you have some foundation building to do
before that makes sense.


## M

Hence my favourite advice for learning haskell or a lot of things for that matter...
Slow down.


## E

Sure, before i found haskell everything i learned was just syntax.

Haskell was...unexpected. I had assumed i knew everything there was to know
about programming.


## B

One of the things i really love about haskell is that it makes it quite clear it
isn't the endpoint either - that there's this whole world of dependently-typed
stuff we'll one day be able to use.


## S

I think the rush is people interested in haskell want to start using it, then find they
can't do anything they normally do. that's frustrating.

Some find they can figure it out. Those people succeed.



## E

So when i found haskell i slingshotted off through dependent and substructural types.
Assuming that if a little was good a lot was better. Made it half way through TaPL
and found pure type systems, coq, etc.

I think the power to weight ratio isn't there. I find that Haskell gives amazingly
expressive types that have _amazing_ power for the amount of code you tie up in them
and that are _very_ resistant to refactoring.

If i write agda and refactor I scrap and rewrite everything. If i write haskell, and
get my tricky logic bits right?

I can refactor it, split things up into classes, play all the squishy software
engineering games to get a nice API I want. And in the end if it still compiles I
can trust I didn't screw up the refactoring with a _very_ high degree of assurance.


## C

Admittedly I'm not playing at the level E is, but this was my experience. I can make
sweeping changes to my API, get all the bugs caught by the type system, and still
have minimal code impact.


## E

That freedom to refactor is probably my greatest love for haskell.


## M

I'm having that right now, I'm trying to apply what I've learnt to my Write Yourself
a Scheme repo and I can just keep refactoring things with almost *complete*
confidence that it'll work if it compiles.


## E

There are only so many 'ground truths' to write down, but so many ways to organize it.
So having that ability to bounce through them quickly, to find something that has
good taste? That matters a lot to me.

After all i spent all that time stomping around as a 'software engineer' aka, not
a computer scientist, but someone who wants to artfully arrange their code.


## B

That is what I was getting at with the tweet about not using dynamically typed langs
because I need to be able to prototype quickly and get rapid feedback.

I think a lot of my friends thought i was just being trollish. Even just being able to
see what would have to change if you changed your design slightly and being able to
back it out quickly...


## E

Really its just that when I wite haskell I write code i can actually for once in my
career _actually_ reuse. Not plan to reuse.

Not promise to factor out later. Just outright reuse directly.

90% of my libraries expose _all_ of their guts. No information hiding and there are
no problems. Parametricity used right is so nice.

You can test small things in isolation. Free theorems get stronger.

We took the typechecker for $COMPILER and were about half way through writing it in
Haskell. This compiler spits out 'witnesses' of type checking as 'core' expressions
for later stages. We had to stop and work on something else for 6 months mid-authorship.

We came back, dusted it off for a week, updating dependencies. Slotted in and got to
work...and it worked perfectly the first time. Then we found we could shed 30% of it.

Did so, and it _still_ worked perfectly.

Take 6 month of context away and try to dive into any codebase with non-trivial
invariants like a compiler in _any_ other language. That example is just a month
or two old even.


## My experience

I've had type errors in Clojure that multiple professional Clojure devs (including myself) couldn't resolve in less than 2 hours because of the source-to-sink distance caused by Clojure being dynamically typed. We had copious tests. We added println's everywhere. We tested individual functions from the REPL. It still took ages.

It was only 250 lines of Clojure.

I did fix it the next day, it was due to vectors in Clojure implementing IFn. The crazy values that propagated from the IFn usage of the vector allowed malformed data to propagate downward far away from the origin of the problem.

The same issue in Haskell would be trivially resolved in a minute or less because the type-checker will identify *precisely* where you were inconsistent.

I use Haskell because I want to be able to refactor without fear, because I want maintenance to be something I don't resent. So I can reuse code freely.

This doesn't come without learning new things. Yes, it takes time, but that's why I keep my gist updated on how to get started learning Haskell (linked top right of my blog).

I still use Python and Clojure at work but I have no intention to do anything more than the bare minimum to support the open source Clojure libraries I wrote or worked on. I'm moving everybody I can over to Haskell.

If you didn't catch the bit about Haskell have a very nice power-to-weight ratio, there's another point I'd like to make. I hear people using Clojure beg-off learning Haskell because they'd rather use a "real" type system such as Agda or Idris.

This is created an excuse to not learn something new. You are delusional if you think learning (or for that matter *USING*) Agda or Idris is easier than Haskell. They're both nice languages for which you could find uses in production, but Haskell is ready here and now with a power-to-weight ratio that most dynlang users would appreciate.

This is why I use Haskell. It's easier and enables me to do a better job. That's it. There's a ramp-up required in order to get started, but that can be ameliorated if you empty your mind and stop being in a rush.

I get it, you're threatened. Maybe you even staked some of your credibility on your current language of choice. Maybe you're known in your professional network for using your particular language of choice. I've gone through the same process. It behooves you to be professional and be willing to change your mind in the face of new information.

I know I'd rather have a doctor who is willing to understand that he makes mistakes, will be responsive to new information, and even actively seek to expand his understanding rather than one that hunkers down with a pet theory.

If it wasn't obvious, the story about people "good at math" that "do it in their head" versus the professional mathematicians is an analogy. It's also a true story. When you're using a dynamically typed language, you're forcing yourself unnecessarily to do it "in your head". Thinking in terms of types is a different way of working, but it works.

Before you ask: Scala isn't good enough. 

[Scala FP Book review](http://www.quora.com/Reviews-of-Functional-Programming-in-Scala-2013-book)

[Summary of some of the problems](http://www.reddit.com/r/haskell/comments/1pjjy5/odersky_the_trouble_with_types_strange_loop_2013/cd3bgcu)
