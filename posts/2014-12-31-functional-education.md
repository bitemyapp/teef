---
title: Functional Education
---

Rejected title: Focus on fixing things.


# The problem of learning functional programming

There was a thread recently where a person was complaining about [LYAH](http://learnyouahaskell.com), [cis194](http://www.seas.upenn.edu/~cis194/spring13/index.html), the [Typeclassopedia](http://www.cs.tufts.edu/comp/150FP/archive/brent-yorgey/tc.pdf), [Real World Haskell](http://book.realworldhaskell.org/), and other materials for learning Haskell. Their post ended with, "So, as I apologize for what could easily be considered a rant, can anyone please point out a simple, error free, and intelligible haskell tutorial?".

It was an unconstructive rant, but I don't blame them for being frustrated. It took me a long time to get anywhere with understanding Haskell. Only after several years and and after I started teaching what I learned did I get any traction. There were several replies to the submitter of the thread. Not all of those replies were wrong. One of them mentioned "learning to learn". Programmers are usually bad at learning. Most schools will not teach people how to learn. That's at best a secondary side-effect that happens on accident with the more motivated students. Primarily, schools teach you how to pass tests.

So yes, the ranter needs to learn how to learn, but the materials they're being presented with aren't helping.

Also replies like, "it might not be the Haskell texts that is the problem", are totally unconstructive and play into the "elitist" caricature of functional programmers. I don't care if you invented Haskell, wrote GHC, wrote 10 books about Haskell, if you can't be helpful then *just don't say anything*. Go get a cappuccino and chill. Learning Haskell is hard, especially for people that aren't accustomed to thinking in terms of mathematical functions. Which is, incidentally, *most programmers*. Most programmers are used to learning mostly similar programming languages, swapping out syntaxes as they go. They are not accustomed to a new programming language involving a total reconfiguration of their brain. It's as much about *setting expectations* as it is anything else.

The existing texts for Haskell aren't using an informed pedagogical discipline. Everyone is winging it. This makes it more painful to learn Haskell than it needs to be. Another problem is the lack of integration and coverage. There's a ton of materials covering "how do I arithmetic?" through to "what's a typeclass?" but they rarely go much further than that. We should look to what successful educators in mathematics like Vinogradov have tried for ideas on how to fix the situation. We should also be more systematic and empirically informed in how we develop learning materials. Aesthetic fetishization drives most peoples' tutorial design process, not effective pedagogy. You can see a similar problem with recommendations of works like Mac Lane's Algebra as an introduction to algebra to people that have almost no mathematical background when it was intended for a totally different audience. This is fetishization without taking your intended audience into account. Consider that Vinogradov's work was used to teach algebra to *schoolchildren*. Schoolchildren can be taught Haskell too.

Haskell is sufficiently different that we're teaching people how to learn to program from scratch. The materials should reflect that and be informed by experience teaching Haskell.


# Before I start talking about specific courses and books

This is a rundown of what's worked and not worked about different materials for learning Haskell. If these materials hadn't existed, I probably wouldn't be a happy Haskell programmer today. Using Haskell for my 9-5 work and for my side projects has improved my life considerably.

As a result, I owe our Haskell forebears a great deal for writing all this material. I am critiquing these materials because I think we can do a better job and that doing a better job is *important* so that other people can have as much fun programming as I do.

None of the critique is intended to hurt anybody's feelings. I've held off writing a post like this for months because the idea of hurting the feelings any of the people that worked on these materials was shattering to me. I am doing this because:

1. Haskell programmers that are already comfortable with the language seem to live in a bubble WRT learning Haskell

2. I keep having to explain why I recommend `BOOK_B` but not `BOOK_A` to people in #haskell-beginners and I'd rather just link them this post.

I've taught a lot of people Haskell. This review is based on what has worked for a wide variety of people from people entirely new to programming (like my coauthor Julie) to long-time programmers like myself.

I gather personal anecdotes on what material has worked for people, but weight them differently depending on a number of variables. If you haven't taught at least a handful of people Haskell one-on-one, most of the way through the basics and intermediate material, you are going to have some pedagogical blind spots.

## My educational priorities

I want people to be able to use Haskell to make programs and to read other peoples' code. I am not addressing the needs of category theorists or compiler authors.

I write programs for a living. Haskell makes my work pleasant and enables to me to do a better job.

The way I teach is relatively principled, but it is a principled means to practical ends.


# The reviews


## cis194 Spring 2013

[http://www.seas.upenn.edu/~cis194/spring13/]()

### Strengths

Covers the core sticking points that trip most people up: recursion, folds, typeclasses, functor, applicative, monad. The exercises are well-designed and the material is a good progression from one topic to the next.

### Weaknesses

No exercises for Functor, which makes the Applicative homework that much harder. If you're new to recursion, faceplanting into the Hanoi exercise in the first week is often dispiriting.

The explanations preceeding the exercises are, for obvious reasons, brief as they are designed for a classroom where students can ask questions.

Doesn't explain higher kinded types too well. This can make things like `Either a` more mysterious than they need to be. I'm guessing the creator intended to explain things like this if they came up in the classroom.

### Do I recommend it?

Yes! This, along with the NICTA course is the way I teach programmers Haskell. It precedes the NICTA course for reasons explained later.


## NICTA course

[https://github.com/NICTA/course/]()

### Strengths

The exercises are top-notch and thorough. This is my favorite follow-up to cis194 for teaching people Haskell. Forces people to build on their own code which is really nice.

### Weaknesses

*Extremely* difficult if you don't have an on-site instructor as was originally intended for this course or you haven't done some other course first. This is why this course follows cis194, it's a gentler introduction. This used to be the only course I recommended until I was convinced to look at cis194 by a thoughtful gentleman from Australia.

### Do I recommend it?

Damn skippy. It's what I tell people to do after cis194 Spring 13.


## Learn You a Haskell

[http://learnyouahaskell.com]()

### Strengths

Gets people typing code into the REPL, makes the material less intimidating than most introductions to Haskell. Drawings and style reassure people they're not being plunged into a professorial abyss that assumes you're a graduate student in computer science.

One of the few materials to make a stab at avoiding the "multiple arguments" trap. All Haskell functions take one argument and return one result. Most materials do a poor job of not confusing learners on this matter.

### Weaknesses

Deeply pedagogically unsound. The monkey-see monkey-do process is a warm-up, not an effective way to learn Haskell. Desperately needs exercises. Very little in the way of cogent explanations for any of the topics that learners struggle with (folding, functor, applicative, monad). This problem is worsened by the lack of exercises.

Leans way too much on Functor/Applicative/Monad instances that lend themselves to bad intuitions. Does nothing to attempt to alienate these bad intuitions.

The material often bores learners and leaves them feeling like they're not "getting" it. This because they're being "talked at" and demo'd to. They're not engaging with and solving problems.

Sense of accomplishment is critical to keeping learners going. They're not always going to be 100% comfortable with their comprehension of something and they probably never will or should, but they should have a trail of completed exercises in their wake when they start attacking the hard stuff so they don't give up.

Exercises also make certain they don't miss the point. An easy thing to do with a language like Haskell because the semantics are predictable and don't blow up in your face, even when you don't understand what's going on. Most Haskell programmers I know personally have no idea how IO or laziness works, but they're still able to make working programs. This assertion includes *library authors*. This is a programming language design success and an educational failure.

Tricks people into thinking it was designed for people new to programming, but the opposite is true actually. It works for people new to programming as far as "monkey-see monkey-do" works.

### Do I recommend it?

No.

I only recommend it in rare cases with people completely new to programming that need a warm-up to the process of typing code in and seeing it do things. The goal here being to bootstrap people into cis194 (Spring 13). I am strongly considering shifting this recommendation over to the Thompson book instead. (Craft of Functional Programming)

I am tired of doing clean-up duty on people confused by LYAH as are the other teachers in the IRC channel.


## Programming in Haskell by Hutton

### Strengths

Has exercises. Quite a few in fact.

Book avoided confusing IO and monads.

One of the better introductions to parser combinators I've seen. I prefer the exercises cis194 uses for parsers, but I like the explanations in PiH better.

Explains foldr & foldl.

### Weaknesses

The notation used can confuse people new to Haskell and makes it difficult for them to read the code in the book or translate it to real Haskell. Should've stuck with ASCII code that can be typed in literally for the code sections. The earlier exercises are decent, but there's not much conceptual reinforcement and not enough variety in the exercise design.

Assumes too much knowledge of math and mathematical notation.

Explanations of folding isn't thorough and doesn't leave people with a strong intuition of what expressions a fold is equivalent to unless they're familiar with the mathematical notation used to introduce the topic.

Explanation of IO is technically better than other materials but still wanting. The explanation is evocative of the underlying ST'ish tuple in Haskell implementations like GHC but still not to the point. Lacks explanatory power or justification.

The point here about "explanatory power" is important. If you deliver your explanation of IO to a programmer and they can't tell me what adding `unsafePerformIO` or returning into IO (cf. `evaluate` in Criterion) will do to a Haskell program, then your explanation has failed at least from a practical point of view. I'm not even asking for `unsafeDupableIO`, `unsafeInterleaveIO`, or a deep comprehension of thunk blackholing. Just the basics of what purpose the `IO` type serves in a lazy programming language.

Doesn't cover what really trips people up with Haskell. This is serious. At the time Hutton wrote PiH, things like Applicative weren't known to us, but it's already all over the place as a pattern in Haskell because it's useful. These abstractions are often difficult for new Haskell programmers and must be covered.

The way PiH delivers exercises is pretty rough. cis194 helps avoid a lot of unnecessary difficulty by providing some code to frame the exercise. The best way to see this contrast is to look at the Applicative parser combinator exercises in cis194 and compare them to the parser exercises in PiH.

Empty text buffers are a tremendous mental and practical roadblock for beginners and totally avoidable.

### Do I recommend it?

Guardedly. I think there are too many more up to date alternatives to provide a strong recommendation here. The best thing about the book is the exercises, but I don't think that makes it a good choice as a primary resource.


## FP101x

### Strengths

There are exercises (homework).

### Weaknesses

It's Hutton's book imported into a MOOC with some videos of a wildly gesticulating programmer.

The exercises are based on what was in Hutton's book, but made tedious and easy to get wrong for silly reasons. The final homework is deeper and more satisfying, but learners aren't properly prepared for it, so they usually get totally blocked until somebody shows them what's going on.

Doesn't include the deeper and more interesting exercises from Hutton's book.

Some assertions & explanations in the videos are not merely controversial, but misleading or outright incorrect.

### Do I recommend it?

No. If you think the exercises will suit you, work through Hutton's book.


## Typeclassopedia

### Strengths

Goes over the typeclass-reified algebras that are used all over the place in Haskell in a progression that makes sense.

### Weaknesses

You can't use the Typeclassopedia unless you've been introduced to:

Functions, values, recursion, the function type constructor (->), algebraic data types, higher kinded types, typeclasses, some applications of functor/applicative/monad, how type constructor application and type variable scope works, polymorphism, and laws.

It is not a suitable introduction to the typeclasses outlined within.

### Do I recommend it?

I'm going to copy and paste a comment from Reddit that I think explains it well:

> For me, the typeclassopedia was quite a useful thing to read, but only after quite a bit of actually hacking about with solving problems in Haskell. It solidified lots of concepts I'd half picked up but its not something you can sit down and read from a cold start.

Recommended as a post-course review, not as an introduction to these typeclasses. You're better off completing the NICTA course.


## cis194 Fall 2014

### Strengths

Still uses the Spring 2013 format.

### Weaknesses

Huge gaps in coverage, no longer usable as an introduction to Haskell. Possibly because it's supposed to be preceded by some other UPenn class? Not sure what happened.

Doesn't cover fold, for example.

### Do I recommend it?

Absolutely not. Use the Spring 2013 course mentioned above. I probably should've talked to the people running the course about these concerns, but their priorities are whatever UPenn students need. It's not clear to me if cis194 is intended to be a standalone course or not. Rather than harass a poor grad student, I've decided to just keep linking the old course and focus on writing a book for learning Haskell.


## Beginning Haskell

### Strengths

Projects. Giving the learner something to do that has a real result can help a tonne.

### Weaknesses

Projects. Broken code. Incorrect types and explanations. Seems like the code examples weren't tested. Book needs a lot of editing still, not publication-grade and no indications from author or publisher that they will be fixed.

Projects are pedagogically problematic as an *introduction*. They force too many thunks at once and force the learner to do far too much cargo-culting when used as an introduction. Cargo-culting in education should be treated like credit card debt. Should only be used to afford a higher leverage explanation of something and then be paid off as quickly as possible.

It should be noted that analogies and outright lies in teaching are more confusing to learners than cargo-culting.

Projects are a great idea once fundamentals are in place and they're focused on exploring a particular concept or library.

### Do I recommend it?

Not as an introduction, you need to be able to catch mistakes yourself. Can be a good project-workbook if you need ideas for practical projects or an introduction to the libraries used in the book.

The book *does* touch on something which is needed for Haskell, but the implementation is too flawed for me to recommend it to somebody that wants to learn Haskell.


## Haskell - The Craft of Functional Programming

### Strengths

Affordances are made for people that have never programmed before and this makes the book more effective for non-programmers and programmers alike.

Good explanation of functions as they are understood in math rather than allowing the reader to mistake them for *procedures* as they are used in other programming languages.

Tells people to use GHCi. This is good.

The DSL chapter is quite good and covers smart constructors.

Time and space behavior chapter touches on some nice practicalities.

### Weaknesses

Same coverage problems as we see elsewhere - doesn't go far enough for people to start understanding Haskell code written by Haskellers.

Throw-away explanation of recursion.

Doesn't explain what you have to do to make arbitrary code work in GHCi. This is bad.

Time and space behavior is far too short and doesn't touch on knowledge that is portable and lasting. Bird's work on laziness and asymptotics in "Introduction to Functional Programming 2ed" is better.

### Do I recommend it?

For people new to programming as a bootstrap to cis194.


## Introduction to Functional Programming using Haskell 2nd Edition

### Strengths

The explanation of laziness is top-notch. One of the only materials to cover guarded recursion properly.

Relates recursion to induction, something often not seen in other materials.

Has exercises.

### Weaknesses

Introduces IO and monads at the same time, which leads to people often not understanding either.

Exercises are very spare.

Same coverage problems mentioned elsewhere because the book was written awhile ago.

### Note

The beginning parts of the parsing chapter in this book and the one in Hutton's are nearly identical, down to the types and order things are introduced in.

### Do I recommend it?

No, not as an introduction to Haskell. Only after you've done other books/courses and if you'd like an introduction to laziness, induction, and coinduction.


## Thinking Functionally with Haskell

### Strengths

Another Bird book.

Explains parsers like his last book. Connects them more strongly to monads this time.

Folds are more clearly demonstrated in terms of equivalent expressions than in other books.

Explains the numeric typeclasses more thoroughly than the Gentle Introduction, which is nice, but this sort of stuff isn't really important or a serious roadblock for people learning Haskell.

Shows you `:set +s` in GHCi, which is freakin' sweet.

### Weaknesses

The foldr/foldl space/time explanation looks like it was copied and pasted out of the Haskell Wiki.

Still no mention of applicative. Not even in the parsing chapter.

Damnable IO/monad in the IO section. Same doesnt-explain-anything "IO is like you're passing in the world as a parameter!" IO explanation you see in Hutton's book. Makes things worse than what you got with Hutton's book by confusing IO and Monad.

Same coverage problems mentioned elsewhere. You still won't be able to understand most Haskell code written by non-undergrads after reading this book.

*Way* too much talking at the reader before they do exercises. You can't chuck an entire chapter of words at somebody and then bookend it with a long list of exercises. It's confusing, frustrating, boring, and forces a lot of flipping back and forth.

The exercises for the "Efficiency" (covers laziness, space, time, sorting) chapter aren't good. There's no way for the learner to know if they got something right or wrong without looking at the answer. The learner should be able to build confidence in their answers experimentally.

If their knowledge and experience cannot enable the student to form hypotheses and test those hypotheses, then what are they learning? Math students have the benefit of being able to form a conjecture and attempt to work out a proof.

We are depriving learners of programming to do something similar empirically or deductively with programs.

### Do I recommend it?

No. It has some improvements over Bird's Introduction to Functional Programming, so you could consider this as an alternative to that, but the difference isn't compelling and the core problems weren't addressed.

This book was published in 2014.

## Stanford CS240h Spring 2014

[http://www.scs.stanford.edu/14sp-cs240h/]()

### Strengths

Some of the best explanations and motivations of phantom types, quickcheck, generics, streaming, zippers, and lenses I've ever seen. Great patterns for programmers to know so they can harden up their types to more precisely capture and enforce intent.

Sweet explanations for GHC runtime system stuff by Edward Z. Yang.

### Weaknesses

This is expressly an intermediate course, so I'm not docking points for not being an introduction. People should just know that it's more of a finishing course for people wanting to know practicalities and patterns in Haskell programming.

The course has three labs then a final project. I don't have a lot to say about that. If it were up to me, there would be more, smaller exercises for demonstrating each topic but I don't think it's a serious issue. The labs seem quite forgiving, but I may have missed something.

### Do I recommend it?

Yes, but not as an introduction to *Haskell*, just as an introduction to post-fundamentals patterns and concepts you'll want to know like phantom types.


## Gentle Introduction to Haskell

[https://www.haskell.org/tutorial/]()

### Strengths

Explains the numeric types and typeclasses a little bit, which isn't often bothered with. Explains Monads without IO.

### Weaknesses

More like a respin of the Haskell Report than a way to learn functional programming.

No exercises. Explanations are brief and unmotivated.

Dated. Monad explanation isn't well connected to Functor. I don't expect an F-A-M progression but they don't do F-M either. They dive into Monad before things like higher kinded types have been well explained.

### Do I recommend it?

No.


## Note about the Haskell Wikibook

I am still reviewing and testing it. I will edit this post when I am comfortable commenting on it.


## Recap

How to learn Haskell as laid out [in my guide](https://github.com/bitemyapp/learnhaskell) is my recommendation. When I change my mind or discover better material I update the guide.

It should not be missed that the best way to learn Haskell as an independent learner involves two courses that weren't even designed with independent learners in mind.

We can do a lot better.

### But how do I learn Haskell?

For now:

cis194 -> NICTA course

Use Craft of Functional Programming if you're new to programming, need a resource to check for basics, or need a warm-up for cis194.

Do the Stanford CS240h course after cis194 and the NICTA course if you're going to be using Haskell for work.


## What I'm doing about it

I'm writing [a book for learning Haskell](http://haskellbook.com). I have brought on [a coauthor who is a better writer than I am](https://superginbaby.wordpress.com/) and whose first time *ever* programming was in Haskell. She also spends a lot of time teaching, more than I do in fact, as she homeschools her children.

We have very different and complementary backgrounds and strengths that I think are going to help us make a good book for learning Haskell. That said, while I hope the book will be the *best* way to learn Haskell when it is released, I'd rather it not stay at the top for very long.

I want people to make better material!
