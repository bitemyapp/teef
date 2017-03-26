---
title: A review of Learn Python the Hard Way, 3rd ed
tags: python, practical, education
---

As a break from usual, I thought I would review Zed Shaw's Learn Python the Hard Way. I've had several beginners to programming ask me what they should use to learn and Shaw's book frequently comes up. I've looked over his materials before when they were a free website but I wanted to see what the current published version was like.

<!--more-->

## A note from the reviewer

This review will be self-indulgent. I make copious comparisons with _Haskell Programming from First Principles_.

I write about and primarily use Haskell in my work now. I used Python for about 7 years of my 10 year career so I am comfortable with Python-the-language even if I don't keep up with the community any longer.

My final conclusions and recommendation are at the bottom of this review!

# Comparing the physical version of Learn Python the Hard Way with the ebook

### Skip this section if you only want to hear about the content.

<img src="/images/lpthw_digi.jpg" alt="Learn Python the Hard Way's cover" style="width: 540px;"/>

I bought the physical version from Amazon and the ebook from Zed Shaw directly. Below you'll see a picture of the physical copy I got.

<img src="/images/lpthw_phys.jpg" alt="Learn Python the Hard Way physical book binding" style="width: 540px;"/>

The margins in the layout are good enough. The main problem with the paper version is that it is a paperback with a typical perfect-bound binding. As a result, the book cannot stay open even on the 88th page depicted in the photo above. Page 88 of the print book was not at all the same content as the ebook. Page 88 in the print book was halfway through Exercise 25, titled "Even More Practice." Page 88 in the ebook was in Exercise 17, titled "More Files." Exercise 25 in the book was located at page 114. The ebook appears to use a slightly less compact layout than the print book, so the page difference will not necessarily be a fixed constant.

The content itself seemed identical, but there were some formatting differences. Here's an example using the common student questions from Exercise 25:

<img src="/images/lpthw_phys_fmt.jpg" alt="Learn Python the Hard Way physical formatting" style="width: 540px;"/>

In the physical version, typewriter text is monospaced, bold, and slightly larger than the bolded text surrounding it.

<img src="/images/lpthw_digi_fmt.png" alt="Learn Python the Hard Way ebook formatting" style="width: 540px;"/>
<img src="/images/lpthw_digi_fmt2.png" alt="Learn Python the Hard Way ebook formatting" style="width: 540px;"/>

In the digital version, it's monospaced but seemingly not bold. It's also the same size as the surrounding text. The result is that the typewriter text is more visually apparent in the ebook version.

## Table of contents

<img src="/images/lpthw_phys_toc.jpg" alt="Learn Python the Hard Way ebook table of contents" style="width: 540px;"/>

The physical version's table of contents is conventional and clear with a clean layout. The structuring in terms of chapter/exercise, section, and then sub-section makes more sense than what I've seen in some tech books. Often in Manning books you'll see "units" encompassing multiple chapters that serve no useful purpose. The physical version of LPTHW transitions to backmatter after the 52nd exercise and some comments on learning.

<img src="/images/lpthw_digi_toc.png" alt="Learn Python the Hard Way ebook table of contents" style="width: 540px;"/>

Unfortunately, the formatting of the table of contents is broken in the ebook version. There are 15 exercises in the appendix of the physical version, enumerated as 1 through 15. The ebook version counts up to 55.17 for the command-line content. The ebook stops abruptly at 55.17 and there's nothing after that. The physical version includes an index at the end which the ebook does not have. I think this is because Zed's indexing was done manually by humans but not incorporated into the source text he renders the ebook from. For [the Haskell Book](http://haskellbook.com) Julie and I have always indexed things in the original LaTeX source. As a result, the eventual print version of Haskell Programming from First Principles should have the same index as the final version of the ebook.

# Content review starts here

## Preface

Zed is right here about learning to code and instruction. I haven't said much about it publicly before this, but his work on Learn Python the Hard Way heavily influenced my earliest approaches to pedagogy and how I thought about the Haskell Book. Much changed as Julie and I learned more about what worked with reviewers, but the basic principle of guiding learners through cumulative exercises is extremely important.

## The Hard Way Is Easier

Zed starts by explaining and justifying the process of learning code by typing code in, making it work, and learning to pay attention to detail. It's a minimalistic approach to teaching a _programming language_, but for learning to code it would be difficult for me to improve on this. The Haskell Book took a lot more pages (over 1,000 to LPTHW's ~300) because we were trying to convey concepts that would have lasting value in addition to teaching people to code in Haskell. Readers should take this section seriously and strive to follow Zed's directions here.

## The Setup

Zed goes into more excruciating detail on getting things setup than even most books written for children. That's not say it's necessarily easy, but he's working harder to smooth the way than most. In older versions of the book Zed would recommend `gedit`. In the print version he recommends TextWrangler for Mac users, Notepad++ for Windows users. The ebook recommends [Atom](https://atom.io) for users of all operating systems which I think is sound advice even if I am an inveterate Emacs user.

For a sense of how much detail Zed goes into, he tells you multiple possible names for your terminal program, to add it to your dock, run it, and not to expect it to look like much.

This section is followed by how to find answers on the internet using Google, which most working programmers understand to be 80% of what they're paid for. He even includes a screenshot of his example search and the results.

## Warnings for Beginners

Zed warns against some common but faulty programmer advice. I largely agree with him but I think his reasons for doing this bear some explaining.

>If a programmer tells you to use vim or emacs, just say ”no.” These editors are for when you are a better programmer.

Practice some charity and ignore the, "for when you are a better programmer" bit. The are two important reasons to take this and the rest of his advice seriously.

1. You cannot conquer all things at once. My coauthor Julie essentially learned Linux, the terminal, Git, LaTeX, and Haskell all at once when she first joined me on Haskell Book. I do _not_ recommend doing this. Load-balancing your stress levels is important when you are a beginner.

2. Programmers are more frequently pathologically impulsive and self-centered than I have seen in any other profession. They are not thinking of _your_ needs as a beginner if they tell you to learn Python 3 instead of Python 2. They're just cheering for red team vs. blue team or whatever other interest they might have. This is not to say I think Zed's necessarily right to teach Python 3 instead of 2. (I don't care) My point is that ignoring what programmers tell you is often sound advice.

## Exercise 1, A Good First Program

I do not plan to review every exercise. I'd like to go to bed at a reasonable hour tonight and I plan to allot myself only one evening to write this review. If only because I owe my dogs Papuchon and Jack some couch/TV time.

This exercise opens with a warning not to skip the front matter and methodological commentary. I can tell he has paid some attention to how people use the book based on this.

The first exercise code opens with a series of print statements. The lines are enumerated in the code block formatting of both versions of the book. The ebook has syntax highlighting, the print version does not. I can't blame Zed for making the print version monochrome, I've priced what it costs to print a color version of the Haskell Book and it was horrifying.

The ebook version only shows a single screenshot of the code in the Atom text editor. The print version shows (monochrome, natch) pictures of the code in TextWrangler on Mac OS X and Notepad++ on Windows.

After the picture(s) of the code in a text editor, both versions of the book show you what running the program should print in a Mac or Windows terminal. These images seemed identical in both versions of the book. Main thing I noticed is that Zed needs to fix his terminal font and anti-aliasing, but I am petty and finicky about type.

Anticipating a common typographical error in the code, Zed points out where the error might've happened and what the error would look like. He also anticipates and informs the reader on how to correct a potential problem with ASCII encodings.

Exercise 1 is bookended by study drills and common questions asked by students. I was able to two of the three drills in Zed's instructions. I'm not sure what Zed was asking for with the first study drill, which is a little worrying as beginners will be using this. I will assume it's something obvious that I missed.

The common student questions occur at the end of the exercises throughout the book. They are intended to catch failure modes. Zed's approach here is more modular than the Haskell Book. I think this works because the individual exercises are brief and typically last a handful of pages. In HPFFP we treated it more like a linear stream of consciousness and address anticipated problems in media res.

## Exercise 2-5

Zed goes over basic syntactic elements like comments as well as expanding what the learner can do semantically by covering basic arithmetic operations and variables. The progression here seems more focused on minimizing the novelty of what is introduced _syntactically_ rather than in what is introduced _semantically_. This is an important pedagogical distinction in the approaches taken by Zed's book and by ours. We ordered the book based on conceptual dependence and difficulty, not on syntactic elements. Syntax didn't count for nothing, but we believed it was the less difficult category than semantics. Our experience bore this out but I don't think this invalidates Zed's method. To give you an idea of what I mean, here's a snippet of progressions of the code samples:

```python
# Ex1
print "Hello World!"
```

Side note: In the ebook, the source code has unicode quotation marks. This means if the reader attempts to copy-pasta the code from the ebook, it'll break. I'm not certain if it was intentional or if it's like our case where we intentionally don't fix things that would make copying and pasting easier. The potential problem with LPTHW here is that someone familiar with unicode might believe they're actually meant to use the fancy quotes and get stuck. Zed doesn't address it in his student questions section that I could find.

```python
print "I could have code like this." # and the comment after is ignored
```

```python
# Ex3
print "Hens", 25 + 30 / 6
```

```python
# Ex4
average_passengers_per_car = passengers / cars_driven

print "There are", cars, "cars available."
```

```python
# Ex5
my_eyes = ’Blue’
my_hair = ’Brown’

print "He’s got %s eyes and %s hair." % (my_eyes, my_hair)
```

This should illuminate somewhat how Zed is adding to the syntactic elements demonstrated in each example as the reader progresses. The common student questions continue to be a strong point of this book in the potential problems they address.

## Exercises 6-12

I will get briefer here as Zed's approach seems consistent and I mostly just want to touch on what the book covers.

Zed covers printing in more detail, escape sequences, string concatenation, and requesting manual user (terminal) input.

## Exercises 13-17

These exercises cover getting user input from the arguments passed to the `python` invocation at the command-line, combining this with input prompts and reading and writing text files. Getting the length of a string is demonstrated. The code written is still in a scripty top-level style.

## Exercise 18

This is where defining functions begins. Zed doesn't stage out increasing complexity of function definition. Instead, the first function the reader sees will be one that has a gather parameter like so:

```python
# the use of * with the args parameter is called a gather parameter
def print_two(*args):
    arg1, arg2 = args
    print ”arg1: %r, arg2: %r” % (arg1, arg2)
```

I did a search through the ebook PDF with Skim and as far as I can tell Zed never mentions what this is called so that readers could learn more about what it is or what it does. Zed could've showed the user how you can define a parameter-less function that can be invoked multiple times to save on repetition, but chose not to. Odder still, the gather parameter example is subsumed by a simple two parameter function and the first is called out as useless in the context of the example.

## Exercises 19-23

Zed begins by demonstrating the definition of variables along with passing them to functions as arguments. Exercise 18 only demonstrated passing string to functions as arguments. The usual carefulness with progression resumes here. This is followed by using files with functions, functions that return a result, a vocabulary overview, and an exercise in reading code.

Exercise 23 seems careless. The exercise suggests reading whatever Python code you can find on Github, Bitbucket, or Gitorious. There's no real attempt to point people towards things they could understand at that point in the book. I suspect most readers don't get very far with this.

## Exercises 24-26

This sub-sequence begins with practice in writing code from the book which synthesizes the elements you've seen so far. The study drills ask you to describe the elements of the code in a manner not dissimilar from the "parts of speech" you might've done in a language lesson. The `help` function in the REPL is touched upon.

This sub-sequence ends with a quiz where the objective is to fix broken code. I think it would have been better had this exercise been littered throughout the book so that the readers would have more practice doing so. Approaching this decision charitably, it could be said the readers had enough mistakes of their own to fix, but we chose to have many more exercises in our book.

## Exercises 27-31

Boolean logic, truth tables, boolean operators, expressions using boolean operators, and equality. This includes expressions like:

```python
”test” == ”test”
”test” == 1
```

Python isn't typed so Zed doesn't seem to comment upon and equality comparison of values unequal types.

Followed by if-else statements and guarding blocks of code with if-statements. The progression is a cumulative synthesis like before.

## Exercises 32-34

Loops and lists begin here and is the title of the 32nd exercise. Appending onto lists, while loops, and indexing into lists are also covered.

## Exercise 35

This sub-sequence opens with branches within functions. What branch refers to here is the multiple "branches" of code which may or may not execute based on an if statement. The first example combines parameter-less functions, if-else statements, variables, user input, converting an integer from a string, printing, aborting the program, functions calling other functions, an infinite while loop, and having an initial function to kick off a script.

The author doesn't wrap the `start` function in the usual `if __name__ == "__main__"` pablum you see in most Python scripts. I suspect he's bucking it on purpose since these programs are not intended to be imported by other Python programs.

## Exercises 36-38

Debugging (the basic process, not a tool), a review of symbols and syntax, reading code, popping and appending to lists, getting the length of a list, splitting strings based on a character divider, and concatenating a list of lists are demonstrated.

## Exercise 39

The construction and basic manipulation of Python dictionaries is demonstrated here. The style is imperative and evocative of how the code's been written through the book so far. There has been no lurch into a functional style yet.

## Exercises 40-42

Modules, classes, and objects begin here. Zed touches on Python being referred to as an object-oriented programming language. This is also where `import` is principally demonstrated.

>My problem is that Object-Oriented Program- ming (OOP) is just plain weird.

The above quote demonstrates the effort Zed put in to explaining OOP.

Treating modules like dictionaries, invoking functions within modules like methods, accessing top-level variables in a module like a property, and using classes in all these same ways are covered.

Object oriented terms qua Python are briefly explained. Basic subtyping with a typical `Animal`/`Species` hierarchy is demonstrated.

## Exercises 43-44

This sub-sequence opens with basic object-oriented analysis and design. This is where things get much wordier than they had been up to this point. The objective is to write a simple game engine. The wordiness wouldn't be unusual in some other books, but there's a lot of upfront conceptual mapping and the basic approach isn't demonstrated or justified with any smaller examples. This would be less jarring if it occurred in almost any other book.

Eventually Zed has the reader write a bunch of stubbed out classes and empty methods to plan out the API. A bit like...defining types. Anyway, they look like this:

```python
class Scene(object):
    def enter(self):
        pass

class Engine(object):
    def __init__(self, scene_map):
        pass
    def play(self):
        pass
```

There's some commentary on top-down vs. bottom-up design. The mapped out API is correctly described as top-down. This is followed by a listing of all the code that fills in the empty methods and classes for the game engine project. The usual "what you should see" sections and study drills follow. The study drill suggests changing the game, asks you fix a bug he left in the code, asks you to explain a piece of code, adding cheat codes, adding a small combat system, and mentions that the game engine is an example a finite state machine. He suggests reading about finite state machines on the web even if it might not make sense. It's a little amusing to see these small gestures tossed out when he made little to no effort to point readers to further resources or examples earlier in the book. I suspect this time was different because some of Zed Shaw's open source work entailed extensive use of finite state machines and he has an affectation for them.

Later, inheritance versus composition are covered. Composition here is simple methods-invoking-other-methods. He strongly recommends against using multiple inheritance. Nothing too objectionable here.

## Exercise 45

The reader is asked to make their own game like the space game that was just demonstrated. There's a lot of commentary on code style and syntactic elements. There's no attempt at ameliorating the blank-slate problem for beginners making a project from scratch.

## Exercises 46-49

Project structure, automated testing with assertions, `nosetests`, exceptions (_very_ briefly), basic text processing with a lexicon, and basic parsing are covered.

Note that the first time exceptions were called out by name was in the overview of symbols but he's been using `try` off and on throughout the book.

## Exercises 50-51

Making a basic web application with `web.py` (a locked version named `lpthw.web`), getting input from a web browser (HTML forms), HTML templates, and automated tests for forms are demonstrated. As you might imagine, the explanations of what makes a web app tick are briefly but my coauthor and I are no less guilty of this. It's a huge topic.

## Exercise 52

The task in this exercise is to refactor the game from exercise 43 into a web application. This covers the basics of refactoring code and web sessions. The reader is expected to do most of the work this time, including figuring out how to make user authentication work.

This one seems like a leap based on how much handholding there had been in the book so far. I felt uncomfortable with the final project in our book because it expects the reader to learn TCP sockets on their own, but I think the lacuna was not so bad there.

## Zed's Commentary

The book includes two bits of commentary separating the 52nd exercise and the content that goes over the command line. One is called "Next Steps" has a couple subsections. The first subsection of "Next Steps" is an overview of how to go deeper with Python, particularly in specific domains like data analysis. I believe this is partly because Zed is trying to empower people whose principal "job" isn't software itself but which might by augmented by knowing how to code. The second subsection is titled, "How to learn any programming language." The advice he gives here is sound and I recommend following it if you are learning Haskell with our book or not.

## Appendix A: Command Line Crash Course

These are the exercises sectioned under "55". This is the content I noted had a very different presentation in the table of contents of the print and ebook variants of LPTHW. The sequence introduces individual command line applications and builds on them iteratively. Examples are given for Linux/OS X and Windows in each exercise. Learning basic file and directory manipulation is the priority of this appendix.

## A note on Python 2 vs. 3

I've reviewed the third edition of this book which uses Python 2 throughout. The next, unreleased fourth edition of the book will be using Python 3. It shouldn't be an impediment for a beginner to learn Python 2 using this book and then move on to using Python 3 afterward, but you should be aware of this.

At present if you go to the purchase page for the ebook on Zed Shaw's website, it'll say

>Learn Python The Hard Way, 3rd/4th Edition For $29.99

I _believe_ this means purchasers of the ebook will get the eventual fourth edition when it is released even if they're just buying the third for now. The downloads in my account for Learn Python the Hard Way included videos and the PDF for the third edition only. If you are uncertain and care a great deal about this, please ask Zed himself to confirm.

# My conclusions

I think this book is positively influenced by Zed's background as a painter and musician. I believe he's studied education as a domain as well and it shows in his work.

Overall, I recommend this book as an introduction to learning to code and to Python specifically for new or early programmers. Evaluating the book on what I believe to be Zed's own goals, the main flaws are in the sudden leaps from meticulously explained code examples to quizzes that expect you to do independent research and implementation. There wasn't much in the way of "intermediate" tasks in the code drills. There's some half-hearted name-dropping, but there's not much guidance for readers who want to learn more than what's covered in the book. To draw a contrast, we name things for what they are in the Haskell Book and footnote _many_ references in addition to recommending further reading at the end of each chapter.

Shifting to my own priorities, I'd say my main dissatisfaction with this book is that I wished there was a "follow-on" book which used a lot of the same methods for teaching people the _semantics_ of Python. Zed has a "More Python" book in the works but I don't know anything about it. The approach in _Learn Python the Hard Way_ is very mechanical but it's very monkey-see monkey-do. I realize this would've exploded the length of the book had it all been in one text. I wouldn't wish the authoring of a 1,000+ page technical tome on any but my worst enemies.

Of the formats of the book available, I recommend getting the ebook directly from Zed for the ergonomics of working side by side with the book, your text editor, and a terminal. This is also how we recommend working through the [Haskell Book](http://haskellbook.com), but we're preparing a print version anyway. I only glanced at the videos that came with my purchase of the ebook. They seemed like a fairly straight forward walkthrough of the ebook. I don't rate videos very highly in an educational context except to demonstrate basic things like [workflow](https://www.youtube.com/watch?v=Li6oaO8x2VY), but your mileage may vary.

_I did not review the Kindle version of this book!_ If I had to guess, it was prepared from the print version by the print version's publisher. As a result, I would not expect it to have the same content as the ebook directly from Zed Shaw. Further, I mostly wrote this review while reading the ebook because it was faster for me. There may be differences between the print and ebook versions I failed to note! I believe Zed continues to update the electronic version.

[Click here to get the print version of Learn Python the Hard Way on Amazon.](http://amzn.to/2niUtWs) (Affiliate link)

[Click here to get the ebook version of Learn Python the Hard Way from Zed.](https://learnpythonthehardway.org/) (No affiliate link)
