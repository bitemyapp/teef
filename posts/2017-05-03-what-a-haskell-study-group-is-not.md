---
title: What a Haskell Study Group is Not
tags: python, practical, education
---

This article is by Steven Syrek. I'm reposting it here because I endorse what he's saying. I believe Steven brings a valuable perspective on the [haskell book](http://haskellbook.com), reading groups, and education in general.

<!--more-->

<hr>

The most rewarding thing about learning Haskell is the feeling that you are making real progress toward understanding not just the syntax of a programming language but a way of solving problems that is more universal than the domain of software development. The most rewarding thing about teaching Haskell is watching others make this same progress. The most frustrating thing about both learning and teaching Haskell is the widespread attitude that functional programming in general, or Haskell specifically, is difficult.

The goal of any teacher should be to expel this myth. The Haskell community is too small to support much formal education and those most apt to teach have precious little time to do so. And so the myth persists, and many people who could or would learn Haskell do not, whether out of fear, misprision, or lack of communal encouragement. Independent study groups, however, can meet the needs of language learners just as well, but only if they are committed at the outset to pragmatic goals and maintain a culture of mutual support and accountability.

Over the course of about eight months, from September 2016 to April 2017, I ran a Haskell study group in New York City based on Chris Allen and Julie Moronuki's book, [_Haskell Programming from First Principles_](http://haskellbook.com/). Since this book was designed to provide a curriculum, it was ideally suited to our purposes. We covered one chapter a week, more or less, as there is just enough content per chapter to keep a dedicated group humming along: challenged but not overwhelmed.

In this article, I will share my thoughts on what made our group successful, where we went astray, and in particular on what a Haskell study group _should not_ be—as the pitfalls are many but advice for avoiding them, based on actual experience, scant. My own background happens to be in education, which has no doubt informed my observations, but I was just as much a student in this study group, even if I happened to be its organizer. I hope this dual role has given me insight from both sides of the pedagogical divide that will be useful to others thinking about starting their own Haskell study groups, a course of action I highly recommend.

#### 1. A Haskell study group is not meant to serve any purpose other than helping people learn Haskell.

Keep this mantra in mind: the point of a Haskell study group is to help people learn Haskell. Nothing else matters. Refer to this mantra whenever you're in doubt about anything else.

#### 2. A Haskell study group is not a place for people to learn about [monads](https://wiki.haskell.org/What_a_Monad_is_not).

Or any other specific language feature. Do not let the random people who will inevitably show up derail your meetings with general, off-topic questions. Make a schedule, post it publicly, and stick to it. Once you embark on diversions, you may never get back on track. Respect the time, commitment, and ongoing progress of your regulars and let newcomers catch up on their own. Always assume anyone who comes to a meeting has: 1) read the chapter for that week, 2) typed in all the code in the chapter and, separately, 3) attempted to complete all the exercises in the chapter. Avoid spending too much time on topics everyone should already know or any time at all on questions about the language from Meetup shoppers.

#### 3. A Haskell study group should not reinforce mistaken or unhelpful stereotypes about Haskell.

Many people have had a hard time learning Haskell. This has given the language a reputation for being hard to learn. In reality, Haskell is elegant and far easier to learn than many other, less elegant languages. The dearth of teaching materials and low quality of documentation have made it difficult for entrants to gain a foothold in the community as compared to language communities that go out of their way to be approachable to newcomers. The Haskell Book directly addresses this problem, however, and it does no one any good to credit assumptions that Haskell requires you to be a math genius. Haskell is capable of representing extremely abstract mathematical entities, but so is math itself, and you hardly need to understand linear algebra to balance your checkbook. I've been a college writing instructor for over a decade. Writing is hard. Teaching writing is hard. But anyone can become a proficient writer, even if few will ever write literary masterpieces. We can respect the challenge without succumbing to it or using it as an excuse. Practice, and you'll improve. Complain, and you'll remain mediocre. Focus on the positives and the cumulative effect of breakthroughs in the Haskell learning experience.

#### 4. A Haskell study group is not a language advocacy committee.

Given Haskell's rather esoteric reputation in the world of professional developers, you are likely to be issued demands to defend Haskell's general usefulness, the practical utility of certain concepts, or the viability of functional programming as a whole. Don't do it. People have Google if they want to research such things and the entire rest of the Internet if they want to bring their [spoons to a knife fight](https://www.youtube.com/watch?v=mcE0aAhbVFc). Your group exists to teach _people_ how to code, not to teach _developers_ how to do their jobs. Focus on the people. Ignore the trolls, don't use facile comparisons to OOP to explain anything, and don't waste your energy trying to convince the skeptical. Actually, examples of what you can do in Haskell won't necessarily help your cause, because most programming languages can do anything any other language can do. The power of Haskell isn't in any one abstraction. It's in leveraging all of them together to build software, and software is complex, composed of many interlocking parts. You aren't likely to persuade by showing someone "the amazing things you can do with Functor" or what have you. Ultimately, they will have to find out for themselves.

#### 5. A Haskell study group is not an experiment in deliberative democracy.

A successful study group requires leadership. Leadership means that someone has to be in charge. Someone has to find space for the meetings, plan them, show up to all of them on time, be responsible for any contingencies that arise, and enforce norms of behavior. If there is no point person, it is unlikely that the group as a whole will make much progress. Sometimes, leaders emerge organically, but it's better for someone to volunteer to be the organizer in advance. Natural leaders can serve a useful purpose, but they can also be destructive if they aren't reliable, grow resentful at having assumed a role they didn't sign up for, or turn your study group into a cult of personality instead of a collaborative learning environment. It's also essential for the organizer to have buy-in from the rest of the group. On the one hand, participants should appreciate the amount of work it takes to put a study group together and keep it together. On the other hand, an effective leader makes decisions without turning every possible choice into a referendum. Democracy is a fine thing, but in this situation, it is more likely to result in anarchy and listlessness than efficient and decisive action. Respect for the organizer(s) is also relevant to the next point:

#### 6. A Haskell study group should not turn anyone into a martyr.

Whoever is running your group, it's a good idea if the person in charge is a fellow learner. Someone already proficient in Haskell will need to be especially motivated to teach others to stick with a study group for beginners. Another beginner, however, will have intrinsic motivation. In fact, the organizer will have an even stronger incentive to keep up with the work. Beware any situation in which a single person has assumed all of the responsibility but has little incentive to continue participating or is otherwise crushed by the demands of a dysfunctional group culture—it's not sustainable.

#### 7. A Haskell study group is not a traditional classroom.

While it is advantageous for the organizer to have prior teaching experience, it is not essential. Since such experience is in short supply, most groups will have to go with what and whom they have. That means the culture of the group is even more important, because people who don't know how to teach should probably not try to do it. Go over exercises, break into pairs or smaller groups if necessary, but avoid devoting too much of any given meeting to lectures or presentations. Talking is no substitute for coding.

#### 8. A Haskell study group is not a hackathon.

That is, it shouldn't be a freeform, come-and-do-as-you-please affair. Adhere to a long term curriculum, keep individual meetings structured, and enforce norms of accountability. People who drift in and out or consistently show up unprepared are only going to drain energy from the room. Shower those who participate in good faith with praise and attention. Marginalize those who only come for Haskell social hour or for the free food and drinks. Speaking of which:

#### 9. A Haskell study group is not a Meetup.

Go ahead and use Meetup to schedule your meetings. I did. But don't give people the impression that it's one of those Meetups where you can show up to eat pizza, drink beer, and contribute nothing. Likewise:

#### 10. A Haskell study group is not a tech talk.

Your study group is not a form of entertainment. Don't just give talks or show slides. Some of that may be an inducement to attend, but you want students, not audience members. Your best bet is to plug in to a projector, display the REPL side-by-side with the PDF of the book, and code. You can do pairing, mobbing, taking turns on the exercises, or whatever other method you desire as long as you're coding and everyone else has the chance to code right along with you. Live coding can be scary, so make it an exercise in solidarity. If everyone did their homework, then everyone should have something to contribute. I suggest you ask them to close their computers and have them do the exercises again, on the spot, for reinforcement. You'll probably find that everyone ends up loving that in spite of themselves. And failures along the way are fine. In fact, the type checker invites "failure-driven development." Work things out together. Learn to love the REPL together. Encourage hands-on work, and figure out as a group how to leverage error messages to produce provably correct code.

#### 11. Haskell study group meetings should not go on forever.

People are busy. If you get anyone to dedicate an hour a week to attending a Haskell meeting, it's a miracle. Don't ask for too much, or you're just going to discourage people who would otherwise like to come. As with learning anything, most of the work of learning Haskell people have to do on their own. The study group can provide motivation and moral support (and asynchronous assistance if you use Slack or something similar), but the meetings themselves shouldn't be longer than an hour or two. Use them to catch up, go over as many of the exercises as possible, and answer questions relevant to that week's chapter assignment. The book provides a curriculum, and it's best to just follow it. There is no need to diverge into advanced language features or demonstrate practical applications of every concept. Keep it short, and keep it simple. Also, you don't have to make sure everyone completely understands everything. Give people time to figure things out, and help them do so however you can, but if it takes too long, move on. You can always work with people one-on-one if they get stuck somewhere.

#### 12. Haskell study groups themselves should not go on forever.

Choose a realistic goal for your study group. Covering chapters 1 to 18 is a realistic goal, at least to start. That will get you through monads (the denouement for many students), at which point you can decide how to proceed with the more advanced material. Some will have had enough, while others will be ready to learn independently. At any rate, don't give people the impression that they'll be visiting your co-working cubicle farm every week for the rest of their lives.

#### 13. A Haskell study group is not group therapy.

Remember the point of a Haskell study group? It isn't to make people feel good about themselves or give them a place to go at night. It's to teach people Haskell. You don't need to be an intimidating jerk, but you aren't doing anyone who attends any favors by not prodding them at least a little bit. If group members come prepared, they should be able to participate in group discussion of the exercises. You can organize smaller groups if some members are truly shrinking violets, but I don't recommend letting people off the hook just because they're reluctant to speak up. Create a supportive environment, and there's no reason for anyone to be terrified of contributing. You never know when an otherwise shy person may be withholding a valuable insight just for being reluctant to be the center of attention. Moreover, you may never know when one such person's confusion about a concept is shared by others, just because no one wants to admit it. Seek out opportunities for productive conversations. Don't let people be dominating or rude, but also don't let them be window dressing. Both attitudes are often just forms of pride, and you need to break through that to reach the vulnerable, yearning students within. Likewise, don't be afraid to ask people to leave if their attitude or behavior is causing problems or their lack of preparedness makes their ongoing participation pointless and a negative example for others.

#### 14. A Haskell study group is not a race.

Do not skip material in the book, and do not try to cover too much at once. A chapter a week, more or less, should be the pace you adopt. Any faster is overwhelming. Any slower is boring and ineffective. Some chapters are long enough to warrant coverage over multiple weeks, but that should be a rarity. And precious few chapters (certainly not chapter 1) are skippable. Set a reasonable pace, and trust the curriculum. Don't feel the need to move quickly or to slow down, even if some people ask for it. I repeat, most of the work they do, they should do at home. The meetings are for review, as much as you can reasonably do, and not to serve the needs of any one student. Also, don't skip the chapter on QuickCheck, and don't let anyone skip writing all those `quickBatch` tests. We know who you are.

#### 15. A Haskell study group is not a competition.

Programmers often have strong, competitive personalities. Do your best to contain that. Even better, turn it to useful ends: make the game about helping others, not being smarter or further along in the book. Encourage more experienced students to help less experienced students, in pairs if necessary, so the value of collective progress is enhanced and "being in charge, because I'm the best" diminished. That said, a competitive spirit can be motivating as long as it isn't toxic. Whatever keeps people showing up without discouraging others—it's often a fine balance.

#### 16. A Haskell study group should not encourage or facilitate cheating.

Implore group members to at least try the exercises for themselves and avoid looking for solutions online. Insist that they do not post solutions publicly themselves. On a related note, make sure they buy the book. The authors worked hard on it, of course, but most people will value something more highly if they have paid for it. You'll want your group to have that feeling of "buy-in" however you can get it.

#### 17. A Haskell study group should not let its culture develop spontaneously.

The culture of any group of people has at least as much to do with how that group behaves and what it considers acceptable, en masse, than its laws. And culture has inertia. If the culture of your study group is misaligned with the goal of learning Haskell, you will have a hard time changing it. Therefore, establish and reinforce your group's culture from the outset. Be serious about the responsibilities of its members and that it's not OK to show up unprepared. Think about creating the proper incentives for productive behavior, and avoid giving in to a culture of impunity. Over time, as the group becomes habituated to doing things the right way, you won't need to be an enforcer as much. On the other hand, a culture that has gone astray will not easily be corrected with new rules, because by that point, leadership will no longer have the credibility to overrule custom: _consuetudo pro lege servatur_.

#### 18. A Haskell study group does not need anything other than Haskell in order to teach people Haskell.

Use the tools provided by the language to teach the language: the REPL, the type checker, handy compiler extensions such as `InstanceSigs`, etc. Empower students to use these tools to develop an intuition for the type system and how to use it to solve problems. You don't need to explain anything in terms of JavaScript, and be careful about overusing metaphors, too. Comparing Haskell to other languages and making abstract concepts more concrete feel like easy wins, but don't do either as a substitute for teaching and learning the language on its own terms.

#### 19. A Haskell study group is not for people who can't handle discipline and hard work.

The Haskell Book is nothing if not demanding. But the intensity of the curriculum is meant to guide students to develop an intuition for how the type system works and the ability to interpret the terse but expressive syntax of Haskell code as quickly as possible. It helps you build up a repertoire of fundamental concepts and techniques that lead to ever more complex, but still comprehensible, abstractions down the road. There are no shortcuts here. Diligent learners will rapidly establish for themselves that foundation of understanding, and it will help them move more easily, and with a greater sense of achievement, as more advanced concepts are introduced. Conversely, the less studious are unlikely to overcome the psychological barriers that make people think Haskell is difficult. They will grow frustrated and give up as their more enthusiastic peers come to seem like magicians when working with the REPL. The truth is just the difference in commitment.

#### 20. A Haskell study group is not only about discipline and hard work.

After your meetings, go out with the group to a local bar or cafe. Pick a regular place so it becomes a tradition. Not only is it good for community cohesion, which is good for the continuation of your study group, but it gives people a venue for venting their natural desire to socialize—a venue that is not the meeting place itself. You're likely going to end up with a core group of people who always come. Cultivate them. Cherish them. Protect their interests. Get the less committed to want to be a part of the in-crowd, and welcome them with open arms when they prove themselves worthy.
