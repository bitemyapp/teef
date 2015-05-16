---
title: Why are types useful?
---

Rejected title: OOP is a Ford Pinto.

[This is a respin.](http://inventwithpython.com/blog/2014/12/02/why-is-object-oriented-programming-useful-with-an-role-playing-game-example/)

I am going to quote the author and their code examples then comment and show how I would do it in Haskell.

Apologies upfront to the author and audience as I am using them as the whipping boy for some pet peeves of mine.

> Taking out the graphics from these RPG video games, the characters, armor, and other objects are just a bunch of integer or string values in variables. Without using object-oriented concepts, you could implement these things in Python code like this:

```python
# Python code
name = 'Elsa'
health = 50
magicPoints = 80
inventory = {'gold': 40, 'healing potion': 2, 'key': 1}

print('The hero %s has %s health.' % (name, health))
```

Just going to translate this.

```haskell
-- this is Haskell code yo.
name = "Elsa"
health = 50
magicPoints = 80

-- couple options for inventory

-- lispers should feel at home here.
inventory = [("gold", 40), ("healing potion", 2), ("key", 1)]

-- assume we have `import qualified Data.Map as M` at the top
inventory2 = M.fromList inventory

-- if there aren't that many possibilities
data Inventory = Inventory {
    gold :: Int
  , healingPotion :: Int
  , key :: Int
  } deriving (Eq, Show)

inventory3 = Inventory 40 2 1

main = putStrLn msg
  where msg = "The hero " ++ name
              ++ " has "  ++ health
```

Skipping some code.

> In fact, since the hero will have all the same features of monsters (health, inventory, etc.) we can just have a generic LivingThing class that the hero and monsters share. Your code can be changed to look like this:

Sounds like a fine idea.

```python
class LivingThing():
    def __init__(self, name, health, magicPoints, inventory):
        self.name = name
        self.health = health
        self.magicPoints = magicPoints
        self.inventory = inventory

# Create the LivingThing object for the hero.
hero = LivingThing('Elsa', 50, 80, {})
monsters = []
monsters.append(LivingThing('Goblin', 20, 0, {'gold': 12, 'dagger': 1}))
monsters.append(LivingThing('Dragon', 300, 200, {'gold': 890, 'magic amulet': 1}))

print('The hero %s has %s health.' % (hero.name, hero.health))
```

Well, I have a few ways I could go about this in Haskell. I could make LivingThing into a typeclass or a data type. I'm going to keep it simple and start with a data type and see where it leads me.

```haskell
-- in real code, I would make all this
-- String/Int nonsense newtypes.

import qualified Data.Map as M

data LivingThing = LivingThing {
    name :: String
  , health :: Int
  , magicPoints :: Int
  , inventory :: M.Map String Int
  } deriving (Eq, Show)

hero = LivingThing "Elsa" 50 80 M.empty

goblin = LivingThing "Goblin" 20 0
    M.fromList [("gold", 12), ("dagger", 1)]
dragon = LivingThing "Dragon" 300 200
    M.fromList [("gold", 890), ("magic amulet", 1)]

monsters = [goblin, dragon]

main = do
  -- I can actually just print the hero now since I have a show instance
  print hero
  -- or manually format it
  putStrLn msg
  where msg = "The hero " ++ (name hero) ++ " has "
              ++ (health hero) ++ " health."
```

> Hey look, using classes has already cut down our code in half since we can use the same code for both player characters and monsters.

Yeah but your class doesn't say what name, health, inventory, magicPoints *are*. I saved the same amount of code but now my compiler will make certain I don't forget the structure of my data.

> In the above code you are defining a new data type/class (pedantics aside, but the two terms are basically the same.)

1. No they're not. You don't have types in Python.

2. "pedantics aside" - Yeah we're programmers why would we care about details. Not like we interact all day with super-fast machines that propagate mistakes at light-speed if we have the slightest slip-up like an evil genie on PCP.

> Say you wanted to add "hunger" levels to your RPG. If a hero or monster has a hunger level of 0, they are not at all hungry. But if their hunger level is above 100, then they take damage and their health value decreases each day. You could change your __init__() method to this:

k

```python
    def __init__(self, name, health, magicPoints, inventory):
        self.name = name
        self.health = health
        self.magicPoints = magicPoints
        self.inventory = inventory
        self.hunger = 0 # all living things start with hunger level 0
```

k

```haskell
import qualified Data.Map as M

-- again, I'm not validating properly but
-- having a separate smart constructor sets
-- things up for later.

data LivingThing = LivingThing {
    name :: String
  , health :: Int
  , magicPoints :: Int
  , inventory :: M.Map String Int
  , hunger :: Int
  } deriving (Eq, Show)

mkLivingThing name health magicPoints inventory =
  LivingThing name health magicPoints inventory 0
```

>Without changing any other line of code, every LivingThing object in your game now has a hunger level! You don't have to worry about some LivingThing objects having the hunger member variable and some not: the very definition of what a LivingThing is has been updated.

And if I set `self.hunger` to `None`? Where's your god now?

>You also don't have to change any of the constructor calls since you didn't add a new hunger level parameter to the __init__() method. That's because 0 is a good common-sense default value for a new LivingThing object's hunger level. If you do add a new parameter to __init__() for the hunger levels, you'll have to update all the code that calls the constructor. But this is true for any function anyway.

Particularly:

> common sense default value

I'd like to have a conversation with the programming community about the unconscionable abuse it subjects "common sense" to (1) but that's for another day. I'll zoom in on "default" instead.

A default value only makes sense with respect to a particular *operation* performed with that value. It turns out, those kooky mathematicians we've been assiduously ignoring have already formalized this idea as "identity".

This is why `Monoid` in Haskell can have a "default", identity, value. Because it's the identity *of* an operation.

Riddle me this: What is the default `Integer`? There ain't one. 42 is as valid as any other answer.

Reformulation: What is the identity for summation? For what x will "x + y == y" for all y?

What is the identity for multiplication? For what x will "x * y == y" for all y?

(1): Cf. "natural law"

> If your RPG has a lot of default values for things, you can avoid a lot of "boilerplate" code by using classes with a constructor that sets the default for you.

That's never the stuff that eats through your sanity like xenomorph acid through the medical deck of the Nostromo. It's getting a grip on what's going on that's hard and doesn't scale well.

> Methods are useful for running code that affect the object itself. For example, you could just have code that changes the health of a LivingThing object directly:

```python
hero = LivingThing('Elsa', 50, {})
hero.health -= 10 # Elsa takes 10 points of damage
```

> But this isn't a very robust way to handle taking damage. Lots of other game logic needs to be checked whenever something takes damage. For example, say you want to check if a character dies each time they take damage. You would need code like this:

```python
hero = LivingThing('Elsa', 50, {})
hero.health -= 10 # Elsa takes 10 points of damage
if hero.health < 0:
    print(hero.name + ' has died!')
```

> The problem with the above approach is that you would need that check everywhere your code decreases the health of a LivingThing object. But duplicating code is a Bad Thing. The non-OOP way to prevent duplicate code would be to put it in a function:

```python
def takeDamage(livingThingObject, dmgAmount):
    livingThingObject.health = self.health - dmgAmount
    if livingThingObject.health < 0:
        print(livingThingObject.name + ' is dead!') 

hero = LivingThing('Elsa', 50, {})
takeDamage(hero, 10) # Elsa takes 10 points of damage
```

> This is a better solution because any updates to takeDamage() (such as factoring in armor, protective spells, bonuses, etc.) just have to be added to the takeDamage() function.

Yep. Here's the Haskell:

```haskell
data TheyDead = TheyDead deriving Show

takeDamage :: Int -> LivingThing -> Either TheyDead LivingThing
takeDamage dmg lt = if (health newLt) <= 0
                      then Left TheyDead
                      else Right newLt
  where newLt = lt { health = (health lt) - dmg }
```

Also not sure why original author did a <0 comparison...is 0 hp not dead?

> However, the downside is that when your program grows in size, it's easy for takeDamage() to get lost in among them. It isn't so clear that takeDamage() is related to the LivingThing class. If you have hundreds of functions in your program, it will be hard to figure out which ones are related to the LivingThing class.

Nope.

Your problem is a lack of types. If you think physical co-location is going to tell you what relates to what, you are *doomed*.

```python
class LivingThing():
    # ...other code in the class...

    def takeDamage(self, dmgAmount):
        self.health = self.health - dmgAmount
        if self.health == 0:
            print(self.name + ' is dead!')

    # ...other code in the class...

hero = LivingThing('Elsa', 50, {})
hero.takeDamage(10) # Elsa takes 10 points of damage
```

That was the code equivalent of when you want your parents to think you're eating your food by shifting the spaghetti around on your plate.

>Methods and member variables can be marked as public or private

You can choose not to export the constructors of a data type, making it what Haskellers call an *abstract data type*.

> In some languages such as Java, this "can be called/set" is strictly enforced by the compiler. In Python, there's no such concept as "private" and "public". All methods and member variables are "public".

Sure. Within that modality, Java is doing a better job enforcing intent. Ideally it just wouldn't be necessary.

>But all of OOP organizing is for nothing if you accidentally put this code in there:

```python
class LivingThing():
        # ...code in the class...

hero = LivingThing('Elsa', 50, {})

# ...some more code...

if someCondition:
    hero.health -= 50
```

>That hero.health -= 50 will subtract 50 points of health, without taking into any consideration what armor Elsa is wearing, if she has protective spells, or is wearing that enchanted healing cape. This code bluntly decrements health by 50.

So not only are you unable to enforce basic guarantees like, "health is always an int", you can't abstract anything in the modality your language purports to be inspired by.

> It's easy to forget about the takeDamage() method and accidentally write code like this. This doesn't check if the hero object's health member variable has dropped below 0. The program continues as though Elsa is alive even if she has negative health! This is a bug we can avoid with public/private members and methods.

Wouldn't it be cool if we could jot down constraints for our types and functions once and then let computer programs enforce them for us?

> If you rename the health member variable to _health and mark it private, then it's easy to catch this bug when you write it:

```python

hero = LivingThing('Elsa', 50, {})

# ...some more code...

if someCondition:
    hero._health -= 50 # WAIT! This code is outside the hero object's class but modifying a private member variable! This must be a bug!
```

Weaksauce. If you were serious about `health` being private you would've renamed it to `_health_PLEASE_DONT_TOUCH_SERIOUSLY`.

> In a language like Java where the compiler enforces private/public access, it would be impossible to write a program that illegally accesses a private member or method. Object-oriented programming helps prevent these kinds of bugs.

No, types do.

> Inheritance

I need more bourbon for this.

> Using the LivingThing class for dragons is nice, but dragons have a lot of other qualities in addition to the ones provided by LivingThing. So you want to create a new Dragon class that will have member variables like airSpeed and breathType (which can be string such as 'fire', 'blizzard', 'lightning', 'poison gas', etc).

Makes sense.

> Since Dragon objects will also have health, magicPoints, inventory, and all the other things that LivingThing objects have, you could just create a new Dragon class and copy/paste all the code from your LivingThing class. But this would result in duplicate code, which is a Bad Thing.

I understand objection to duplicate code but there are better ways than inheritance and overriding.

> Instead, make a Dragon class that is a subclass of the LivingThing class:

At least they didn't call it a subtype.
 
```python
class LivingThing():
    # ...other code in the class...

class Dragon(LivingThing):
    # ...Dragon-specific code in the class...
```

My counter-part is getting lazy.

> This is effectively saying, "A Dragon is the same as a LivingThing, with some additional methods and member variables".

Yeeaaahhh...it never stops at "additional", you almost always end up overriding stuff and then it becomes a game of, "whose code is it anyway?" aka code ravioli.

Anyway, in Haskell.

```haskell
data DragonBreath =
    FireBreath
  | IceBreath
  | LightningBreath
  | Halitosis
  deriving (Eq, Show)

data Dragon = Dragon {
  livingData :: LivingThing
  fireBreath :: DragonBreath
  } deriving (Eq, Show)
```

> This principle is called subtype polymorphism.

You don't have polymorphism or types, so all you have is a 'sub'...that can't swim.

> In practice, inheritance is easy to abuse though.

"In practice, this car may explode"

> You must be certain that any conceivable change or update you make to the LivingThing class would also be something you would want the Dragon class and every other subclass of LivingThing to also have. This might not always be so straightforward.

Djikstra's spinning so hard the Earth will be plunged into the daystar like we deserve.

> For example, what if you created Monster and Hero subclasses of the LivingThing class, and then created FlyingMonster and MagicalMonster subclasses from Monster. Would the new Dragon class be a subclass of FlyingMonster or MagicalMonster? Or maybe just its own subclass of Monster?

...

> This is where inheritance and OOP start to get tricky and religious arguments over the "correct" way to design classes come about.

I don't care for Smalltalk, but this is where Kay's quote about pop culture comes to mind.

> I hate programming tutorials for beginners that start with object-oriented programming.

We can elide "beginners" and "start".

> OOP is a very abstract concept.

It's a faux programmer religion with no foundations in anything principled. Insofar as Java and C++ failed to live up to true "OOP", they were doing their best to use the parts of established programming language theory that *worked* in a practical sense and had principled foundations.

# All is not lost.

There is an essential aspect to OOP that is useful and can be used in how we think about Haskell code.

Want to know what I'm talking about? Read:

1. [These slides](http://userpages.uni-koblenz.de/~laemmel/paradigms1011/resources/pdf/xproblem.pdf)

2. [Wadler's Email](http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt)

3. [If you dare](http://okmij.org/ftp/tagless-final/)
