
Add this: 
- An instance is the implementation of the methods defined in the class.
- When you use a type class method with one of the types that has such an instance, the compiler looks up the code that dictates how the function works for that type.
- Is a child of just isn't part of the Haskell jargon/lingue

---
Here is the scoop in these phrases
- "Is a child of" - Haskell classes do not have children, they have "instances" which are not at all like object oriented children.  I have yet to come across the term "child" in Haskell.
- "Is a member of" - This term is used in Haskell to mean the list of types that have an instance for a given class (add: explain this). It does not mean a property or method of a class.
- "Inherits from" - This term is used in Haskell to mean that one class gets or shares certain properties of another. This relationship is hierarchical and I'll have more to say about it below.

I might have mentioned I'm not a seasoned Haskell developer. As a result I, despite having read extensively, I did a check to boost my confidence in the above. I searched the entire text of [Haskell Programming from first Principles]() and [Get Programming with Haskell]() for the phrases and here is what I found.



I AM HERE AND NEED TO FOLLOW ON FROM THE LAST SENTENCE ABOVE.



My understanding of Haskell was slowed down by my extensive experience with object oriented programming. This was mostly around the word "class" and the phrases I often heard used such as "is a member of", "is a child of" and "inherits from". (WRONG: From what I can tell, the first two are not valid phrases in Haskell) and the latter means something very different from its object oriented relative.

As I write this I hear someone telling me I'm wrong and maybe I am in some way. I am far from a seasoned Haskell expert. However, in defense of my point, when searching the entire 1,225 pages of the Tomb [Haskell Programming from first priciples](), as well as all of[Get Programming with Haskell](), the phrases "member of" and "child" appear seldom and used in ways that do not in any way relate to Haskell entities such as classes and instances. 

[Get Programming with Haskell]()
- has the word "children" once, but it is referring to human children
- members, p.134, 
- get programming with haskell uses 'members of a class' to mean the list of its instances. Unlike object oriented members of a class, these are not methods nor public variables, they are instances that implement the methods described in the class.



Let's get to it.


Classes are not used for encapulation of functionality or hiding data. They are used to describe the methods and the types of the methods that an instance of the class must implement in order to be considered "an istance of the class".

Let me try an very fake example

Here is a class
```haskell
Class Foo a where
  <> = a -> a -> a

instance Foo (Boo a) where
  myMethod = [a] -> [a] -> [a]
```



Class examples used elsewhere
```haskell
class Num a where
	(+) :: a -> a -> a
	(-) :: a -> a -> a
	(*) :: a -> a -> a
	negate :: a -> a
	abs :: a -> a
	signum :: a -> a
```

`Eq` is the friendliest class as we all understand equality.
```haskell
class Eq a where
(==) :: a -> a -> Bool
(/=) :: a -> a -> Bool
```
You don't see any code there like
```haskell
if a == b then True else False
```


I'm not sure 'is a member of' or 'is a child of' are valid Haskell terms, and 'inherits from' does not mean what it does on the object oriented paradyme.


A class in Haskell is not a way to encapsulate functionality. it is not a container nor a box that holds stuff. It doesn't hold anything. From an object oriented perspective It has no members nor any children.

What it does is describe something zzzzzzzzzzzzzzzzzzzzzzzzzzzz. Let's look at the `Semigroup` class to see what it describes. Here is part of the source code for `Semigroup`

```haskell
class Semigroup a where
	-- | An associative operation.
	--
	-- >>> [1,2,3] <> [4,5,6]
	-- [1,2,3,4,5,6]
	(<>) :: a -> a -> a
```

- add: here you see only types. It isn't usable function.
- add: what types can be a semigroup - anthing that can be joined together
- add: for now, think of instances as the pace where the actual code for implementing the method (in this case `<>`) is.

`Semigroup` 
- Takes only one type `a`
- `a` gives no indication of which type. Therefore it can be any type, but only one type. In other words, one type variable one type.
- And one method `<>`. (Ignore the `()` for now.)
- `<>` is the method name

So what does `Semigroup` do? What can we use it for. What it does, as mentioned above, is describe what an **instance** of `Semigroup` is. It does nothing. Let's demystify what it describes.

`class Semigroup a where`
[1]       [2]             [3]  [4]

[1]. `class` - it is a class
[2]. `Semigroup` - Its name is "Semigroup"
[3]. `a` - It takes one and only one type but what type is not part of the description
[4]. `where` - What what? What follows `where` is the description of `Semigroup`


It doesn't seem very useful yet because we haven't talked about instances yet. Before we do, lets look at a couple of examples of types that "are instances of" `Semigroup`.

The code example above shows the use of `[]`, so one can reasonably assume `[]` is an instance of `Semigrop`. The example also clearly shows what instances of `Semigroup` di via the `<>` method.

```haskell
[1,2,3] <> [4,5,6] == [1,2,3,4,5,6]
```

**It combines things.**


---

Notice I didn't mention algebra, set, associative, nor binary associative operation :) I'm not saying you don't need to know those things, you do, but we can begin to eliminate the enevitable confusion around the word "class" of those with an object oriented background.



# Something on error messages

I finally put a couple of things together in my head

```haskell
> (1,2) == "something"
```
Is an error because `(==)` can only compare things of the same type and the above code is trying to compare `(Int,Int)` to `String` :). If you execute the above code in GHCi you will get an error that starts with
```
Couldn't match expected type
  ‘(Integer, Integer)’
```

What it doesn't say is "Couldn't match expected type **`String`**".
So this tells you the expected type is the first parameter/type - duh.

```haskell
> a = 1 :: Int
> a == "something"
-- Couldn't match type ‘[Char]’ with ‘Int’
--      Expected: Int
--        Actual: String
```
Note that "Expected" is the first parameter to `==`, `a`, which is an `Int`.


```haskell
> "something" == a
-- Couldn't match type ‘Int’ with ‘[Char]’
--      Expected: String
--        Actual: Int
```
Note that "Expected" is the first parameter to `==`, `a`, which is an `String`.

---

I was thinking about Classes and Instances so went looking for a familiar example
```haskell
(++) :: [a] -> [a] -> [a]
{-# NOINLINE [2] (++) #-}
  -- Give time for the RULEs for (++) to fire in InitialPhase
  -- It's recursive, so won't inline anyway,
  -- but saying so is more explicit
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys

{-# RULES
"++/literal"      forall x. (++) (unpackCString# x)     = unpackAppendCString# x
"++/literal_utf8" forall x. (++) (unpackCStringUtf8# x) = unpackAppendCStringUtf8# x #-}

{-# RULES
"++"    [~1] forall xs ys. xs ++ ys = augment (\c n -> foldr c n xs) ys
  #-}
```
