# #type-class #overloaded

*sources: [Index of /comp/15OPLD/Notes](https://www.cs.tufts.edu/comp/150PLD/Notes/) Tufts University*


A symbol is #overloaded if it has two (or more) meanings that are *resolved at compile time*.
- An example is `(+)` which can work with different types of numbers such as `Int` and `Float`.

## Resolved at compile time

If a function is overloaded, then the compiler must choose between the possible algorithms at compile time. The process of doing so is called #overloaded-resolution.

## Overloaded result?

"In many languages, if a function is overloaded, then only the function arguments are used to resolve overloading. " *(p. 154)*

This statement implies that in other languages, perhaps Haskell, the result can be overloaded.

#automatic-conversion is mentioned here but not explained. My guess is that in the articles example of `(+)` with `Int` `3 + 2` vs `(+)` with `Float` `3.0 + 2.0` automatic conversion would handle the case of `Int + Float`, i.e., `3 + 2.0`, resolving to either `Int` or `Float` (but I'm somewhat sure it will be `Float` as the better choice).

## Overloading vs Polymorphism

Could be looked at as
- haskell-style polymorphism == *parametric polymorphism*
- overloading == *ad-hoc polymorphism*.

Where
- parametric polymorphic functions use one algorithm to operate onl may different types
- overloaded functions *may* use a different algorithm for each type of argument

## Why is overloading important?
*(Derived from section 7.1, p.154)*

Because many useful functions are not polymorphic: they work only for types that support certain operations. E.g., `member` only works for types that support `Eq`.

```haskell
member :: [t] -> t -> Bool
```

And `sort` only works for types that have `Ord`.

```haskell
sort :: [t] -> [t]
```

*(p. 156)* This discussion highlights some of the goals for a mechanism to support overloading, **namely, the need to avoid a version explosion** **while allowing users to define new overloaded functions**. In addition, a general principle of language design is to avoid special cases, so designs that treat equality and arithmetic operators specially are considered less good than designs that allow any kind of operation to be overloaded.


## Overloaded vs Polymorphic

This function is #parametric because it depends on the parameter `cmp :: (a -> a -> Bool)`.
```haskell
qsort :: (a -> a -> Bool) -> [a] -> [a]
qsort cmp [] = []
qsort cmp (x:xs) = qsort cmp (filter (cmp x) xs)
                   ++ [x] ++
                   qsort cmp (filter (not.cmp x) xs)
```

This function is #overloaded because it depends on the overloaded operator `(<)`.
```haskell
qsort1 :: Ord a => [a] -> [a]
qsort1 [] = []
qsort1 (x:xs) = qsort1 (filter (< x) xs) ++ [x] ++ qsort1 (filter (> x) xs)
```

`poly` is overloaded because it depends on `(*)` and `(+)`.
```haskell
poly x = x * (x + x)
```

`poly1` is parametric because it depends on the parameters `times` and `plus`.
```haskell
poly1 (times, plus) x = times x (plus x x)
```

## 7.3.1 Intuition
*(p. 157) Full implementation of `poly2` which starts on p. 157*
```haskell
-- Dictionary type
data NumDict a = MkNumDict (a->a->a) (a->a->a) 

-- The book didn't implement intTimes, intPlus, 
-- floatTimes and floatPlus, but here they are:
intTimes :: Int -> Int -> Int
intTimes x y = x * y

intPlus :: Int -> Int -> Int
intPlus x y = x * y

floatTimes :: Float -> Float -> Float
floatTimes x y = x * y

floatPlus :: Float -> Float -> Float
floatPlus x y = x + y

intDict :: NumDict Int
intDict = MkNumDict intTimes intPlus

floatDict :: NumDict Float
floatDict = MkNumDict floatTimes floatPlus

-- Accessor functions
getTimes :: NumDict a -> (a->a->a)
getTimes (MkNumDict times plus) = times

getPlus :: NumDict a -> (a->a->a)
getPlus (MkNumDict times plus) = plus

-- Dictionary-passing style
poly2 :: NumDict a -> a -> a
poly2 dict x =  let  times = getTimes dict
                     plus = getPlus dict
                in times x (plus x x)

-- call them
anInt :: Int
anInt = poly1 (intTimes, intPlus) 10

aFloat :: Float
aFloat = poly1 (floatTimes, floatPlus) 23.5
```


## Haskell #type-class system / mechanism
(p. 158)

Haskell #type-class mechanism has 3 components:
1. type class declarations
2. type class instance declarations
3. qualified types

### 7.3.2 #type-class-declarations
*(p. 158)*

A #type-class-declaration defines a set of operations and their types and gives the set a name.

The `Eq` class has two methods with their types.
```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

> Nothing in Haskell "requires"  or "enforces" the above.


### 7.3.3 #instance-declarations

A type class #instance-declaration makes a type an instance of a #type-class by **defining the operations** of the #type-class for the given type. 

The below makes `Int` ans instance of the `Eq` #type-class:
```haskell
instance Eq Int where
  i == j = int eq i j
  i /= j = not (int_eq i j)
```

This is the **implementation** of the methods defined in the #type-class. Note that the instance has both of the methods (`==`, `/=`) defined in the `Eq` #type-class.

Example `instance Num Int`
```haskell
instance Num Int where
  (*) = int_times
  (+) = int_plus
  negate x = int negate x
  ... <other numeric operations> ...
```

When processing an instance declaration, the compiler ensures that the types of the given definitions match the declared types in the associated type class after replacing the type variable in the class declaration with the type given in the instance declaration. For example, the compiler checks that the type of int times is equal to the type a -> a -> a when a is replaced by Int.

>[!WARNING] Code appears to be old

Looking at the current GHC source code for `Num Int` it looks like this
```haskell
-- | @since 2.01
instance Num Int where
    I# x + I# y = I# (x +# y)
    I# x - I# y = I# (x -# y)
    negate (I# x) = I# (negateInt# x)
    ...
```

- There is no `int_times` function in the source code and this snake style is discouraged by Haskell Language Server.
- `I#`  is links to Hackage modules. Not sure if that is functional or just reference. 

### 7.3.4 #qualified-types

#qualified-types  concisely express the operations required to convert an overloaded function into a polymorphic one. For example, the type of the member function is a qualified type:
```haskell
member :: Eq t => t -> [t] -> Bool
```
The `Eq t => ` makes it qualified.
- Is this the same as a "constraint"?

### 7.4 COMPILING #type-class(es)

Using the information provided by type class declarations, instance declarations, and qualified types, the Haskell compiler automates the rewriting process we went through by hand in 7.3.1. *(p. 161)*

From a type class declaration, the Haskell compiler generates a new [A] dictionary type and corresponding [B] accessor functions.

Some of the code from 7.3.1
```haskell
data NumDict a = MkNumDict (a->a->a) (a->a->a)  -- [A]

intDict :: NumDict Int                  -- [C]? is a dictionary
intDict = MkNumDict intTimes intPlus    --    value?

floatDict :: NumDict Float              -- [C]? with same note
floatDict = MkNumDict floatTimes floatPlus

-- Accessor functions
getTimes :: NumDict a -> (a->a->a)      -- [B]
getTimes (MkNumDict times plus) = times

getPlus :: NumDict a -> (a->a->a)       -- [B]
getPlus (MkNumDict times plus) = plus
```

From a type class instance declaration, the Haskell compiler generates a [C] dictionary value.

>[!WARNING] I stopped taking notes for this section

### 7.7 #type-inference

In the example the book gives it really isn't mysterious how type inference figures things out;

**Example**
```haskell
example :: Ord a => a -> [a] -> Bool
example z xs =
  case xs of
    [] -> False
    (y:ys) -> y > z || (y==z && ys == [z])
```

- `y > z` gives rise to the need for `Ord`
- `y==z` gives rise to the need for `Eq` as does `ys == [z]` which is `Eq [a]` but I suspect is covered by `Eq a`.
- While `Eq` is required it is not shown because `Eq` is a superclass of `Ord`.

### 7.9 Type Classes vs. Object-Oriented Classes

#### Similarities

| Type Class | Object |
| - | - |
| A type class defines a collection of method names and associated types. | Are similar to interfaces in Java |
| A type class can define default implementations for some methods. | Similar to abstract classes in C++ |
|  Instance declarations specify the implementations of the methods of a type class | Like a class that implements an interface |
|  The collection of methods is gathered into a dictionary structure |  is similar to a method table in an object-oriented language |
| When a method is invoked, the type class code looks up the appropriate method in a dictionary |  the object-oriented program looks up the method in a method suite |

#### Differences

| Type Class | Object-Oriented |
| - | - |
|Is **static**: The static type of the arguments and the expected return type of the method is used to select the appropriate dictionary at compile time | Is **dynamic**: The method suite is associated with objects at run-time, and the appropriate method definition is selected based on the dynamic type of the receiver object |
|  The appropriate method to invoke is determined by all the arguments to the method as well as its result type. |  Use only the receiver object to select the method body to run |
|  existing types can be made instances of new type classes by adding new instance declarations. |  typically require a class to specify the interfaces it implements and the abstract superclasses from which it inherits when the class is declared |
|  based on parametric polymorphism and do not require subtyping | make heavy use of subtyping but have only recently started to incorporate parametric polymorphism | 

