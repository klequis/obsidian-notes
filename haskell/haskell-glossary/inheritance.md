# inheritance

## Quotes
- **Type class inheritance** is when a type class has a superclass. (HPFFP p.214)
- Type class inheritance extends downward from a superclass, such as `Num`, to subclasses, such as `Integral` and then `Int` but not the other way around (HPFFP p. 141).
- A subclass cannot override the methods of its superclass. (HPFFP p. 141).

### Summation

- Type class inheritance extends downward only from superclass to subclass.
- A subclass cannot override the methods of its superclass.

## Type class inheritance and instances

If a class has superclass(s) and you want to write a instance of the the class

If a type class has a superclass you can not create an instance of the type class for a type unless the type also has an instance of the superclass.

**Example**

```haskell
-- class Num a => Fractional a where

data Type1 = Type1 Double 

instance Fractional Type1 where
  (Type1 x) / (Type1 y) = Type1 (x / y)
  recip (Type1 n) = Type1 (recip n)
  fromRational r = Type1 (fromRational r)
```

 This results in the error
```
  • No instance for (Num Type1) arising from the 
    superclasses of an instance declaration
```

To solve it, create an instance of `Num Type1`
```haskell
instance Num Type1 where
  (+) (Type1 a) (Type1 b) = Type1 (a + b)
  -- ... add implementations for
  -- *, abs, signum, etc.
```


