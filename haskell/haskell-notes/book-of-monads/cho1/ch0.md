
```haskell  
class Eq a where
  (==) :: a -> a -> Bool
```
- `a` refers to any possible instance of that variable in the methods.
- `==` not only requires its arguments to support equality, but they must be of the same #type-class.

```haskell
eqList :: Eq a => [a] -> [a] -> Bool
eqList [] [] = True
eqList (x:xs) (y:ys) = x == y && eqList xs ys
eqList = False
```
- Lists must be of the same type `[a]`
- The type inside the lists must support equality (have an `Eq` instance).

```haskell
instance Eq Bool where
  True  == True  = True
  False == False = True
  _     == _     = False
```
- Type variable, `a`, in the class is replaced with the actual type, `Bool`.
- Instead of the type signature of methods as are in the type class, there is the actual implementation.

Sometimes an instance can depend on another instance. `eqList` depends on:
```haskell
instance Eq a => Eq [a] where(==) = eqList
```

--- 

(p. 6) The definition of a Container type class in Haskell does not obviously reflect that we are abstracting over a type constructor. The only way we can notice this fact is by observing how the variable `c` is used — in this case, **`c` is applied to yet another type `a`, which means that `c` must be a type constructor**:
```haskell
class Container c where
  empty : c a
  insert :: a -> c a -> c a
```