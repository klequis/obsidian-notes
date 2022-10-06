#### Types
-  **type classes**
- **type class instances**
- **datatypes**





#### Statements
- The declaration of a type defines ho that type is created
- The declaration of a type class defines how a set of types are consumed / used in computations.
- Type classes allow us to generalize over a set of types in order to define and execute a standard set of features for those types.
    - E.g., equality will work with any type that implements the type class `Eq`
- "Type has *instance* of *class x*": means that there is code that defines how the values and functions from that type class work for that type.
-  When you use a **type class method** with one of the types that has **such an instance**, the compiler looks up the code that dictates how the function works for that type.
      - "type class method"
      - `instance Eq MyType where
          (==) = ... `
- "instances" are type classes that are implemented for a type
      - `instance Eq Bool ...`
- 

#### Lesson 19, p. 214 
Unlike List or Map, which represent containers for values, Maybe is the first of many types you’ll see that represents a context for a value.


#### Lesson 21, p. 250
Maybe is a parameterized type (a type that takes another type as an argument) that represents a context when a value may be missing.













# Q

- how does > pure 6 work

- Is applicative by itself a context

- Can a class have an instance of Applicative i.e., Applicative Num - NO?
  - if not, how does pure (+1) work?
  
  List is a context and a context
  
  Can be one or other or both
