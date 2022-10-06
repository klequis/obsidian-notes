# #lift #lifted

> [!WARNING] Just reiterating what is in the book
> HFF p. 466

- kind * is the kind of all standard lifted types
- types of kind # are unlifted
* Any data type you create yourself is lifted.
* A lifted type is any type that can be inhabited by *bottom*
* An unlifted type is any types that *cannot* be inhabited by bottom.
*  Lifted types are represented by a pointer and include most of the datatypes we’ve seen and most that you’re likely to encounter and use.

Types of type # are often native machine types and raw pointers.

**Newtypes**

Newtypes are a special case in that they are kind *, but they are unlifted, because their representation is identical to that of the type they contain, so a newtype itself is not creating any new pointer beyond that of the type it contains. 

That fact means that the newtype itself cannot be inhabited by bottom—only the thing it contains can be—so newtypes are unlifted.