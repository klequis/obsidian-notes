# Ch 4 Haskell development with modules, packages and projects

- Haskell code (data types, function definitions, etc) is organized into modules.
- `Prelude` is imported into every module by default.
	- A subset of `base` (which implements the language report).
- Haskell packages are on Hackage (https://hackage.haskell.org)
	- Not all packages on Hackage work together
- Stackage (https://www.stackage.org) has sets of Hackage packages that work together.

## modules & packages
- module: 
	- a tool for namespace control
	- a collection of functions, types, classes, etc.
	- A module can import definitions from other modules and export some of its definitions.
	- A module's name begin with an uppercase letter and resides in a `.hs` file.
- package
	- a unit of distribution
	- may be a
		- libraries (exports intended to be used by other packages)
		- Stand-alone applications
		- both
- Packages are organized into projects


## Importing
- keywords / modifiers
	- `qualified`: only imports identifers with fully qualifed name `Module.identifierName`
	- `hiding`: hide specified modifiers
		- `import Prelude hiding (head)`
	- `as`: import with an alias
		- `import Mod as Foo`

> Good resource on importing
> https://wiki.haskell.org/Import

## Exporting

```haskell
module ModuleName (
    -- export entire module
	-- reexport ?
	Module X,
	-- export type constructor only
	DataType1,
	-- export type constructor and all data
	-- constructors
	DataType2 (..),
	-- export type constructor & data constuctors
	-- Cons1 & Cons2 *only*.
	DataType3 (Cons1, Cons2),
	-- Type classes follow data type pattern
	
	-- export functions
	fn1, fn2, fn3
) where

```


**`MyModule.hs`**
```haskell
module MyModule (someFunction) where

someFunction :: p -> p
someFunction x = x
```


> A module’s name should begin with an uppercase letter (for example, Fmt ). It is expected to reside in the .hs file with the same name.
> 
> While the book says "should" and "is expected", in my testing:
> - The module name **must** start with an uppercase letter
> - The file name must match the module name and is **case sensitive**.
> 
> - The entry point `main` is an exception to this

**`main.hs`**
```haskell
module Main where

import MyModule (someFunction)

main :: IO ()

name :: [Char]
name = someFunction "Joe"

main = do
	let str = "hello " ++ name
	putStrLn str
```


## Module Hierachy & Data Structure

- A module is always imported by its full name (fully qualified name) regardless of where it is imported from.
- You can only import individual modules (which are files), not a directory of them.
- GHC will search down the directory tree but not up unless you launch `ghci` with `-i..`
	- Question: is `-i..` a `ghci` flag only or can you use it in the build process?