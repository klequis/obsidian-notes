# #overloaded-string-literals, #overloaded-strings

*source: [Overloaded string literals](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html)*

- Normally a string literal has type String, 
- but with overloaded string literals enabled via `OverloadedStrings`, a string literal has type `(IsString a) => a`.

