
## 6 kinds of names/namespaces in Haskell

- Names for variables and constructors denote values.
- Names for type variables, type constructors and type classes refer to entities related to the type system.
- Names for modules refer to modules

| Example | Meaning |
| ------- | - |
| [pattern] | optional |
| {pattern} | zero or more repetitions |
| (pattern) | grouping |
| pat1 \| pat2 | choice |
| pat⟨pat′⟩ | difference—elements generated by pat except those generated by pat′ |
| fibonacci | terminal syntax in typewriter font  |

## Identifiers & Operators

- Identifiers are lexically distinguished into two namespaces
    - those that begin with a lowercase letter (**variable identifiers**) 
    - those that begin with an upper-case letter (**constructor identifiers**).

### Abbreviations

varid	    	    (variables)
conid	    	    (constructors)
tyvar	  →	  varid	                    (type variables)
tycon	  → 	conid	                    (type constructors)
tycls	  →	  conid	                    (type classes)
modid	→	  {conid .} conid	    (modules)


| This |	Lexes as this |
| - | - |
| f.g	| f . g (three tokens) |
| F.g	| F.g (qualified ‘g’) |
| f..	| f .. (two tokens) |
| F..	| F.. (qualified ‘.’) |
| F.	| F . (two tokens) |



## Two names spaces for operator symbols

- An operator symbol starting with a colon is a **constructor**.
- An operator symbol starting with any other character is an **ordinary identifier**.


---

## 3.5 Sections

- Sections are a convenient syntax for partial application of binary operators.

```haskell
(*a+b)    -- invalid
(+a*b)    -- valid
(*(a+b))  -- valid
```
