Original
```haskell
Expr1.hs:73:34: error:
    • Ambiguous type variable ‘a0’ arising from the literal ‘2’
      prevents the constraint ‘(Num a0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instances exist:
        instance Num Integer -- Defined in ‘GHC.Num’
        instance Num Double -- Defined in ‘GHC.Float’
        instance Num Float -- Defined in ‘GHC.Float’
        ...plus two others
        ...plus 76 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the first argument of ‘Lit’, namely ‘2’
      In the first argument of ‘Add’, namely ‘(Lit 2)’
      In the second argument of ‘showbPrec’, namely
        ‘(Add (Lit 2) (Lit 3))’
   |
73 |         $  showbPrec 6 (Add (Lit 2) (Lit 3))
   |                                  ^
```



• Ambiguous type variable ‘a0’ arising **from a use** of ‘showbPrec’ 
- prevents the constraint ‘(TextShow a0)’ from being solved. 

- Probable fix: use a type annotation to specify what ‘a0’ should be.

- These potential instances exist: instance (TextShow a, TextShow b) => TextShow (Either a b) -- Defined in ‘TextShow.Data.Either’ instance TextShow Ordering -- Defined in ‘TextShow.Data.Ord’ **instance TextShow a => TextShow (Expr a) -- Defined at /home/klequis/d/learn/haskell/book/haskell-in-depth/mini-proj/expr/app/Expr1.hs:13:10** ...plus 26 others ...plus 208 
- instances involving out-of-scope types (use -fprint-potential-instances to see them all) 
- • In the first argument of ‘(<>)’, namely ‘showbPrec 6 (Add (Lit 2) (Lit 3))’ 
- In the second argument of ‘($)’, namely ‘showbPrec 6 (Add (Lit 2) (Lit 3)) <> plus <> showbPrec 5 expr2’ 
- In the expression: showbParen (True) $ showbPrec 6 (Add (Lit 2) (Lit 3)) <> plus <> showbPrec 5 expr2
- typecheck(-Wdeferred-type-errors)

## (1)
Why does it say `a0` instead of just `a`. It implies in some cases there can be `a0`, `a1` `...`. If so what is an example of such a case.

## (2)
Partially negates the above.
OK, read both the links provide by @j @j & see I am not alone with this.

## (3)
Being a beginner at Haskell I likely see things differently and perhaps (often actually) incorrectly.

## (4)
I think "Ambiguous type variable" is a very helpful phrase as it is the problem. Should it be the title of the error?
- Is there, or is it possible to categorize all haskell errors?

There are two things I find most helpful in the error message and only came to see them after I carefully parsed out the error message in a text editor.
1. The problem is in the use of `showbPrec`
2. the constraint ‘(TextShow a0)’ from being solved
3. In the first argument of ‘(<>)’, namely ‘showbPrec 6 (Add (Lit 2) (Lit 3))’ 
4. I would swap the position of "These potential instances" with the location of the problem (in the first, in the seconde)
5. It seems known that the 

Fixes
- Call `showbPrec` a "function"
- Could it be that if there is a user-defined function for the Constraint that is likely the one not met (maybe and say it better) "instance TextShow a => TextShow (Expr a) -- Defined at /home/klequis/d/learn/haskell/book/haskell-in-depth/mini-proj/expr/app/Expr1.hs:13:10" or just coincidence for this case.
- "in the first argument" is confusing


--- 


A little bit on how a Haskell beginner with a non-functional, imperative background looks at an error message ((fix that up including "didn't deal with types, no TypeScript, bool a, int b"))

Comments reflect my current level of experience. I may be a good example of some persona leaning Haskell as I am not a computer scientists, have not studied math beyond college calculus and have a long and well ingrained imperative way of thinking about code (mostly VB, C# & JavaScript).

There is a lot of good in the error message

Firstly, `a0` is not a type variable in my code so I'm have been until now mystified by the mention of similar type variables that I can not see since I started learning Haskell. Since the type of `TextShow` is `TextShow a => Int -> a -> Builder` I assume it is referring to the `a` in there but don't know why it

"prevents the constraint ‘(TextShow a0)’ from being solved"
- Still there is `a0` but
- It says the problem is "figuring out how to apply/solve/(or something) ". I think that is a key point and