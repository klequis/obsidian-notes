Assumptions
1. GHC doesn't know it is the second argument to `showbPrec`
2. It seems know that the ambiguous type variable is in the `showPrec`  function.


Ambiguous type variable in the function `showbPrec` prevents the constraint `TextShow a` from being solved.



"Arising from the use of `showbPrec`"

So the ATV is not in the function `showbPrec`, it is in the type of `showbPrec`