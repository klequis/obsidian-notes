# Operator associativity
#operator-associativity #associative #left-associative #right-associative #non-associative

**Associativity** of an operator is a property that determines how operators of the same precedence are grouped in the absence of parentheses.

If an operand is both preceded and followed by operators (e.g., $\textasciicircum \hspace{.25cm} 3 \hspace{.25cm} \textasciicircum$), and those operators have equal precedence, then the operand may be used as input to two different operations. 

The choice of which operations to appy to the operand is determined by the associativity operators.

- **associative:** can be grouped arbitrarily
- **left-associative:** operations are grouped from the left
- **right-associative:** operations are grouped from the right
- **non-associative:** the operations cannot be chained.

Consider an operator $\sim$ and the expression $a \sim b \sim c$.

- If $\sim$ is left associative: $(a \sim b) \sim c$
- If $\sim$ is right associative: $a \sim (b \sim c)$

* Subtraction & devision are left associative
* Addition and multiplication are both left & right associative.

Unlike associativity in math, associativity in programming languages must always be left-, right-, or non-associative.



