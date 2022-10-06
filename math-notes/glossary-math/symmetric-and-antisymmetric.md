# Symmetric & Antisymmetric

#symmetric #antisymmetric

source: https://courses.engr.illinois.edu/cs173/fa2010/Lectures/relations.pdf

> "Does the order matter within each pair?"
> Aka: if $(x,y)$ is in $R$, is $(y,x)$ in R?


**Symmetric**: A relation where both $(x,y)$ & $(y,x)$ are in $R$.

- Equality ($=$) is generally **symmetric**.
- Relations that put elements into an order such as $\leq$ or $\div$ are **antisymmetric**.
- A relation is **antisymmetric** if two distinct elements are never related in both directions.

Formal definition
- symmetric: for all $x,y \in A$, $xRy$ implies $yRx$.
- antisymmetric:
	- for all $x$ and $y$ in $A$ with $x \neq y$ $xRy$ implies $y \cancel{R}x$.
	- for all $x$ and $y$ in $A$, $xRy$ and $yRx$ implies $x=y$.
	- 


---

## Other explanation

For any $x \in A$ and $y \in A$
- If $xRy$ 
- and $y \neq x$ 
- then $y\cancel{R}x$.

For any $x \in A$ and $y \in A$
- if $xRy$
- and $yRx$
- then $x = y$

Note that all the arrows in the below diagram go only one way.

![[Pasted image 20220215104838.png]]

