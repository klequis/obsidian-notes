# Relations

> source: https://courses.engr.illinois.edu/cs173/fa2010/Lectures/relations.pdf


A "relation $R$ on as set $A$" is a subset of $A \times A$. $R$ is a set of ordered pairs of elements from $A$.

## Properties of Relations
- [[reflexivity]]:
	- reflective: every element is related to itself
	- irreflective: no element is related to itself
	- neither: some elements are related to themselves but some are not
- [[symmetric-and-antisymmetric|symmetric / antisymmetric]]
	- symmetric: elements are related in both directions
	- antisymmetric: elements are only related in one direction
- [[transitivity|transitive]]
	- for all $a,b,c \in A$, $aRb$ and $bRc$ implies that $aRc$.
	- e.g., if $x<y$ and $y<z$, then $x<z$.

## Types of relations

- equivalence relationa: a relation that is reflective, symmetric and transitive.
- partial order: a relation that is reflective, antisymmetric and transitive.
- strict partial order: a relation that is irreflective, antisymmetric and transitive.


- Equivalence relations act like equality
- Partial orders act like $\leq$ and $\geq$.
- Strict partial orders act like $\gt$ or $\lt$.

## Partitions

A **partition** of $A$ is a collection of non-empty subsets of $A$ which cover all of $A$ and don't overlap. Subsets in a partition must satisfy three conditions:

1. $A_1 \cup A_2 \cup ... \cup A_n = A$
2. $A_i \neq \emptyset$ for all $i$
3. $A_i \cap A_j = \emptyset $ for all $i \neq j$

