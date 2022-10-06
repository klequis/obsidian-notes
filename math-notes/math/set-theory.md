
## Notation

Set theory begins with a fundamental binary relation between an object $o$ and a set $A$.

- $o \in A$ - $o$ is a member/element of $A$
- $A \subseteq B$  - If all members of set $A$ are also members of set $B$, then $A$ is a subset of $B$. (aka *inclusion*)

- A set can be a member of a set
- A set can be a set of itself. When this is unsusitable the term *proper subset* is defined.

## Proper subset

A is a proper subset of B if and only if 
- $A$ is a subset of $B$ but $A$ is not equal to $B$
- 1, 2, & 3 are members of set $\{1,2,3\}$, but are not subsets of it.
- Subsets such as $\{1\}$ are not members of set $\{1,2,3\}$

## Binary operations

### Union

$A \cup B$ - "union of $A$ & $B$".

![](https://upload.wikimedia.org/wikipedia/commons/thumb/3/30/Venn0111.svg/200px-Venn0111.svg.png)

- The set of all objects that are a member of $A$, $B$ or both.

let $A = \{1,2,3\}$
let $B = \{2,3,4\}$
then $A \cup B = \{1,2,3,4\}$

### Intersection

$A \cap B$ - "intersection of $A$ & $B$"



![[intersection.png|225]]

- The set of all objects that are members of both $A$ and $B$.

let $A = \{1,2,3\}$
let $B = \{2,3,4\}$
then $A \cap B = \{2,3\}$

### Set difference
> aka: Relative complement, disjunctive union

$U \setminus A$ - "The difference set of $A$ & $B$"
- The set of all members of $U$ that are not members of $A$".

$\{1,2,3\} \setminus \{2,3,4\} = \{1\}$

$\{2,3,4\} \setminus  \{1,2,3\} = \{1\}$

## Compliment 

When $A$ is a subset of $U$, the set difference $U \setminus A$ is also called the **compliment**.

- When $A$ is a subset of $U$
- the set difference $U \setminus A$ 
- is also called the complement $A$ in $U$
- If the choice of $U$ is clear from the context, the notation $A^C$  is sometimes used.

**Example**
Assume that the universe is the set of integers. If A is the set of odd numbers, then the complement of A is the set of even numbers.


### Symetric difference

$A \bigtriangleup B$ or $A \circleddash B$ - The symmetric difference of sets $A$ and $B$

![](https://upload.wikimedia.org/wikipedia/commons/thumb/4/46/Venn0110.svg/220px-Venn0110.svg.png)

- 1. The set of all objects that are a member of exactly one of $A$ and $B$. That is, elements that are in one set but not in both.
- 2. The set of elements which are in either of the sets but not in their intersection.

$A = \{1,2,3\}$
$B = \{2,3,4\}$
$A \bigtriangleup B = \{1, 4\}$



It is the set difference of the union and the intersection, 
$(A \cup B) \setminus (A \cap B)$ or $(A \setminus B) \cup (B \setminus A)$

$(A \cup B) \setminus (A \cap B)$
(A union B) diff (A intersection B)
$\{1,2,3,4\}  \setminus  \{2, 3\} = \{1,4\}$

$(A \setminus B) \cup (B \setminus A)$
(A diff B) union (B dff A)
$\{1,2,3\}\setminus \{2,3,4\}$ union $\{2,3,4\} \setminus \{1,2,3\}$
$\{1,4\}$ union $\{1,4\}$ = $\{1,4\}$







## Needs verification

- **Disjoint sets:** are those sets whose intersection with each other results $\emptyset$.
- 












