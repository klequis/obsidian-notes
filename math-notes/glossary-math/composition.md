# Composition (math)

> Not sure if this is correct yet.

- Morphisms are equipped with a partial binary operation called **composition**.
- The composition of two morphisms $f$ and $g$ is defined precisely when the target of $f$ is the source of $g$.
	- This is denoted as $f \cdot g$.
	


Hey, that makes sense now if you think of the sources as input and target as output ...
- source -> $g \cdot f$ -> target
But I'm not totally sure that is what it means.

---

$f \cdot g$

- target of $f$ is the source of $g$
- the source of $g$ ∘ $f$ is the source of $f$, 
- the target of $g$ ∘ $f$ is the target of $g$


- The source of $g \cdot f$ is the source of $f$ 
- The target of $g \cdot f$ is the target of $g$

$target \leftarrow f \leftarrow source \leftarrow target \leftarrow g \leftarrow source$

---

$g \cdot f$ is the composition of $g$ and $f$.
If given a parameter $x$ it could be written g(f(x))
So $f$ will execute on $x$ first.
The result of $f(x)$ will be passed to $g$.
$g$ will execute and return the final result.

$f (x) = x + 1$
$g( x) = x \times 2$

Let's make that actual code:

```js
const f = x => x + 1
const g = x => x * 2
```

> Do not that it doesn't matter what the variable name is in `g()`. We could have used a name other than `x`.

So that leaves us with 
```js
const x = 1
const a = g(f(x))
```

f(x) -> 2 -> g(2) -> 4



---

Changing the example from above.

$f (x) = x + 1$
$g( x) = x + 2$

A = $\{1,2,3\}$



$f \cdot g$ = 

g of A = {3,4,5}
f of g of A = {4,5,6}

$g \cdot f$ = 

f of A = {2,3,4}
g of f of A = {4,5,6}




---

I'm struggling with this paragraph:

Morphisms are equipped with a [partial binary operation](https://en.wikipedia.org/wiki/Partial_operation "Partial operation"), called _composition_. The composition of two morphisms $f$ and $g$ is defined precisely when the target of $f$ is the source of $g$, and is denoted $g$ ∘ $f$ (or sometimes simply _gf_). The source of $g \cdot f$ is the source of $f$, and the target of $g$ ∘ $f$ is the target of $g$.

As I understand it "source" is input and "target" is output. So this makes not sense

"the target of $f$ is the source of $g$"

How can the output of f

---

I'm a programmer but not a math person. I'm learning category & set theory. I have run into the below in a Wikipedia article that seems backward to me and want to get some help understanding it. The full article is [here](https://en.wikipedia.org/wiki/Morphism) The paragraph I'm having trouble with is:

"Morphisms are equipped with a [partial binary operation](https://en.wikipedia.org/wiki/Partial_operation "Partial operation"), called _composition_. The composition of two morphisms $f$ and $g$ is defined precisely when the target of $f$ is the source of $g$, and is denoted $g \cdot f$ (or sometimes simply $gf$). The source of $g$ ∘ $f$ is the source of $f$, and the target of $g$ ∘ $f$ is the target of $g$."

I'm not understanding "the target of $f$ is the source of $g$"

As understand things 

- $g \cdot f$ is $h = g(f(x))$
- $x$ is the source (input) of $f$ 
- the target of $f$ (output) becomes the source (input) of $g$
- and the target (output) of $f$ is the final result

So as I understand things

- "the target of $f$ is the source of $g$"

has $f$ & $g$ reversed, and should be

- "the target of $g$ is the source of $f$"

---

**I spent 1.5 hours thinking this statement was backward.**

The composition of two morphisms $f$ and $g$ is defined precisely when the target of $f$ is the source of $g$, and is denoted $g \cdot f$ 


**Until I finally rewrote it like this**

The composition of two morphisms f and g (denoted g⋅f) is defined precisely when the target of f is the source of g.

**That is fustrating, if not down right silly**