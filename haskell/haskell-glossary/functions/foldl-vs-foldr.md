# `foldl` vs `foldr`

## Some rules for folds

- `foldr` is usually the correct fold to use.
- `foldr` can short-circuit, `foldl` cannot.

Use `foldl'` when
- Using a large but finite list and the function used is commutative (e.g., `(+)`, `(*)`) and you seek to improve performance.
- When you actually want to reverse the order of the list in addition to whatever transformation you are performing.
- If you procede or follow a fold with a reverse, you likely could use `foldl`.

## Some facts
- `foldr` is more often efficient for lazy evaluation
- `foldr` can be used with infinite lists. `foldl` cannot
- `foldl` with an infinite list will never terminate

```
foldl (-) 0 [1..10] = -55

(((((((((0-1)-2)-3)-4)-5)-6)-7)-8)-9)-10

foldr (-) 0 [1..10] = -5

1-(2-(3-(4-(5-(6-(7-(8-(9-(10 - 0)))))))))
```

```
left:   ((0-1)-2)-3
-6

((0-1)-2)-3
(-1 - 2)-3
-3 - 3
-6
```

```
right: (1-(2-(3-0)))
2

(1-(2-(3-0)))
(1-(2-3))
(1-(-1)
2

```

---

## Doing it step by step

`foldl (-) 0 [1..10] = -55`

```haskell
a = (((((((((0-1)-2)-3)-4)-5)-6)-7)-8)-9)-10

b = (((((((((-1)-2)-3)-4)-5)-6)-7)-8)-9)-10

c = ((((((((-3)-3)-4)-5)-6)-7)-8)-9)-10

d = (((((((-6)-4)-5)-6)-7)-8)-9)-10

e = ((((((-10)-5)-6)-7)-8)-9)-10

f = (((((-15)-6)-7)-8)-9)-10

g = ((((-21)-7)-8)-9)-10

h = (((-28)-8)-9)-10

i = ((-36)-9)-10

j = (-45)-10

k = -55
```


`foldr (-) 0 [1..10] = -5`

```haskell
a = 1-(2-(3-(4-(5-(6-(7-(8-(9-(10 - 0)))))))))
b = 1-(2-(3-(4-(5-(6-(7-(8-(9-(10)))))))))
c = 1-(2-(3-(4-(5-(6-(7-(8-(-1))))))))
d = 1-(2-(3-(4-(5-(6-(7-(9)))))))
e = 1-(2-(3-(4-(5-(6-(-2))))))
f = 1-(2-(3-(4-(5-(8)))))
g = 1-(2-(3-(4-(-3))))
i = 1-(2-(3-(7)))
j = 1-(2-(-4))
k = 1-(6)
l = -5
```
