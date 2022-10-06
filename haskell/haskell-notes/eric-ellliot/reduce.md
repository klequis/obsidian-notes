#reduce #eric-elliot

# Reduce

- Also know as fold and accumulate
- Allows interating over a list, applying a function to an accumulated value and the next item in the list

Reduce takes a "reducer function" & an "initial value" and returns the "accumulated value".

He also calls the "reducer function" "the reducer"

Here is a "reducing function":

 ```js
 const summingReducer = (acc, n) => acc + n;
 ```

 And it is used in `reduce` like this:

 ```js
[2, 4, 6].reduce(summingReducer, 0)
 ```

 - `reduce` iterates through the collection left to right, but there is also `reduceRight` which iterates the collection righ to left.
 - You can define `map`, `filter`, `forEach` and lots of other common functions with `reduce`.

## Compose

`f . g` or `f(g(x))`.

You could define multiple functions such as `(f(g(h(x)))`.

But, for a reason not explained, you need to run `reduce` in revers useing `reduceRight`. Why?

Given `(f(g(h(x)))` `h` executes first, then `g` & finally `f`. So what if we sent a list of functions to reduce with `x` as the initial value.

```js
R.compose(

)[f, g, h]
```

That makes no sense, the functions need to go within compose, but then it would no longer be a list of functions.

Here is Eric Elliots compose function

```js
const compose = (...fns) => x =>
  fns.reduceRight((v, f) => f(v), x)
```

If I pass this `[f, g, h]` what will happen?

Three steps
1. `h` will be applied to `x`
2. `g` will be applied to the result of step 1
3. `f` will be applied to the result of step 2


Hum... So if you used `reduce` instead of `reduceRight` then rather than the execution order being `h -> g -> f` it would be `f -> g -> h`.

In many cases the order matters:

3 x 2 + 1 = 7

1 + 2 x 3 = 7

That works in math but if the order of operations is dictated by the ordering of functions in an array the answers will not be the same:

(3 x 2) = 6
(6 + 1) = 7

vs

(1 + 2) = 3
(3 x 3) = 9

oops.

**How did we get here?**

One thing that occurs to me is that function composition does't know nor implement the rules of math. That is, it doesn't know you must perform multiplication before addition and it doesn't allow you to specify that.

If you think about it, all we have is an order of functions `[f, g, h]` or `[h, g, f]`. This issue is not only with math rules. For example:

> It would be nice to have an example showing that order matters but don't want to do that now.




<div style="width:100%;height:0;padding-bottom:75%;position:relative;"><iframe src="https://giphy.com/embed/PDenglwEXySNTOOlYK" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/pokemon-anime-face-ditto-PDenglwEXySNTOOlYK">via GIPHY</a></p>

## Composition in Category Theory

```
1  g(a) -> b
2  f(b) -> c
3  h(x) = f(g(x))
```


If you have 1 and 2 there must also exist 3

Is a JS array a functor?

```js
const f = [1, 2, 3]

F.map(x => f(g(x)))

// is equivalent to

F.map(g).map(f)
```

In both cases `g` will process `x` then `f` will process its result.

### Endofunctors

#endofunctor

Wow, that is a scary word!

A *functor* can map from category to category: `X -> Y`.
An *endofunctor* can map from category to the same category: `X -> X`.

