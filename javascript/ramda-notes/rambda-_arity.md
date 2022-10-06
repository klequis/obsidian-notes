#ramda, #functional #ramda-arity

# _arity

> Examined in the context of #ramda-pipe 

`pipe`

```js
export default function pipe() {

...

log('arguments[0].length', arguments[0].length)
log('arguments', arguments)
return _arity(
	arguments[0].length,
	reduce(_pipe, arguments[0], tail(arguments))
);

}
```



Example Call

```js
const fn0 = (x, y) => x + y
const fn1 = x => x + 1
const fn2 = x => x + 2
const fn3 = x => x + 3
const a = pipe(fn0, fn1, fn2, fn3)(1, 2)

log('a', a)
```

## `arguments` && `_arity`
In this example the arguments to `pipe` are:

```js
{
  '0': [Function: fn0],
  '1': [Function: fn1],
  '2': [Function: fn2],
  '3': [Function: fn3]
}
```

`pipe` sends the number of parameters in the first function `fn0` and `fn0` itself to `_arity` which returns:

```js
function (a0, a1) { return fn.apply(this, arguments) }
```

In this case `fn` in `fn.apply(...)` is `fn0`.


From Ramda doc on `pipe`:

Performs left-to-right function composition. The first argument may have any arity; the remaining arguments must be unary.

Although it says 'any arity', the truth is `_arity` limits this to a maxium of 10. Also, not only do 'the remaining arguments must be unary' but they have to be since a function can only return one value, all functions after the first will receive one value. However, that one value can be a complex object which effectively allows you to return/send multiple values.

## `tail`






