#functional #transduce 

souce: https://tgvashworth.com/2014/08/31/csp-and-transducers.html

> transform + reduce = transduce

**Note**: The article uses the terms "result" & "input" for "accumulator" & "currentVal". 

| Article's name | New Name | Description |
| -------------- | -------- | ----------- |
| result | acc | accumulator |
| input | cur | currentValue |
| seed | initVal | initialValue |



## Call tracing on final

```js

main(concat)
	f(lt3)
		return f1(reduce)
	m(inc)
		return m1(reduce)
	m1(concat)
		return m2(acc, cur)
	f1(m2)
		return f2(acc, cur)
	f2([], 1)
		m2([], 1)
			concat([], 2)
				return [2]
			return [2]
		return [2]
	f2([2], 2)
		m2([2], 2)
			concat([2], 3)
				return [2, 3]
			return [2, 3]
		return [2, 3]
	f2([2, 3], 3)
		return [2, 3]
	f2([2, 3], 4)
		return [2, 3]

```



## Final

Let's talk about the end before getting to the beginning.

In this article there are two types of transformers "mapping" & "filtering".

- mapping takes a "transformer" function
- filtering takes a "predicate" funciton
- A transformer function takes two parameters `acc` & `cur` .
- Both `mapping` & `filtering` functions need a combining function. 
	- %%IS THIS ALSO CALLED A "REDUCING FUNCTION"? %%
	- Its role is to combine the result & input. 
	- Only the combing function know how to merge the input into the result


## It all begins with `reduce`

[MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce#description) "... `reduce` calls the callback, as a function, once for each element after the first element present in the array..."

The key idea here is that `reduce`  calls the callback **once** for each element in the array. This is the main benefit of transducers: When you want to perform multiple transformations on a data structure you can either loop over the data structure once for each transformation (like chaining) or once. %%THIS COULD USE A DIAGRAM%%


## Examining `mapping`

```js
function mapping(transform) {
    return function (reduce) {
        return function (acc, cur) {
            return reduce(acc, transform(cur));
        };
    };
}
```

* `mapping` takes a `transform` which is a function that when applied to `cur` transforms it. `transform` could be as simple as `x => x + 1`. So if `cur` is 1 it will become 2.
- It then returns a function  that takes a `reduce` function. The job of the `reduce` function is to combine `acc` & `cur` into one value and return it.


## An example

```js
function mapping(transform) {
    return function (reduce) {
        return function (acc, cur) {
            return reduce(acc, transform(cur));
        };
    };
}

const lessThanThree = x => x < 3

const arr = [1, 2, 3, 4, 5]

arr.reduce(mapping(lessThanThree))
```




## Terms
- **accumulator value**
- **current value**
- **init function**: can be used to provide an initial accumulator (but is ignored by transduce).
- **iterator function**
- **reducing function**: is any function that can be passed to reduce. It takes two params and returns one. It often combines the two inputs but doesn't have to.
	- `(accumulator, input) -> something`.
 - **result function**: is used to convert the final accumulator into the return type
- **step function**: used as the iterator function in reduce.
- **transducer**: 
	1. a function that accepts a transformer and returns a transformer. It can be composed composed directly
	2. In the final example, it is the inner function that takes `reduce` as a parameter. These functions encapsulate some reducing behaviour without caring about the nature of the result data structure.
- **transformer**: an object that provides a 2-arity reducing iterator function, step, 0-arity initial value function, init and 1-arity result extraction function, result.

## Reducing functions

A **reducing function** is any function that can be passed to `reduce`. It's signature is:

```js
(previousValue, currentValue)  => {}
```

[MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce) calls it simply a "reducer".


## Parse Text

### P1

- Initializes a tranducer using supplied iterator function
- Returnes a single item by 
	- iterating through the list, 
	- successively calling the transformed iterator function 
	- and passing it an accumulator value & current value
	- and passes the result to the next call

### P2

- **Iterator** function receives two values `(acc, value)`
- It gets wrapped in a **transformer** to initialize the **transducer**
- A transformer can be passed directly in place of an iterator function

### P3

...

### P4

The interation is performed with `R.reduce` after initializing the **transducer**

`(c → c) → ((a, b) → a) → a → [b] → a`

## Example

```js
import * as R from 'ramda'

const numbers = [1, 2, 3, 4]

const transducer = R.compose(R.map(R.add(1)), R.take(2))

const ex1 = R.transduce(transducer, R.flip(R.append), [], numbers)
```

#### According to [Robbie](https://dev.to/rodw1995/ramdajs-transduce-59c6) 
The paramaters are `(transducer, iterator, initialValue, list)`. So that is:

- transducer: `transducer`
- iterator: `R.flip(R.append)`
- initialValue: `[]`
- list: `numbers`


#### According to [craigdallimore](https://gist.github.com/craigdallimore/8b5b9d9e445bfa1e383c569e458c3e26#also-see)

- "2-arity reducing iterator function" is a **reducer** that takes 2 arguments and returns one of the same as one of the 2 arguments given.


## References
- https://dev.to/rodw1995/ramdajs-transduce-59c6
- https://gist.github.com/craigdallimore/8b5b9d9e445bfa1e383c569e458c3e26
- 
