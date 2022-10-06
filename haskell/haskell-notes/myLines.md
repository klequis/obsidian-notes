**goal:** convert string with “\n” into a [String] of lines

## 1. get location of each \n

**fn:** `locateNewLines`
**input:** `"abcd\nefghijk\nlmnopqrst\nuvwxyz"`
**output:** `[4,12,22]`

## 2. add start & end to indices
**fn:** `addStartEnd`
**input:** `"abcd\nefghijk\nlmnopqrst\nuvwxyz" [4,12,22]`
**output:** `[0, 4,12,22,29]`

Where
- Start is always 0 so pre-pend 0
- End is always the length of the string so append `length xs`, which in this example is 29.

## 3. a dups of all but first and last number

**fn:** `dupNums`
**input:** `[0, 4,12,22,29]`
**output:** `[0,4,4,12,12,22,22,29]`

## 4. make list with dups a list of tuples

**fn:** `toTupes`
**input:** `[0,4,4,12,12,22,22,29]`
**output:** `[(0,4),(4,12),(12,22),(22,29)]`

## 5. calculate diff of each tuple

**fn:** `calcLengths`
**input:** `[(0,4),(4,12),(12,22),(22,29)]`
**output:**  `[4,7,9,6]`

## 6. calculate start and length for each line

Take indicies and diffs


calcStarts


starts = [0,5,13,23]
indices = [4,12,22]

starts = [0, indice + 1..., length]

**fn:** `calcStarts`
**input:** indices, alpha
**output:** [0,5,13,23]


-- 



## bla

### Expected

**fn:** `substr`
**input:** `Int Int Text`
  - `Int`: the position to start at. Is zero based.
  - `Int`: the number of characters to get
  - `Text`: any text
**input:** `5 7 "abcd\nefghijk\nlmnopqrst\nuvwxyz"`
**output:** `"efghijk"`

## 7. cut-up the string

**fn:** `cutText`
**input:** 
