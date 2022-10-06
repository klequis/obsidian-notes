Function: `encoderDecoder = "book"`

```haskell
encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad
```

```haskell
encoderDecoder "book"
-- "1\a\a\ETX"
```

----

Function: `applyOTP myPad "book"`

```haskell
applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
	where bitList = applyOTP' pad plaintext
```

```haskell
applyOTP myPad "book"
-- "1\a\a\ETX"
String
```

```haskell
bitList = applyOTP' myPad "book"
-- [[True, True ...], [True, ... False]]
[[Bool], [Bool]] -- length exactly 2
```

```haskell
a = map bitsToChar bitList
-- "1\a\a\ETX"
String
```

Function: `applyOTP' myPad "book"`

```haskell
applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair ->
						(fst pair) `xor` (snd pair))
						(zip padBits plaintextBits)
	where padBits = map charToBits pad
		  plaintextBits = map charToBits plaintext
```

```haskell
padBits = map charToBits myPad
-- [[True, True ...], [True, ... False], ...]
[[Bool]] -- 7 inner [Bool]
```

```haskell
plaintextBits = map charToBits "book"
-- [[False, False ...], [True, ... False], ...]
[[Bool]] -- 4 inner [Bool]
```

```haskell
a = zip PadBits plaintextBits
-- [
--    ([True,False...],[False,True...]),
--    ([True,False...],[False,True...]),
--    ...
-- ]
[([Bool], [Bool])] -- 4 ()
```

Take 1 pair  from `a`, call it `p`, and feed to the below you get
```haskell
mm = \pair -> ((fst pair) `xor` (snd pair)) ([Bool],[Bool]) p
--- b = [Bool,Bool,...]
[Bool]
```

Back to the function: If you use the same equation but apply it by mapping over `a`
```haskell
b = map (\pair -> (fst pair) `xor` (snd pair)) a
[[Bool]] -- 4 inner [Bool]
```

```haskell
xorBool
```

```haskell
xorPair
	xorBool
```

```haskell
xor
	xorPair
```

```haskell
intToBits'
	(none)
```

```haskell
maxBits
	intToBits'
```

```haskell
intToBits
	itToBits'
```

```haskell
charToBits
	intToBits
```

```haskell
bitsToInt
	(none)
```

```haskell
bitsToChar
	bitsToInt
```

```haskell
applyOTP'
	charToBits
	xor
```

```haskell
applyOTP
	applyOTP' 
	bitsToChar
```

```haskell
encoderDecoder
	applyOTP
```
---

```haskell
encoderDecoder 
-> applyOTP 
   -> applyOpt' -> charToBits -> intToBits
		           -> xor -> xorPair -> xorBool
   -> bitsToChar -> bitsToInt
```

```haskell
String -> String
encoderDecoder 
   String -> String -> String
-> applyOTP 
      String -> String -> [Bits]
   -> applyOpt' -> charToBits -> intToBits
		           -> xor -> xorPair -> xorBool
   -> bitsToChar -> bitsToInt
```