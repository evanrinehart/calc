# calc

In this language there are only expressions to compute integers or
strings. An expression can be type-checked using `infer` to determine
its type or the line of a type error. A well-formed expression can be
simplified using `eval` to produce its unique normal form which always
exists. Based on the first example from the book [Practical Foundations
for Programming Languages](http://www.cs.cmu.edu/~rwh/plbook/2nded.pdf).

## Language / Implementation

```
T = Integer | String
E = integer
  | string
  | variable
  | let variable = E in E
  | E + E
  | E - E
  | E * E
  | E ++ E
  | len(E)
  | decimal(E)
  | (E)

parse : Text -> E || Error
infer : E -> T || Error
eval  : E -> E
```

## Usage

```
./calc
let x = 2 in x + x
<ctrl+D>
4 : Integer
```

If file `prog.x` contains the expression

```
let s1 = "fruit" in
let s2 = "cake" in
s1 ++ " " ++ s2
```

then you can feed it directly to the interpreter with

```
./calc prog.x
"fruit cake" : String
```

## Type Checking / Inferencing

Not all syntactically valid expressions make sense.

```
./calc
let s1 = "fruit" in
let s2 = "cake" in
s1 ++ " " ++ len(s2)
<ctrl+D>
calc: line 3: right operand of ++ not a string
```

## Compiler

Output C source code.

```
./calc -c prox.x | gcc -x c -
./a.out
fruit cake
```
