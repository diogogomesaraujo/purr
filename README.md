<br />
<div align="center">
    <img src="./assets/purr.png" alt="purr" style="width: 200px;"/>
  <p align="center">
      A *purr*ely functional programming language.
  </p>
</div>

<!-- ABOUT THE PROJECT -->
## About

This repository contains a compiler and interpreter for a lazy evaluation programming language (such as Haskell) with ML-like syntax.

It was built as a hobby project to retain concepts like lambda calculus, graph reduction, type inference and lazy evaluation learnt from the Fundamentals of Programming Languages and Implementation of Programming Languages courses of my Master's Degree in Computer Science at Universidade do Porto.

## Type System

Much like any functional programming language, `purr` is strongly typed (at compile-time) and uses the Hindley-Milner/Damas-Milner algorithm for inferring types without the programmer intervention.

### Syntax
```
type ::= int | bool | float | string | type -> type | [type] 
```

### Examples

A wrapper for the `cons` operation can be defined by the following expressions (semantically identical):

```haskell
let my_cons : x xs = x:xs in my_cons 3 [1,2]
```
```haskell
let my_cons : x xs : a -> [a] -> [a] = x:xs in my_cons 3 [1,2]
```
```haskell
let my_cons : x xs : int -> [int] -> [int] = x:xs in my_cons 3 [1,2]
```

## Language

The language consists of an extended lambda calculus and, as previously stated, has a syntax similar to ML-based languages like OCaml, SML and F#.

### Syntax
```
const ::= int | bool | float | [const]

op    ::= == | != | < | <= | > | >= | && 
             | || | + | - | / | * | :

var   ::= string | (custom_op)

term  ::= const | term op term 
                | let var := term in term 
                | let var : [string] = term in term 
                | let var : [string] : type = term in term
                | let rec var := term in term 
                | let rec var : [string] = term in term 
                | let rec var : [string] : type = term in term
                | \[string] . term
                | \[string] : type . term
                | if term then term else term
                | term term
                | term -- comment
```

### Examples

There are several examples you can try in the [`examples`](./examples) folder such as:

1. Increment
```haskell
let incr :=
  fn x -> x + 1
in map incr [0,1,2]
```

2. Fibonnaci Sequence

```haskell
let rec fib : x =
    if x < 2
        then 1
        else (fib (x - 1)) + (fib (x - 2))
in fib 10
```

3. Ascending List
```haskell
let rec ascending_rec : size current_size list
    = if current_size > size
        then list
        else current_size
             : ascending_rec size (current_size + 1) list
in

let ascending : size
    = ascending_rec size 1 []
in

ascending 10
```

## Standard Library

A set of predefined functions that are automatically compiled with every program. Check out the [implementation](./stdlib/common.pr).

```haskell
--
-- Standard Library for the *purr* functional programming language.
--

--
-- Operators
--

-- Operator that represents a pipe (inspired by OCaml) where the first
-- argument is applied to the second (f x <=> x |> f).
let (|>) : x f : c -> (c -> d) -> d
    = f x
in

-- Alternative operator for different (!=) (for OCaml lovers).
let (<>) : x y : e -> e -> e
    = x != y
in

--
-- Lists
--

-- Function that checks if a list is empty.
let is_empty : l : [f] -> bool
    = l == []
in

-- Function that applies a given function `f` to all elements of `l`.
let rec map : f l : (g -> h) -> [g] -> [h] =
    if is_empty l
        then []
        else f (head l) : map f (tail l)
in

-- Function that applies a given function `f` to all elements of `l`
-- (leftwise) carrying an accumulator `acc`.
let rec fold_left : f acc l : (j -> i -> j) -> j -> [i] -> j =
    if is_empty l
        then acc
        else fold_left f (f acc (head l)) (tail l)
in

-- Function that returns the number of elements in a list
let len : l : [k] -> int =
    fold_left (\acc x . acc + 1) 0 l
in
```

## Getting Started

### Prerequisites

In order to run this project from source, you will need to have Haskell and `cabal` installed.

### Command-line Tool

Before executing the program you have to compile the lexer and the parser with the following commands:
```bash
# compile the lexer
alex src/Lexer.x

# compile the parser
happy src/Parser.y --ghc
```

Then you have to install the command-line tool:
```bash
cabal install exe:purr
```

Then, you can execute programs written in files with the following command:
```bash
purr <file_path>
```

And use a playground environment with:
```bash
purr
```

## References

The study material I used to learn the concepts required to implement `purr` where:

- Professor Mario Florido's lecture notes;
- *Implementing lazy functional languages on stock hardware: the Spineless Tagless G-machine* - Simon Peyton Jones.
