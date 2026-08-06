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

```ocaml
let my_cons : x xs = x:xs in my_cons 3 [1,2]
```
```ocaml
let my_cons : x xs : a -> [a] -> [a] = x:xs in my_cons 3 [1,2]
```
```ocaml
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
```ocaml
let incr :=
  fn x -> x + 1
in incr 1
```

2. Fibonnaci Sequence

```ocaml
let rec fib : x =
    if x < 2
        then 1
        else (fib (x - 1)) + (fib (x - 2))
in 

fib 10
```

3. Ascending List
```ocaml
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
