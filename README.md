<br />
<div align="center">
  <h3 align="center">purr</h3>
  <p align="center">
      A *purr*ely functional programming language.
  </p>
</div>

<!-- ABOUT THE PROJECT -->
## About

This repository contains a compiler and interpreter for a lazy evaluation programming language (such as Haskell) with ML-like syntax.

It was built as a hobby project to retain concepts like lambda calculus, graph reduction, type inference and lazy evaluation learnt from the Fundamentals of Programming Languages and Implementation of Programming Languages courses of my Master's Degree in Computer Science at Universidade do Porto.

## Language Features

- Boolean, ints and floats;
- Comments (`-- ...`);
- Conditionals (`if then else`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `&&`, `||`);
- Arithmetic expressions (`+`, `-`, `/`, `*`);
- Lambda Functions (`fn x -> ...`);
- Variable Declaration (`let x := ...`);
- Recursive and Non-Recursive Functions (`let f : x = ... in ...`, `let rec f : x = ... in ...`);
- Custom Operators (`let (**) : x y = x * y in x ** y`)
- Lists (`[...,...]`).

## Getting Started

### Prerequisites

In order to run this project from source, you will need to have Haskell and `cabal` installed.

### Command-line Tool

You can execute programs written in files with the following command:
```bash
cabal run purr -- -f <file_path>
```

## Code Examples

There are several examples you can try in the [`examples`](./examples) folder such as:

1. Increment
```ocaml
let incr : x =
  fn x -> x + 1
in incr 1
```

2. Factorial

```ocaml
let rec fact : x
    = if x == 0 then 1 else x * fact (x - 1)
in

fact 5
```

3. Fibonnaci Sequence

```ocaml
let rec fib : x =
    if x < 2
        then 1
        else (fib (x - 1)) + (fib (x - 2))
in 

fib 10
```

4. Catalan Numbers
```ocaml
let rec fact : x
    = if x == 0 then 1 else x * fact (x - 1)
in

let rec catalan : x
    = fact (2 * x) / (fact (x + 1) * fact x)
in

catalan 10
```
