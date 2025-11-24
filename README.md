# Typed Lambda Calculus Interpreter with Security Type System

A small interpreter and static type checker for a typed lambda calculus language implemented in Ocaml.

Currently supports integers, booleans, basic arithmetic, let bindings, functions, and conditionals. 


### Future plan:
 - Support security labels (`@public`, `@secret`) to enforece information-flow control (IFC).
 - (Optional) Add support for loops


## Getting Started
Note that this was written mainly for Apple Scilicon MacOS.
### 1. Clone the repository
```
git clone git@github.com:ge419/typed_lambda.git
cd typed_lambda
```

### 2. Install dependencies
Instructions to install OCaml and Dune can be found [here](https://ocaml.org/docs/installing-ocaml). Once you've installed OCaml and initialized opam, run the commands below.
```
opam install dune ounit2
```

### 3. Build and run tests
```
dune build
dune runtest
```

You should see something like:
```
Ran: 13 tests in: 0.11 seconds.
OK
```

### 4. Run the REPL
```
dune exec bin/main.exe
```