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

To run each test file separately, 

```
dune exec bin/main.exe examples/<testname>.lambda
```

### 4. Run the REPL
```
dune exec bin/main.exe
```
## Language

### Language Syntax
$$
\begin{array}{rcl}
v &::=& n \mid \text{true} \mid \text{false} \mid \lambda (x : T).\, e \\
e &::=& v \mid x \mid e_1 + e_2 \mid e_1 - e_2 \mid e_1\,e_2 \\
  &\mid& \textbf{if } e_1 \textbf{ then } e_2 \textbf{ else } e_3 \\
  &\mid& \textbf{let } x = e_1 \textbf{ in } e_2 \\
T &::=& \text{int} \mid \text{bool} \mid T_1 \to T_2 \\
\tau &::=& T@\ell \qquad \ell \in \{\text{public},\text{secret}\}
\end{array}
$$

### Security Lattice

$$
\text{Lattice}
\qquad
\text{public} \le \text{secret} \quad\qquad
\text{join}(\ell_1,\ell_2) =
\begin{cases}
\text{public} & \text{if } \ell_1=\ell_2=\text{public} \\
\text{secret} & \text{otherwise}
\end{cases}
$$

### Examples

```
let id = fun (x : int@public) -> x in
let a : int@public = id 3 in
a
```
This will be allowed since it only deals with public

```
let s : int@secret = 42 in
let p : int@public = s in
p
```
Rejected: secret explicitly flows into public

```
let s : bool@secret = true in
let p : int@public =
  if s then 1 else 0 in
p
```
Rejected: implicit flow violated

```
let a : int@public = 1 in
let b : int@secret = 2 in
let c = a + b in
...
```
Here, `c` is now treated as secret

### Typing Rules

<!-- $$
\text{T-Int}
\qquad
\frac{ }{ \Gamma \vdash n : \mathsf{int} }
$$

$$
\text{T-Bool}
\qquad
\frac{ }{ \Gamma \vdash b : \mathsf{bool} }
$$

$$
\text{T-Var}
\qquad
\frac{ x : T \in \Gamma }{ \Gamma \vdash x : T }
$$

$$
\text{T-Add}
\qquad
\frac{ \Gamma \vdash e_1 : \mathsf{int} \qquad \Gamma \vdash e_2 : \mathsf{int} }
     { \Gamma \vdash e_1 + e_2 : \mathsf{int} }
$$

$$
\text{T-Sub}
\qquad
\frac{ \Gamma \vdash e_1 : \mathsf{int} \qquad \Gamma \vdash e_2 : \mathsf{int} }
     { \Gamma \vdash e_1 - e_2 : \mathsf{int} }
$$

$$
\text{T-If}
\qquad
\frac{ \Gamma \vdash e_c : \mathsf{bool} \qquad \Gamma \vdash e_1 : T \qquad \Gamma \vdash e_2 : T }
     { \Gamma \vdash \textbf{if } e_c \textbf{ then } e_1 \textbf{ else } e_2 : T }
$$

$$
\text{T-Let}
\qquad
\frac{ \Gamma \vdash e_1 : T_1 \qquad \Gamma, x:T_1 \vdash e_2 : T_2 }
     { \Gamma \vdash \textbf{let } x = e_1 \textbf{ in } e_2 : T_2 }
$$

$$
\text{T-Lam}
\qquad
\frac{ \Gamma, x:T_1 \vdash e : T_2 }{ \Gamma \vdash \lambda(x:T_1).\,e : T_1 \to T_2 }
$$

$$
\text{T-App}
\qquad
\frac{ \Gamma \vdash f : T_1 \to T_2 \qquad \Gamma \vdash a : T_1 }
     { \Gamma \vdash f\,a : T_2 }
$$ -->

$$
\text{S-Int}
\qquad
\frac{ }{ \Gamma,\,\text{flow} \vdash n : \mathsf{int@public} }
\qquad
\text{S-Bool}
\qquad
\frac{ }{ \Gamma,\,\text{flow} \vdash b : \mathsf{bool@public} }
$$

$$
\text{S-Add}
\qquad
\frac{ \Gamma,\,\text{flow} \vdash e_1 : \mathsf{int@}\ell_1 \qquad
       \Gamma,\,\text{flow} \vdash e_2 : \mathsf{int@}\ell_2 }
     { \Gamma,\,\text{flow} \vdash e_1 + e_2 : \mathsf{int@join}(\ell_1,\ell_2) }
$$
$$
\text{S-Sub}
\qquad
\frac{ \Gamma,\,\text{flow} \vdash e_1 : \mathsf{int@}\ell_1 \qquad
       \Gamma,\,\text{flow} \vdash e_2 : \mathsf{int@}\ell_2 }
     { \Gamma,\,\text{flow} \vdash e_1 - e_2 : \mathsf{int@join}(\ell_1,\ell_2) }
$$
$$
\text{S-Let}
\qquad
\frac{ \Gamma,\,\text{flow} \vdash e_1 : \tau_1 \qquad
       \Gamma, x:\tau_1 \vdash e_2 : \tau_2 }
     { \Gamma,\,\text{flow} \vdash \textbf{let } x = e_1 \textbf{ in } e_2 : \tau_2 }
$$
$$
\text{S-LetAnn}
\qquad
\frac{ \Gamma,\,\text{flow} \vdash e_1 : T@\ell_{e1} \qquad
       \text{join}(\ell_{e1},\text{flow}) \le \ell_x \qquad
       \Gamma, x:T@\ell_x \vdash e_2 : \tau_2 }
     { \Gamma,\,\text{flow} \vdash \textbf{let } x : T@\ell_x = e_1 \textbf{ in } e_2 : \tau_2 }
$$
$$
\text{S-If}
\qquad
\frac{ \Gamma,\,\text{flow} \vdash e_c : \mathsf{bool@}\ell_c \qquad
       \Gamma,\,\text{join}(\text{flow}, \ell_c) \vdash e_1 : \tau \qquad
       \Gamma,\,\text{join}(\text{flow}, \ell_c) \vdash e_2 : \tau }
     { \Gamma,\,\text{flow} \vdash \textbf{if } e_c \textbf{ then } e_1 \textbf{ else } e_2 : \tau }
$$
$$
\text{S-Lam}
\qquad
\frac{ \Gamma, x:\tau_1,\,\text{flow} \vdash e : \tau_2 }
     { \Gamma,\,\text{flow} \vdash \lambda(x:\tau_1).\,e : \tau_1 \to \tau_2 }
$$
$$
\text{S-App}
\qquad
\frac{ \Gamma,\,\text{flow} \vdash f : \tau_1 \to \tau_2 \qquad
       \Gamma,\,\text{flow} \vdash a : \tau_1 \qquad
       \text{label}(a) \le \text{label}(\tau_1) }
     { \Gamma,\,\text{flow} \vdash f\,a : \tau_2 }
$$
