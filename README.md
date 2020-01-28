# TLghc

### Summary
**TLghc** is a proof-of-concept interpreter of the **TransLucid** language,
written in Haskell.  TransLucid is the language being developed to explore
**Cartesian Programming**.  Presentation and discussion of these ideas
can be found at
[TransLucid and Cartesian Programming](cartesianprogramming.com).

### Installation
The interpreter is a standalone **cabal** package, which can only be used
if you have **ghc** and **cabal** installed.  If you do, then just
**git clone** the repository, and then run `cabal v2-build`. It will
build a binary called **tlghc**.

### Usage
The interface for **tlghc** is very simple:
  `tlghc file1 file2 ... fileN`
will concatenate the _N_ files `file1`, ..., `fileN`,
then parse them as a TransLucid program, then execute that program.

### Structure of program
A TransLucid program consists of a set of declarations for dimensions,
variables and functions, and a set of demands for computation.
These can be interleaved as the programmer wishes.

The `dim` keyword declares a dimension identifier.
* In the declaration
      dim d <- expr
  dimension `d` is declared, whose initial ordinate is set
  to be the result of evaluating expression `expr`.

The `var` keyword declares a variable or function identifier.

* In the declaration
      var x = expr
  variable `x` is declared to be equal to expression `expr`.
* In the declaration
      var x.d1...dm = expr
  variable `x`, indexed by the _m_ dimension formal parameters
  `d1` through `dm`, is declared to be equal to expression `expr`.
* In the declaration
      var x(x1,...,xn) = expr
  function `x` is declared to take the _n_ formal parameters
  `x1` through `xn` and to have `expr` as body.
* In the declaration
      var x.d1...dm(x1,...,xn) = expr
  function `x`, indexed by the _m_ dimension parameters
  `d1` through `dm`, is declared to take the _n_ formal parameters
  `x1` through `xn` and to have `expr` as body.

