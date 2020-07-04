# TLghc

### Summary
**TLghc** is a proof-of-concept interpreter of the **TransLucid** language,
written in Haskell.  TransLucid is the language being developed to explore
**Cartesian Programming**.  Presentation and discussion of these ideas
can be found at
[TransLucid and Cartesian Programming](https://cartesianprogramming.com).

### Installation
The interpreter is a standalone Haskell **stack** package, which can only be
used if you have **stack** installed. If you do not have **stack** installed,
follow the
[Stack Install/upgrade Instructions](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

Once you have **stack** installed, **git clone** the repository,
enter the directory, and type

    stack build

to build a binary called **TLghc-exe**. See **Usage** below.

To run the **TLghc** test suite, type

    stack test

### Usage
The basic interface for **TLghc-exe** is as follows:

    stack exec TLghc-exe file1 file2 ... fileN

will concatenate the _N_ files `file1`, ..., `fileN`,
then parse them as a TransLucid program, then execute that program,
and output the result of the execution.

If _N_ = 0, i.e., no files are specified,
then **TLghc-exe** will read from the standard input.

There is also the possibility of adding options to **TLghc-exe**:

    stack exec TLghc-exe -- options file1 file2 ... fileN

There are currently two options:
* The option

      --parseOnly

  ensures that the input parses correctly, but does not execute it.

* The option

      --printAST

  prints out the abstract syntax tree of the parsed input. If there
  are syntax errors in the input, then this option can be used to
  figure out where the syntax errors were detected by the parser.

### Structure of a TransLucid program
A TransLucid program consists of a set of declarations for dimensions,
variables and functions, and a set of demands for computation.
These can be interleaved as the programmer wishes.

The `dim` keyword declares a dimension identifier.
* In the declaration

      dim d <- expr

  dimension `d` is declared, whose initial ordinate is set
  to be the result of evaluating expression `expr`.

The `var` keyword declares a variable identifier.
* In the declaration

      var x = expr

  variable `x` is declared to be equal to expression `expr`.

The `fun` keyword declares a function identifier.
* In the declaration

      fun x.d1...dm = expr

  function `x`, indexed by the _m_ dimension formal parameters
  `d1` through `dm`, is declared to be equal to expression `expr`.
* In the declaration

      fun x(x1,...,xn) = expr

  function `x` is declared to take the _n_ formal parameters
  `x1` through `xn` and to have `expr` as body.
* In the declaration

      fun x.d1...dm(x1,...,xn) = expr

  function `x`, indexed by the _m_ dimension parameters
  `d1` through `dm`, is declared to take the _n_ formal parameters
  `x1` through `xn` and to have `expr` as body.

The `evalExpr` keyword makes a demand for computation.
* In the demand

      evalExpr expr @ [d1 <- min1..max1, ..., dm <-minm, maxm]

expression `expr` is to be evaluated in each of the contexts
in the hypercube defined by setting, respectively,
the ordinate for dimension `i` to be between `mini` and `maxi`.

### Structure of TransLucid expressions
TransLucid is a functional language with `where` clauses, that allows
the manipulation of a multidimensional context. The syntactic elements are:

* variables
  * `x` defined in one of the `var` declarations

* constants
  * integers
  * characters in single quotes
  * strings in double quotes
  * Booleans `true` and `false`

* unary operators
  * `!` (Boolean negation)
  * `-` (arithmetic negation)

* binary operators
  * `*`, `/`, `%` (mod), `%%` (rem)
  * `+`, `-`
  * `<`, `<=`, `>=`, `>`, `==`, `/=`
  * `&&`, `||`

* conditional operator
  * `if expr then expr else expr`

* arrays (1 to 5 dimensions)
  * `[[(d,min,max)], [val, ..., val]]` (1 dimension, `max-min+1` elements)
  * `[[(d1,min1,max1),(d2,min2,max2)], [val, ..., val]]` (2 dimensions)
  * ...

* context query
  * `#d` queries the current context for
    the ordinate associated with dimension `d`.

* context perturbation
  * `expr [d1 <- expr1, ..., dm <- exprm]` perturbs the current context
    by changing, respectively, the ordinate for dimension `di` to
    the result of evaluating `expri`.

* lambda abstraction
  * `fn.d1...dm(x1,...,xn) -> expr` defines a dimensionally abstract function.

* function application
  * `(expr).d1...dm(expr1,...,exprn)` evaluates `expr` to produce a
    dimensionally abstract function, then applies it to dimension
    actual parameters `d1` through `dm`, and actual parameters
    `expr` through `exprn`.

* local dimension and variable declaration
  * `expr where declarations end`, where `declarations` is a sequence of
    `dim` and `var` declarations, allows the declaration of identifiers
    local to expression `expr`.

### Simple examples
The file `examples/simpleExamples` contains the following declarations:

    fun index.d = #d
    fun index2.d1.d2 = #d1 + #d2
    fun first.d(X) = X [d <- 0]
    fun next.d(X)  = X [d <- #d + 1]
    fun prev.d(X)  = X [d <- #d - 1]
    fun fby.d(X,Y) = if #d < 1 then X else prev.d(Y)
    fun ybf.d(X,Y) = if #d > -1 then Y else next.d(X)
    fun lPair.d(X) = X [d <- #d * 2]
    fun rPair.d(X) = X [d <- #d * 2 + 1]
    fun evenParent.d(X) = X [d <- #d / 2]
    fun oddParent.d(X) = X [d <- #d / 2 + 1]

    fun asa.d(X,Y) = first.d (X wvr.d Y)

    fun wvr.d(X,Y) = X [d <- T]
      where
        var T = U fby.d U [d <- T+1]
        var U = if Y then #d else next.d(U)
      end

    fun upon.d(X,Y) = X [d <- W]
      where
        var W = 0 fby.d (if Y then W+1 else W)
      end

    dim d  <- 0
    dim d0 <- 0
    dim d1 <- 0

    var X = 0 fby.d X+1
    var Y = Y-1 ybf.d 0

    evalExpr X @ [d <- -2 .. 2]
    evalExpr Y @ [d <- -2 .. 2]
    evalExpr index2.d0.d1 @ [d0 <- 1 .. 3, d1 <- 2 .. 4]

The expected output from typing

    stack exec TLghc-exe examples/simpleExamples

is as follows:

    TLvar "X"
      d <- -2: TLint 0
           -1: TLint 0
           0: TLint 0
           1: TLint 1
           2: TLint 2
    TLvar "Y"
      d <- -2: TLint -2
           -1: TLint -1
           0: TLint 0
           1: TLint 0
           2: TLint 0
    TLapply (TLvar "index2") ["d0","d1"] []
      d0 <- 1, d1 <- 2: TLint 3
                     3: TLint 4
                     4: TLint 5
            2, d1 <- 2: TLint 4
                     3: TLint 5
                     4: TLint 6
            3, d1 <- 2: TLint 5
                     3: TLint 6
                     4: TLint 7
