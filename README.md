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
[Stack Install/upgrade Instruction](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

Once you have **stack** installed, **git clone** the repository,
enter the directory, and type

    stack build

A binary called **TLghc-exe** will be built. To execute this binary, type

    stack exec TLghc-exe

To run the **TLghc** test suite, type

    stack test

### Usage
The interface for **TLghc-exe** is very simple:

    stack exec TLghc-exe file1 file2 ... fileN

will concatenate the _N_ files `file1`, ..., `fileN`,
then parse them as a TransLucid program, then execute that program.

### Structure of a TransLucid program
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
The file `simpleExamples` contains the following declarations:

    var index.d = #d
    var index2.d1.d2 = #d1 + #d2
    var first.d(X) = X [d <- 0]
    var next.d(X)  = X [d <- #d + 1]
    var prev.d(X)  = X [d <- #d - 1]
    var fby.d(X,Y) = if #d < 1 then X else Y [d <- #d - 1]
    var ybf.d(X,Y) = if #d > -1 then Y else X [d <- #d + 1]
    var lPair.d(X) = X [d <- #d * 2]
    var rPair.d(X) = X [d <- #d * 2 + 1]

    var asa.d(X,Y) = first.d (X wvr.d Y)
    var wvr.d(X,Y) = X [d <- T]
      where
        var T = U fby.d U [d <- T+1]
        var U = if Y then #d else next.d(U)
      end
    var upon.d(X,Y) = X [d <- W]
      where
        var W = 0 fby.d if Y then W+1 else W
      end

    dim d  <- 0
    dim d0 <- 0
    dim d1 <- 0
    var X = 0 fby.d X+1
    var Y = Y-1 ybf.d 0

    evalExpr X @ [d <- -2 .. 2]
    evalExpr Y @ [d <- -2 .. 2]
    evalExpr index2.d0.d1 @ [d0 <- 1 .. 3, d1 <- 2 .. 4]
