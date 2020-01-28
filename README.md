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
**git clone** the repository, and then run **cabal v2-build**. It will
build a binary called **tlghc**.

### Usage
The interface for **tlghc** is very simple:
  `tlghc file1 file2 ... fileN`
will concatenate the _N_ files `file1`, ..., `fileN`,
then parse them as a TransLucid program, then execute that program.

