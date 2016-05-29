# [Javalette Compiler]

> Javalette Compiler or **jlc** is a compiler for a small C-like language.

## What?

A compiler written in haskell for the small C-like langauge called Javalette. This is for the course **[TDA283, Compiler Construction]** at Chalmers University of Technology, Sweden.

## Copyright & License

Licensed under the **[GPL 2+ License]**.
Copyright 2016 Björn Tropf, Mazdak Farrokhzad.

## Authors

The authors and contributors of this project are:

**Björn Tropf [&lt;bjoerntropf@gmail.com&gt;]**

**Mazdak Farrokhzad [&lt;twingoow@gmail.com&gt;]**

## How to use

`$ jlc --help` yields:

```
The glorious Javalette Compiler.
Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
Distributed under GPL2 or any later version.

Usage: jlc FILES... [-o|--output FILE] [--outtype exec | asm | bc]
           [-w|--fwarn-error] [-n|--fno-warn-unused] [-t|--typecheck]
           ([-q|--quiet] | [--info] | [--warn]) ([-0|--O0] | [-1|--O1] |
           [-2|--O2] | [-3|--O3] | [-4|--O4] | [-5|--O5] | [-6|--O6])
           [-l|--llvm-extras FILES...] [-i|--llvm-inter]

  To compile and link a complete Javalette program, run the compiler like so
  (assuming it is on the PATH):

      jlc myprogram.jl

  which will create the file myprogram.out (or myprogram.exe on Windows).

  Jlc will determine how to handle files by their extensions.
  It will treat .ll files as llvm IR and .bc at llvm bitcode.
  All other files will be treated as .jl files.

  For more advanced use cases, see options.

Available options:
  -h,--help                Show this help text
  FILES...                 FILE paths to javalette input files
  -o,--output FILE         FILE path to executable to produce
  --outtype exec | asm | bc
                           exec for executable, asm for native assembly, bc for
                           llvm bitcode (default: exec)
  -w,--fwarn-error         Turn all warnings into errors
  -n,--fno-warn-unused     Do not warn about unused parameter and variables
  -t,--typecheck           Only perform typechecking
  -q,--quiet               Suppress all messages except for errors
  --info                   Verbose mode. Show all types of messages including
                           info messages
  --warn                   Suppress all messages except for errors and warnings
                           (default)
  -0,--O0                  No optimization (default), for LLVM this is -O0
  -1,--O1                  Optimization level 1, for LLVM this is -O1
  -2,--O2                  Optimization level 2, for LLVM this is -O2
  -3,--O3                  Optimization level 3, for LLVM this is -Os
  -4,--O4                  Optimization level 4, for LLVM this is -Oz
  -5,--O5                  Optimization level 5, for LLVM this is -O3
  -6,--O6                  Optimization level 6, for LLVM this is -O4
  -l,--llvm-extras FILES...
                           FILE paths to additional llvm code for compilation
                           Applies in conjunction with the llvm backend
  -i,--llvm-inter          Emit all intermediary produced .ll files
                           Applies in conjunction with the llvm backend
```

**NOTE:** `--outtype bc` is default in submissions, even tho `jlc --help` says otherwise.

## Installation & Building

### Requirements

The recommended versions below are the versions which the project was tested with and is known to be working.

+ **[make]** or [gradle]
+ **[haskell]**, recommended: `7.10.2`
+ **[cabal]**, recommended: `1.22.4.0`
+ **[bnfc]** `>= 2.8`
+ **[alex]**, recommended: `3.1.4`
+ **[happy]**, recommended: `1.19.5`
+ **[pandoc]**, recommended: `1.17.0.2`, for building documentation.
+ **[llvm]**, recommended: `3.5.*` - will **NOT** work with `>= 3.7`

### Building, from git repository:

First clone the repository to your local machine:

```shell
git clone https://github.com/Centril/TDA283-compiler-construction
```

Assuming that:

```shell
cd TDA283-Compiler-Construction
```

If you are building from the git repository with gradle, you can do:

```shell
./gradlew build
```

To build and run tests.

or alternatively using make:

```shell
make all
```

### Building, from src/ (using make):

Assuming that:

```shell
cd src/
```

```shell
make all
```

## Javalette Grammar conflicts

### shift-reduce

The standard dangling else:
```c
Cond.      Stmt ::= "if" "(" Expr ")" Stmt ;
CondElse.  Stmt ::= "if" "(" Expr ")" Stmt "else" Stmt ;
```

The array separator is an empty string:
```c
Array.     Type ::= Type [DimT] ;
separator  nonempty DimT "" ;
```

Dimensions of LValues can be empty:
```c
LValueS.   LValue ::= Ident [DimE] "."  LValue ;
separator  DimE "" ;
```

Coercions causes issues with the brackets "(" and ")":
```c
ECastNullX. Expr7 ::= "(" Ident ")" "null" ;
coercions   Expr 7 ;
```

### reduce-reduce

There are no reduce-reduce conflicts.

## Javalette language specification

<!-- references -->

[Javalette Compiler]: https://github.com/Centril/TDA283-compiler-construction

[TDA283, Compiler Construction]: http://www.cse.chalmers.se/edu/course/TDA283_Compiler_Construction/

[&lt;twingoow@gmail.com&gt;]: mailto:twingoow@gmail.com
[&lt;bjoerntropf@gmail.com&gt;]: mailto:bjoerntropf@gmail.com

[GPL 2+ License]: LICENSE.md

[gradle]: http://gradle.org/
[make]: https://www.gnu.org/software/make/
[alex]: https://www.haskell.org/alex/
[happy]: https://www.haskell.org/happy/
[bnfc]: http://bnfc.digitalgrammars.com/
[haskell]: https://www.haskell.org/
[cabal]: https://www.haskell.org/cabal/
[pandoc]: http://johnmacfarlane.net/pandoc/
[llvm]: http://llvm.org/

<!-- references -->