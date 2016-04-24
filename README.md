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

## Installation & Building

### Requirements

The recommended versions below are the versions which the project was tested with and is known to be working.

+ **[make]** or [gradle]
+ **[haskell]**, recommended: 7.10.2
+ **[cabal]**, recommended: 1.22.4.0
+ **[bnfc]** >= 2.8
+ **[alex]**, recommended: 3.1.4
+ **[happy]**, recommended: 1.19.5
+ **[pandoc]**, recommended: 1.17.0.2, for building documentation.

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

### Building, from src/ repository (using make):

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
if ( cond ) { s; }
if ( cond ) { s1; } else { s2; }
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

<!-- references -->