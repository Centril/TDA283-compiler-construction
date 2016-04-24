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

<!-- references -->The lexical structure of Javalette
----------------------------------

### Identifiers

Identifiers *Ident* are unquoted strings beginning with a letter, followed by any combination of letters, digits, and the characters `_ '` reserved words excluded.

### Literals

Integer literals *Integer* are nonempty sequences of digits.

Double-precision float literals *Double* have the structure indicated by the regular expression `digit+ '.' digit+ ('e' ('-')? digit+)?` i.e.\\ two sequences of digits separated by a decimal point, optionally followed by an unsigned or negative exponent.

String literals *String* have the form `"`*x*`"`}, where *x* is any sequence of any characters except `"` unless preceded by `\`.

### Reserved words and symbols

The set of reserved words is the set of terminals appearing in the grammar. Those reserved words that consist of non-letter characters are called symbols, and they are treated in a different way from those that are similar to identifiers. The lexer follows rules familiar from languages like Haskell, C, and Java, including longest match and spacing conventions.

The reserved words used in Javalette are the following:

|          |           |          |          |
|:--------:|:---------:|:--------:|----------|
| `String` | `boolean` | `double` | `else`   |
|  `false` |    `if`   |   `int`  | `return` |
|  `true`  |   `void`  |  `while` |          |

The symbols used in Javalette are the following:

|       |      |       |      |     |
|:-----:|:----:|:-----:|------|----:|
|   (   |   )  |   ,   | {    |     |
|   }   |   ;  |   =   | ++   |     |
|   --  |   -  |   !   | &&   |     |
|       |      |   +   | \*   |    /|
|   %   | &lt; | &lt;= | &gt; |     |
| &gt;= |  ==  |   !=  |      |     |

### Comments

Single-line comments begin with \#, //.Multiple-line comments are enclosed with /\* and \*/.

The syntactic structure of Javalette
------------------------------------

Non-terminals are enclosed between &lt; and &gt;. The symbols -&gt; (production), *\|*  (union) and **eps** (empty rule) belong to the BNF notation. All other symbols are terminals.

|              |       |                                          |
|:------------:|:-----:|------------------------------------------|
|   *Program*  | -&gt; | *\[TopDef\]*                             |
|   *TopDef*   | -&gt; | *Type* *Ident* `(` *\[Arg\]* `)` *Block* |
| *\[TopDef\]* | -&gt; | *TopDef*                                 |
|              | *\|*  | *TopDef* *\[TopDef\]*                    |
|     *Arg*    | -&gt; | *Type* *Ident*                           |
|   *\[Arg\]*  | -&gt; | **eps**                                  |
|              | *\|*  | *Arg*                                    |
|              | *\|*  | *Arg* `,` *\[Arg\]*                      |
|    *Block*   | -&gt; | `{` *\[Stmt\]* `}`                       |
|  *\[Stmt\]*  | -&gt; | **eps**                                  |
|              | *\|*  | *Stmt* *\[Stmt\]*                        |
|    *Stmt*    | -&gt; | `;`                                      |
|              | *\|*  | *Block*                                  |
|              | *\|*  | *Type* *\[Item\]* `;`                    |
|              | *\|*  | *Ident* `=` *Expr* `;`                   |
|              | *\|*  | *Ident* `++` `;`                         |
|              | *\|*  | *Ident* `--` `;`                         |
|              | *\|*  | `return` *Expr* `;`                      |
|              | *\|*  | `return` `;`                             |
|              | *\|*  | `if` `(` *Expr* `)` *Stmt*               |
|              | *\|*  | `if` `(` *Expr* `)` *Stmt* `else` *Stmt* |
|              | *\|*  | `while` `(` *Expr* `)` *Stmt*            |
|              | *\|*  | *Expr* `;`                               |
|    *Item*    | -&gt; | *Ident*                                  |
|              | *\|*  | *Ident* `=` *Expr*                       |
|  *\[Item\]*  | -&gt; | *Item*                                   |
|              | *\|*  | *Item* `,` *\[Item\]*                    |
|    *Type*    | -&gt; | `int`                                    |
|              | *\|*  | `double`                                 |
|              | *\|*  | `boolean`                                |
|              | *\|*  | `void`                                   |
|  *\[Type\]*  | -&gt; | **eps**                                  |
|              | *\|*  | *Type*                                   |
|              | *\|*  | *Type* `,` *\[Type\]*                    |
|    *Expr6*   | -&gt; | *Ident*                                  |
|              | *\|*  | *Integer*                                |
|              | *\|*  | *Double*                                 |
|              | *\|*  | `true`                                   |
|              | *\|*  | `false`                                  |
|              | *\|*  | *Ident* `(` *\[Expr\]* `)`               |
|              | *\|*  | *String*                                 |
|              | *\|*  | `(` *Expr* `)`                           |
|    *Expr5*   | -&gt; | `-` *Expr6*                              |
|              | *\|*  | `!` *Expr6*                              |
|              | *\|*  | *Expr6*                                  |
|    *Expr4*   | -&gt; | *Expr4* *MulOp* *Expr5*                  |
|              | *\|*  | *Expr5*                                  |
|    *Expr3*   | -&gt; | *Expr3* *AddOp* *Expr4*                  |
|              | *\|*  | *Expr4*                                  |
|    *Expr2*   | -&gt; | *Expr2* *RelOp* *Expr3*                  |
|              | *\|*  | *Expr3*                                  |
|    *Expr1*   | -&gt; | *Expr2* `&&` *Expr1*                     |
|              | *\|*  | *Expr2*                                  |
|    *Expr*    | -&gt; | *Expr1* `||` *Expr*                      |
|              | *\|*  | *Expr1*                                  |
|  *\[Expr\]*  | -&gt; | **eps**                                  |
|              | *\|*  | *Expr*                                   |
|              | *\|*  | *Expr* `,` *\[Expr\]*                    |
|    *AddOp*   | -&gt; | `+`                                      |
|              | *\|*  | `-`                                      |
|    *MulOp*   | -&gt; | `*`                                      |
|              | *\|*  | `/`                                      |
|              | *\|*  | `%`                                      |
|    *RelOp*   | -&gt; | `<`                                      |
|              | *\|*  | `<=`                                     |
|              | *\|*  | `>`                                      |
|              | *\|*  | `>=`                                     |
|              | *\|*  | `==`                                     |
|              | *\|*  | `!=`                                     |


