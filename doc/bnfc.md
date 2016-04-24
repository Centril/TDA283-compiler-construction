The lexical structure of Javalette 
----------------------------------

### Identifiers 

Identifiers *Ident* are unquoted strings beginning with a letter,
followed by any combination of letters, digits, and the characters `_ ’`
reserved words excluded.

### Literals 

Integer literals *Integer* are nonempty sequences of digits.

Double-precision float literals *Double* have the structure indicated by
the regular expression `digit+ ’.’ digit+ (’e’ (’-’)? digit+)?`
i.e.$\backslash$ two sequences of digits separated by a decimal point,
optionally followed by an unsigned or negative exponent.

String literals *String* have the form `"`*x*`"`}, where *x* is any
sequence of any characters except `"` unless preceded by `\backslash`.

### Reserved words and symbols 

The set of reserved words is the set of terminals appearing in the
grammar. Those reserved words that consist of non-letter characters are
called symbols, and they are treated in a different way from those that
are similar to identifiers. The lexer follows rules familiar from
languages like Haskell, C, and Java, including longest match and spacing
conventions.

The reserved words used in Javalette are the following:

  ---------- ----------- ---------- ----------
  `String`   `boolean`   `double`   `else`
  `false`    `if`        `int`      `return`
  `true`     `void`      `while`    
  ---------- ----------- ---------- ----------

The symbols used in Javalette are the following:

  -------- ----- ------ -----
  (        )     ,      {
  }        ;     =      ++
  –        -     !      &&
  $|$$|$   +     \*     /
  %        $<$   $<$=   $>$
  $>$=     ==    !=     
  -------- ----- ------ -----

### Comments 

Single-line comments begin with \#, //.Multiple-line comments are
enclosed with /\* and \*/.

The syntactic structure of Javalette 
------------------------------------

Non-terminals are enclosed between $<$ and $>$. The symbols -$>$
(production), **$|$** (union) and **eps** (empty rule) belong to the BNF
notation. All other symbols are terminals.

  -------------- ------ ------------------------------------------
  *Program*      -$>$   *\[TopDef\]*
  *TopDef*       -$>$   *Type* *Ident* `(` *\[Arg\]* `)` *Block*
  *\[TopDef\]*   -$>$   *TopDef*
                        *TopDef* *\[TopDef\]*
  *Arg*          -$>$   *Type* *Ident*
  *\[Arg\]*      -$>$   **eps**
                        *Arg*
                        *Arg* `,` *\[Arg\]*
  *Block*        -$>$   ``
  *\[Stmt\]*     -$>$   **eps**
                        *Stmt* *\[Stmt\]*
  *Stmt*         -$>$   `;`
                        *Block*
                        *Type* *\[Item\]* `;`
                        *Ident* `=` *Expr* `;`
                        *Ident* `++` `;`
                        *Ident* `–` `;`
                        `return` *Expr* `;`
                        `return` `;`
                        `if` `(` *Expr* `)` *Stmt*
                        `if` `(` *Expr* `)` *Stmt* `else` *Stmt*
                        `while` `(` *Expr* `)` *Stmt*
                        *Expr* `;`
  *Item*         -$>$   *Ident*
                        *Ident* `=` *Expr*
  *\[Item\]*     -$>$   *Item*
                        *Item* `,` *\[Item\]*
  *Type*         -$>$   `int`
                        `double`
                        `boolean`
                        `void`
  *\[Type\]*     -$>$   **eps**
                        *Type*
                        *Type* `,` *\[Type\]*
  *Expr6*        -$>$   *Ident*
                        *Integer*
                        *Double*
                        `true`
                        `false`
                        *Ident* `(` *\[Expr\]* `)`
                        *String*
                        `(` *Expr* `)`
  *Expr5*        -$>$   `-` *Expr6*
                        `!` *Expr6*
                        *Expr6*
  *Expr4*        -$>$   *Expr4* *MulOp* *Expr5*
                        *Expr5*
  *Expr3*        -$>$   *Expr3* *AddOp* *Expr4*
                        *Expr4*
  *Expr2*        -$>$   *Expr2* *RelOp* *Expr3*
                        *Expr3*
  *Expr1*        -$>$   *Expr2* `&&` *Expr1*
                        *Expr2*
  *Expr*         -$>$   *Expr1* `||` *Expr*
                        *Expr1*
  *\[Expr\]*     -$>$   **eps**
                        *Expr*
                        *Expr* `,` *\[Expr\]*
  *AddOp*        -$>$   `+`
                        `-`
  *MulOp*        -$>$   `*`
                        `/`
                        `%`
  *RelOp*        -$>$   `<`
                        `<=`
                        `>`
                        `>=`
                        `==`
                        `!=`
  -------------- ------ ------------------------------------------
