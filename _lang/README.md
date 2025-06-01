# Operator Innovation

https://www.reddit.com/r/ProgrammingLanguages/comments/ya87l1/what_operators_do_you_wish_programming_languages/

https://en.wikipedia.org/wiki/Glossary_of_mathematical_symbols

https://www.cuemath.com/numbers/math-symbols/

### DRAFT
|Operator|[Properties](https://en.wikipedia.org/wiki/Relation_(mathematics)#Properties_of_relations)|Pronunciation|
|-|-|-|
|`unary` *L.*,`monadic` *gr.*||Each `alphabetisation` may be pronounced <br>with the postfix `... the operand`|
|`binary `*L.*,`dyadic` *gr.*|`commutative`|Each `alphabetisation` may be pronounced <br>with the prefix `the operands ...`, <br>and the postfix `... each other`.|
||`non-commutative`|Each `alphabetisation` may be pronounced <br>with the prefix `the left-hand side ...`, <br>and the postfix `... the right-hand side`.|
|`ternary` *L.*, `trinary` *L.,gr.*|

|Built-ins|
|-|

|Russell-Peano notation|
|-|
|`Principia Mathematica, 1910-1913` *preceded by The Principles of Mathematics, 1903*, `propositional logic`,`0th order logic`; `predicate logic`, `1st order logic`; in the background [relational algebra](https://en.wikipedia.org/wiki/Relation_algebra) was developed from 1860 to 1905 by De Morgan, Peirce, and Schroder ... and modernised by Tarski and Givant, see `A Formalization of Set Theory Without Variables 1987` |

|Operator|Arity / Property|Pronunciation|Discarded Alternatives|Gate|
|-|-|-|-|-|
|`~a`  |                |`negates, logically`                   |`!a`*B, C*,`a^^-`, `not a`|`NOT`|
|`a.b` |`commutative`   |`are multiplied, logically`,`both`        |`a/\b`, `a&b`, `a&&b`, `a and b`|`AND`|   
|`a\/b`|`commutative`   |`are summed, logically`,`either, or both` |`a\|b`, `a\|\|b`, `a or b`|`OR`|
|`a=)b`|`noncommutative`|`implies, logically`                      |`a=>b`, `a->b`|? `IMPLY`|
|`a:b`|`noncommutative`|`all, apply to all on`|`(a):(b)`|-|
||
|`i\|a`|-|`the`|-|-|
|`Df a`||`a definition is presented, where`|-|-|
|`\|- a`||`the truth is asserted, of` *Pp, a primitive proposition / axiom*|-|-|
|`A(b)` *PM 2nd edition*, `A(b,c)`|`noncommutative`|`is predicated, upon the subject`,`is a formula, containing a free variable`|`bA` *PM 1st edition*|-|
|`(E\|c)B(c)`|`ternary`|`there exists some c, where B(c) is true`|`[E\|c]B(c)`, `E\|c:B(c)`, `(3]c)B(c)`, `(3c)B(c)`|-|
|`(E\|B)B(c)`|`ternary`|`there exists some B, where B(c) is true`|`[E\|B]B(c)`, `E\|B:B(c)`, `(3]B)B(c)`, `(3B)B(c)`|-|
|`(A\|c)B(c)`|`ternary`|`for all c, B(c) is true`|`[A\|c]B(c)`, `A\|c:B(c)`, `(\-/c)B(c)`|-|
|`(A\|B)B(c)`|`ternary`|`for all B, B(c) is true`|`[A\|B]B(c)`, `A\|B:B(c)`, `(\-/B)B(c)`|-|
|`(A\|d)(B(d) =) C(d))`|-|-|`(B(d) =)_d C(d)`|-|
|`a.b.c`|-|`delimit the terms, a, b, c`|`(a) (b) (c)`|-|
||
||**related :**|**functionally complete :**|
|`a\|b`|`commutative`|`are mutually exclusive`,`nonconjuction`,`alternative denial`|`~(a.b)`, `a/b` *schaffer stroke*, `a\|^b`, `a^^^b`, `(a.b)^^-`, `-__(a.b)`|`NAND`|
|`a\/^^-b` *Webb operation, Peirce arrow*|`commutative`|`are completely excluded`,`nondisjunction`,`joint denial`|`~(a\/b)`, `a___b`, `a\|vb`, `a\/^^-`, `(a+b)^^-`|`NOR`|
||
||**related :**|
|`a(.)b`|`commutative`|`equivalent`, `if, and only if`, `material biconditional`, `bidirectional implicative`, `biimplication`, `bientailment`|`a==b`, `a<->b`, `a<=>b`, `a<>b`, `\|iff`, `(a\/^^-)^^-`|`XNOR`,`NXOR`,`XAND`|
|`a(+)b`|`commutative`|`inequivalent`, `exclusive disjunction`, `exclusive alternation`|`a===b`, `a\|\|\|b`, `a=/=b`, `a</>b`, `a</=>b`, `a</->b`, `a<=/=>b`,`a<-/->b`, `a\|/b`, `a\/__-b`|`XOR`|
|`a=/)b`|`noncommutative`|`fails to imply, logically`, `does not imply`|`a-/>b`, `a=/>b`, `|? `NIMPLY`|
|`a(=b`|`noncommutative`|`is implied by`|as above||
|`a(/=b`|`noncommutative`|`fails to be implied by, logically`, `is not implied by`|as above||

|Set Notation|Negation|Narration|Discarded Alternatives|
|-|-|-|-|
|`{}`||empty set|`(/)`|
|`{a,b}`||a set containing elements a and b ( Cantor )|
|`{a\|b(a)}`|| set-builder notation; set of as defined by property b ( Cantor )|
|`a'` ||"a prime" : the complement of set a ( Cantor )|`a^c` ( Cantor )|
|`a <= b`|`a </= b`| a is a proper subset of b|
|`a => b`|`a =/> b`| a is a proper superset of b|
|`a <e= b`|`a </e= b`| a is an element of b|`a <E b`, `a (E b`, `a </E b`, `a (/E b`, `a (E' b` |
|`a =e> b`|`a =e/> b`| b is an element of a|
|`a <_= b`|`a </_= b`| a is a subset of b|
|`a =_> b`|`a =_/> b`| a is a superset of b|
|`a \|v\| b`||union of a and b|
|`a \|^\| b`||intersection of a and b (Cantor)|`a ^ b`, `a /\b`, `a /set\ b`|
|`a /_\ b`||  "xor", "symmetric difference" of a,b; "elements in a, or b, but not not in both a and b"; "disjunction union" "set sum"|`a \|del\| b`, `a (+) b`|
|`\|omega\|` ||  ω : "first finite ordinal"; order type of the natural numbers ( Cantor )|
|`\|ALEPH\|_a`|| א :aleph-a : transfinite numbers of order a|`A_a`, `A/_a`, `N_a`|
|`#a`||cardinality of a ; number of elements in a ( Lua, maths )|`\|a\|`|
|`P(a)`||    power set of a|

|Code|Glyphs|Predefined
|-|-|-|
|`\|ALPHA\|`,`\|alpha\|`|Αα|
|`\|BETA\|`,`\|beta\|`|Ββ|
|`\|GAMMA\|`,`\|gamma\|`|Γγ|γ is Euler's constant ( Julia )|
|`\|DELTA\|`,`\|dekta\|`|Δδ|
|`\|EPSILON\|`,`\|epsilon\|`|Εε|
|`\|ZETA\|`,`\|zeta\|`|Ζζ|
|`\|ETA\|`,`\|eta\|`|Ηη|
|`\|THETA\|`,`\|theta\|`|Θθ|
|`\|IOTA\|`,`\|iota\|`|Ιι|
|`\|KAPPA\|`,`\|kappa\|`|Κκ|
|`\|LAMBDA\|`,`\|lambda\|`|Λλ|
|`\|MU\|`,`\|mu\|`|Μμ|
|`\|NU\|`,`\|nu\|`|Νν|
|`\|XI\|`,`\|xi\|`|Ξξ|
|`\|OMICRON\|`,`\|omicron\|`|Οο|
|`\|PI\|`,`\|pi\|`|Ππ|π is Archimedes' constant ( Julia )|
|`\|RHO\|`,`\|rho\|`|Ρρ|
|`\|SIGMA\|`,`\|sigma\|`|Σσς|
|`\|TAU\|`,`\|tau\|`|Ττ|
|`\|UPSILON\|`,`\|upsilon\|`|Υυ|
|`\|PHI\|`,`\|phi\|`|Φφ|φ is golden ratio ( Julia )|
|`\|CHI\|`,`\|chi\|`|Χχ|
|`\|PSI\|`,`\|psi\|`|Ψψ|
|`\|OMEGA\|`,`\|omega\|`|Ωω|
||
|`\|ALEPH\|`|א| ALEPH is the base for transfinite numbers|
|`\|e\|`|e|Euler's number, Napier's constant ( Julia )|
|`\|G\|`|G|Catalan's constant ( Julia )|
|`\|\|`,`\|\|`|
|`\|\|`,`\|\|`|
|`\|\|`,`\|\|`|
|`\|\|`,`\|\|`|
|`\|\|`,`\|\|`|

|Binary Operators|
|-|
Each operator is typed, for safety. `A mechanism should be available for overloading, or reimplementing, the operator's interface.`|
|We may need to overload operators as they have `declarative` and
`stateful` semantics which may be different. |

|Commutative Operators|
|-|

|Code|Alphabetised|Returning||Code|Alphabetised|Returning|
|-|-|-|-|-|-|-|
|`+`|`are added to`|`their sum`||``|``|
|`*`|`are multiplied by`|`their product`||``|``|
|`=`|`are arithmetically equal with`|`a boolean`||``|
|``|``||``|``|


|Non-commutative Operators, WITH and WITHOUT Convenience Twins|
|-|

|Code|Alphabetised|Returning||Code|Alphabetised|Returning|
|-|-|-|-|-|-|-|
|`<`,`>`|`is less than`,`is greater than`|`a boolean`||`-`|`is subtracted by`|
|`=<`,`>=`|`is less than, or equal to`,`is greater than, or equal to`|`a boolean`||`:`|`is the head of a sequence, whereas its tail is`|
|`/`,`\`|`is fractionally divided  by`,`fractionally divides`|`the quotient`|
|`/%`,`%\`|`is integer-wise divided  by`,`integer-wise divides`|`the signed remainder`|
|``,``|``,``|
|``,``|``,``|
|``,``|``,``|
|``,``|``,``|

|First-class Entities|
|-|
|This section needs to talk about how objects work, also. JavaScript
uses auto-boxing to methodise immutable primitives. |

|Primitives|Qualifier||
|-|-|-|
|`undefined`|`?`|
|`definednull`|`?`|
|`boolean`|`combine into 4-logic?`|
||
|`string`|`utf8`|
|`atom`|`utf8`|e.g. Prolog, Erlang, Elixir, Scheme|
||
|`number`|Erlang or JS? floats as IEEE 754-2008 seems normal |
|`bigint`|Erlang or JS? library? e.g 1324234n syntax|
|`arbitrary precision`|Julia wraps GNU GMP,MPFR|

|Enums|
|-|
|`utf8` characters (? codepoints? )|
|`ASCII` characters (as numbers?)|
|``|
|``|
|``|
|``|
|``|

|Data Structures|
|-|
|`records` or `map` of some sort|
|`linked list` of some sort|
|`raw array` of some sort|
|`smart array` of some sort|
|See Erlang's built-ins for other common types|
|There should be some sort of UI for configuring customisable memory
layouts of customised datatypes|

|Uses of the Underscore Character '_'|
|-|
|`variable names : ?[_a-zA-Z]*[_a-zA-Z0-9] or quoted utf8`|
|`visual spacer in sequences of digits`|

|Uses of the Exclamation Character '!'|
|-|
|`exception handling` : to be defined|

|Generics|
|-|
|`compile time` vs `runtime` : as yet undefined|

|Preprocessor Transformations|
|-|
|`allow enumerated passes`|

###### Top-level Syntax
```
build: env:
    from: fs.something
    from: lib.something
    from: src.something

run: block: with env: with type: signature
    ensure: condition
    blocky block block code
```

###### Block Syntax 
```
this is a block_d0
    this is a block_d1
        this is a block_d2
    this is a block_d1

{   this is a block_d0
    {   this is a block_d1 
        { this is a block_d2 } }
    {   this is a block_d1 }
}

{   run, env ENVIRONMENT, type SIGNATURE : this is a desugared block_d0
    {   run, env <INHERIT>, type UNDEFINED : this is a desugared block_d1 
        { run, env <INHERIT>, type UNDEFINED : this is a desugared block_d2 } 
    }
    { run, env <INHERIT>, type UNDEFINED : this is a desugared block_d1 }
}

{ purpose, context CONTEXT, caveat CAVEAT : body }
{ purpose, arguments ARGUMENTS, guards GUARDS: body }
{ :purpose, arguments ARGUMENTS, guards GUARDS: body }
{| purpose, arguments ARGUMENTS, guards GUARDS | body }

```

### SURVEYS

Assignment
```
a:=b            assignment, from b to a ( ALGOL 1958, Pascal, Python, Smalltalk )
a=b             ( Superplan 1949, CPL->BCPL->B->C->etc.  )
a<-b            ( APL 1962, Haskell 1996, R )
LET a=b         ( BASIC 1964 )
let a=b in a+1;;( Ocaml )
let a=b         ( Haskell )
    in c=a
```
AVOID OVERLOADING
= : comparison, assignment X
```

a+=b
a-=b
a*=b
a/=b
a%=b    CHECK MULTIPLE MEANINGS
a**=b
a//=b
a>>=b
a<<=b
etc.

```

AVOID IMPLICIT PRECEDENCE HIERARCHY
Evaluation sequence should follow explicit spatial rules

PROVIDE a canonical english name for each non-alphabetical lexeme; opt
in.

USE WHITESPACE for IFS / INTERNAL FIELD SEPARATION

USE HEREDOCS for nice matrice alignments

USE Erlang/Ada-style BASE#DIGITS.DIGITS#eEXPONENT notation up to base-36 [a-zA-Z0-0] ;
underscore as visual separator ( Ada, 1979 )

DIRECTIONAL INFIX OPERATORS should have symmetrical counterparts

CONSIDE: moderately sized stdlib, absorbing commonly used patterns

CONSIDER: Erlang's == /= for numbers, =:=, =/= for terms, and === for
reified identity

CONSIDER : 'protocols/extension' vs interfaces/implementation ( SWIFT )

CONSIDER : syntax blocks ala markdown

CONSIDER : double-quote-characters represent an escaped quote characters, in any string literal ; this requires lookahead, does it slow things down too much?

CONSIDER : representation of numbers, exponential, rational, complex, machine type
etc.

```
variablex := <<<here
[ 0     1,000   45  ]
[ 1     0       29  ]
[ 1     1       6   ]
here
```
Brackets & Quotes
```
{}  for sets
()  for tuples
[]  for matrices
<>  ? spare! or wait till group theory
``  ? text
''  ? atoms?
""  ? spare!
```
Numerals
```
0xa     indicates that a is a base16 number ( C lang )
0Xa     ditto

0a      indicates that a is a base8 number ( C lang )
0oa     ditto ( Python, Rust )

0ba     indicates that a is a base2 number ( C lang )
0Ba     ditto

        ... above are quite standardised across languages, but may throw
        off new/non-programmers

a|b     ? indicates b is a number in base a
b_a     ?

a,a     ? comma separated numerals 
        ( dangerous if commas have other meaning )

a_a     ? underscore separated numerals

0ra     ? roman numerals

|N~0    ? set of natural numbers, beginning with 0
|N      ? set of natural numbers, beginning with 1
|Z      ? set of integers ( from Zahlen )
|Q      ? set of rational numbers ( from Quotient )
|P      ? set of irrational numbers
|R      ? set of real numbers
|C      ? set of complex numbers

|X~0    ? X excluding 0  ( consistent with Peano ~ )
|X>0    ? X greater than 0
|X<0    ? X less than 0

|Z_p    ? integers modulo p ( advanced )

|inf    ? infinity
|oxo|
```


Piping / Composition
```
a o b   "ring operator" ( maths, Standard Meta Language (SML)  )
a<o>b   "the composition of a and b" : (a o b)(x) = (a(b(x))
        maths::"composition"

a|->b   maths:"reverse composition"
        computing:"composition / piping"
        pipe output from a, as input to b 
a.b     ditto ( Haskell )
a|>b    ditto ( R 4.10, Elixir )
a%>%b   ditto ( R magrittr package )

a^b(x)  functional power ( maths: varies by domain )
        a^2(x) = ( a o a )(x)
        a^3(x) = ( a o a o a)(x)
        where b is negative: a^b(x) = 1/a^|b|(x)
```

Lists
```
[head|tail]     ( Erlang )
head:tail       ( Haskell )
```

Tuples
```
(a, b)  ? math : finite, ordered : distinguished from sets
v[a b]  ? vector

a<<b    append b to tuple a     ( Ruby )
a>>b    prepend a to tuple b

push / pop / shift / unshift?

ARRAYS vs TUPLES ?


SEE BITWISE SHIFTS
```

Matrices
```
a x b   row x column
        follows Cartesian convention ( horizontal, vertical )
a__     ? a is a matrix ( from double underline )
a||     ? a is a matrix

M(m,n)      the set of all m-by-n real matrices

M_(m,n):(R) ?   the set of all m-by-n matrices, over the field or ring R
M(m,n,R)    ?
M_(n):(R)   ?   for square matrices, remove redundant dimension
M(n,R)      ?

Mat         may be in place of M

|M|         determinant of M

(X,Y,Z) = (a,  b,  c  )(x,y,z)   ( Cayley ) 
          |a', b', c' |
          |a'',b'',c''|

TODO : See BQN's matrix notation

a(x)b       multiplication "matmul"

a(x)b   "cross product", "vector product" : a vector
a(.)b   "dot product", "scalar product" : a scalar
        special case of inner product

<a,b>   inner product ( LEARN THIS FIRST )
(a,b)
<a|b>
(a|b)

a(x)b           outer product / tensor product
a(x)_outerb     not the dot product
                not the kronecker product
                not standard matmul

a(x)_kromb      kronecker product

```

Indexing / Keying
```
a[b]        bth item in a ( C-fam )
a_b         ( maths )

a[b][c]     ... depending on language, the outer-most container is
            indexed on the far-left or far-right
a_b,c

a_i1,i2... + b_i1,i2... = c_i1,i2... 
representing a set of many equations

See C array addressing where equivalently : 
    *(base + i)     "pointer form"
    base[i]         "array indexing form"
    i[base]         ditto

SEE : Namespace Referencing
SEE : Memory Pointers 
```
Error
```
_|_     "bottom" throws an error ( Haskell )
```

Functions
```
a(b)=c                                      ( maths )
a:b->c                                      ( maths )

\a->b                                       ( Haskell )
fun a->b                                    ( caml )
a=>b                                        ( JavaScript )
(a)=>{return b}                             ( JavaScript )
function a(b){return c}                     ( JavaScript )
def a(b):c;                                 ( Python )
fun a(b)=c                                  ( ML )
fn : a->b                                   ( ML )
a := [ :b | c = b+1 ]                       ( Smalltalk )
|arg1:type1, arg2:type2| -> type3 { body }  ( Rust )
|arg1:type1, arg2:type2| body               ( Rust )


a(b1) when g1 ->c1;     ( Erlang )
a(b2) when g2 ->c2;

a b | g1 = c1           ( Haskell )
    | g2 = c2

CONSIDER :
- ordered parameters
- named parameters
- default parameter arguments
- destructuring of monoidal argument into multiple arguments
```

Namespaced Referencing
```
a->b        ( C++, PHP )
a.b         ( C, JavaScript, Haskell, rust )
a b         ( Smalltalk )
a::b        ( C++, Haskell )

SIMPLIFIED FIELD INITIALISATION SYNTAX ( Haskell, Rust, JavaScript )
```

Evaluation
```
a.          ( Prolog, Erlang )
a()         ( C ) 
a value.    ( Smalltalk )
```

Subexpressions
```
a,b,c.      ( Prolog, Erlang )
a,b,c;      ( C, JavaScript )

```
Control
```
a?b             if a then b

a?b:c           if a then b else c "ternary operator" 
a->b,c          origin : ( CPL )
a??b!!c         ( Raku )
if(a,b,c)       ( VB.NET )
b if a else c   ( Python )

a??b            "nullish coalescing" if a is falsy, return b ( JavaScript, C# )
a?.b            "ignore missing" continue silently if b is falsy ( JavaScript )
a??=b           "ensure assignment" assign b to a, if a is falsy ( JavaScript )

a?:b            "elvis" : if a is truthy, return a, else b = a ? a : b

a$b             "right precedence" ( Haskell ) ( HOW ? )

for..in..where {}       ( Swift )
switch..case..where     ( Swift )
etc.                    "where guards are true"

( Erlang )
<guard_sequence>    ::= <guard> ";" <guard_sequence> 
<guard>             ::= <guard_expression> "," <guard>
guard_sequence  : true if ANY guard is true
guard           : true if ALL guard_expressions are true 

a(b) when condition -> functionbody (Erlang )

case a of       ( Erlang )
    b1 -> c1;
    b2 -> c2
end.

case a of       ( Haskell )
    | b1 = c1
    | b2 = c2
    where   g1
            g2
```

Bitshift
```
BYTES ARE JUST TUPLES

a<<b    shift bits in a, left by b positions
a>>b    shift bits in a, right by b positions

a<@b    shift bits in a, left by b positions, rotationally / circularly
aROTLb
a<<<b   CHECK symmetry ( no unsigned left shift BECAUSE ... )

a>@b    shift bits in a, right by b positions, rotationally / circularly
aROTRb
a>>>b   unsigned right shift ( Java )

```

Logic
```
E|a     ? there exists at least one a
E]a     ?

E|!a    ? there exists one, and only one, a
E]!a    ?

A|a     ? for all a
\-/a    ?
\/a     ?

!a      NOT gate    ( B lang, C lang; conflict with Peano )
~a                  bitwise : ( C family, JS, etc. )
a^^-

a/\b    AND gate    ( conflict with Peano )
a&b                 bitwise : ( C family, JS, etc. )
a&&b
a.b

a/\^^-b NAND gate
a^^b
a|b
a|^b
(a.b)^^-
-__(a.b)

a\/b    OR gate     ( similar/same with Peano
a|b                 bitwise : ( C family, JS, etc. )
a||b

a\/^^-  NOR gate
a__b
(a+b)^^-
-__(a+b)
a|vb

a(.)b   XNOR gate "equivalent"
a-__\/b
a\/^^-b

a<=>b   IF, AND, ONLY IF
a<>b    ditto
|iff    

a\/__-b XOR gate "nonequivalent"
a-^^\/b
a(+)b
a^b

a<=/=>b
a</=>b
a</>b
a<-/->b

a=>b    implication ( similar to Peano ) "IMPLY"
a->b    ?

a<=b    converse
a<-b

a=/>b   nonimplication "NIMPLY"
a</=b

a-/>b   converse nonimplication
a</-b

a|b     a such that b ( similar to set builder notation )
a:b     ditto
[]      "tombstone" : end of proof
#       ditto

Russell-Peano notation :
~a      negation of a
a\/b    logical sum of a,b
a.b     logical product of a,b
a=)b    implication of b, by a

Consider :
a===b   equivalence of a,b;
a|||b       a=)b.b=)a
|-a     "turnstyle" : assertion of a's truth
        a,b,c|-d = a \/ b \/ c => d

a.a=)b.=)b  inference by modus ponens

```

Model Theory
```
a|=b        "double turnstyle" ; Model a makes true predicate b
            a,b,c|=d = entailment without provability

soundness       "system does not produce invalid conclusions"
completeness    "system can produce all valid conclusions"
```
Set Notation
```
{a, b}      a set containing elements a and b ( Cantor )

{}          empty set
(/)         ?

{a|b(a)}    set-builder notation
            set of as defined by property b ( Cantor )

a'      "a prime" : the complement of set a ( Cantor )
a^c     "a's complement" ( Cantor )

PROPER :

a(=b    a is a subset of b; b is a superset of a
a(set b ?
a <e b

a (/= b
a ~(= b
a (/set b
a ~(= b
a </e b
a ~<e b

a=)b    a is a superset of b; b is a subset of a
a set)b ?
a e> b

IMPROPER : 

a=(=b
a=)=b
a(=_-b
a=)_-b
a =<e b
a e>= b
a <=set b
a >=set b

a ~<=set b
a ~>=set b
a </=set b
a >/=set b

a U b   ? union of a,b ( Cantor ) 
a|v|b
a\set/b ?
a\/b    ?
a&b     ( Python )

a|delb  "symmetric difference" of a,b
a(+)b   "elements in a, or b, but not not in both a and b"
a/_\b   "disjunction union" "set sum"

a ^ b   ? intersection of a,b ( Cantor )
a|^|b
a/\b    ?
a/set\b ?
a|b     ( Python )

w       "first finite ordinal"
        order type of the natural numbers ( Cantor )

A_a     ? transfinite numbers of order a 
|A_a    ?
A/_a    ?

|a|     cardinality of a ; number of elements in a
        ( STRONGLY AVOID )
#a      ditto ( STRONGLY PREFER )   ( Lua, maths )

P(a)    power set of a

a (E b  ? a is an element, in set b (einai=is, adopted by Cantor )
a (e b  ?
b E) a  ?
b e) a  ?

a <E b
a E> b
a <e b
a e> b

a ~<E b
a ~E> b
a </E b
a E/> b

a ~<e b
a ~e> b
a </e b
a e/> b

a (Z b  ? a is not an element, in set b
a (~ b  ? 
a (E' b ? perhaps too discreet
a (/E b ?
a (~E b ?
a (~e b ?
```

Algebra
```
(a+b)*c precedence grouping ( HOW TO RECTIFY WITH TUPLES ? )
a+b     summation / addition of a,b

a*b     multiplication of a,b; addition of a, to a, b times
a(x)b   ?
a(b)    ?

a-b     subtraction of b, from a
a/b     division of a by b; subtraction of b, from a, RESULT times
a\b     for symmetry

a//b    "floor divide" : returns quotient, discards remainder

a%b     modulo OR remainder (CHECK PYTHON VS C TODO )
a%%b        CHECK


a/%b    ?
a\%b    ? for symmetry
a/&b
a\&b

a-/b    ? ath root of b

a^b     a to the bth power
a**b

a::b        ? proportion ( earliest )
a:-:b:-:c   ? geometric proportion
a|cx|b      ? linear proportionality, without detail :
                a=kb, without defining k

+/-     plus or minus
|a|     absolute value

a/\b    SPECIALISED : "exterior product" "wedge sum"
```


Angles
```
L\      "right angled triangle" ?
<)      "arc"?
(>

<)abc   angle of points a,b,c
/_\abc  triangle which is a,b,c
a~=b    congruence between a,b
a~b     similiarity between a,b ( how similar? )
a_|_b   perpendicularity of a,b
a||b    parallelity of a,b

a(o)    a arc degrees = a 360ths of a circle
a^o     
a|deg
<)adeg

a'      a arc minutes = a 60ths of an arc degree
a^|
a(|)
a|'
a|min
<)a'

a''     a arc seconds = a 60ths of an arc minutes
a^||
a(||)
a|''
a|sec
<)a''

a|rad

a(o)b'c''
a(o) b' c''
a^ob'c''
a^o b' c''
a|degb|minc|sec
a|deg b|min c|sec
adeg|bmin|csec
adeg|bmin|csec
<)a^o b' c''
<)a|deg b|min c|sec
<)a|rad

|\t   ooht   sin t ( opposite / hypotenuse )
_\t   aoht   cos t ( adjacent / hypotenue )
|_t   ooat   tan t ( opposite / adjacent )
     
\|t   hoot   csc t ( hypotenuse / opposite )
\_t   hoat   sec t ( hypotenuse / adjacent )
_|t   aoht   cot t ( adjacent / opposite )

a|><|b  line segment from a to b

a|>>|b  ray starting from a, extending through b
a|<>|b  line, infinitely passing through a, b







```

Derivatives
```
a'(x)   "a prime of x" : first derivative of a, with respect to x
a''(x)  "a prime prime of x" : "second derivative of a, with respect to x

a(x)=x' "x prime" : the result of a transformation

\/      "nabla" : gradient in vector calculus
\/.     divergence : vector calculus
\/x     curl : vector calculus
```

Factorial
```
a!      a-factorial
        3! = 3 x 2 x 1
```

Equality
```
~=      approximately equal ( how approximate? )
=/=     not equal
===     identically equal

<=>     "spaceship" : returns -1,0,1 depending on lt/et/gt
=~      regex match ( Perl )
```



Big Greek Letters
```
|int    long small s : integral symbol ( Leibniz )
|Sig    big sigma : summation of sequence
|Pi     big pi : multiplicative product of sequence ( Gauss )
|Om     big omega : various context
|Del    big delta : change or difference
|Phi    big phi : golden ratio, etc.

... I think the following can be replaced with list comprehensions 

|Sig__from^^to:term     ? maths
                        e.g. |Sig__let i=1^^10:i^2+i

|Sig[above;right;below;left]        CSS convention : clockwise
|Sig{top:;bottom:;left:;right:;}                   : explicit

|Sig__(below)^^(above)<<left>>right      ? ( similar : LaTeX, relsize package )

|Sig^^(top)>^(righttop)         ? cartesian convention : xy(do this)
>>(right)>_(rightbottom)
__(bottom)<_(leftbottom)
<<(left<^(lefttop)

|Sig#^(top)>^(righttop)         ? cartesian convention : xy(do this)
>#(right)>_(rightbottom)
#_(bottom)<_(leftbottom)
<#(left<^(lefttop)
```
Letters
```

Greek :

|alp
|bet
|gam
|del
|eps
|zet
|eta
|the
|iot
|kap
|lam
|mu
|nu
|xi
|omi
|pi
|rho
|sig
|tau
|ups
|phi
|chi
|psi
|ome

Norwegian :

|ae
|(/)
|a^o

Other influential character sets may be considered, but they appear less
frequently in English texts even those of math and science.

```

Common Entities
```
|pi     pi, Archimedes' constant
|i      -/-1
|e      Euler's constant; base on natural logarithm
```

Structural Query Language
```
S   ? select
F   ? from
W   ? where
J   ? join
IJ  ? inner join
OJ  ? outer join
U   ? update
D   ? delete
@   ? as

etc.
```

Relational Model
```
relation        : SQL "table"
heading         : SQL "schema" : list of attributes : (name, type "domain")
tuples          : SQL "rows" : but must be unique per relation
attribute       : SQL "columns"
degree/arity    : column count
cardinality     : row count
relational variables / relvars : existentiation of "data in a table"
constraints     : validation rules
candidate key   : smallest SUBSET of attributes, guaranteed to uniquely
                    differentiate each tuple in a relation
superkey        : proper superset of a candidate key, whose tuples are also
                    unique
foreign key     : ( CHECK ) 
projection      : the set of tuples from a relation, filtered for a
                    subset of its attributes
```


List Comprehensions
```
a..b        expansion : integer a to b
a:b         ( Julia )

a.+b        element-wise mapping ( Julia )
            [1,2,3] .+ [0.1,0.2,0.3] => [1.1, 2.2, 3.3]
            [1,2,3] .+ [0.1,0.2,0.3] => [0.9, 1.8, 2.7]

az/b        "reduce" "insert" ( APL ) "foldl" ( Haskell )
            with dyadic/binary operator z
            +/[1,2,3,4] => [10]
            */[1,2,3,4] => [24]

az/[x]b     ditto by the xth axis
            with dyadic/binary operator z

az\b        "scan" "cumulative reduction" ( APL )
            with dyadic/binary operator z
            +\[1,2,3,4] => [1,3,6,10]
            *\[1,2,3,4] => [1,2,6,24]

az\[x]b     ditto by the xth axis
            with dyadic/binary operator z

Distinction between SETS and CLASSES should allow GENERATORS ...

a:=:b       swap values : "exchange" ( Icon )
a~>b        morphism from a to b ( Scala ), which defines a class

[expression||qualifiers1..N]                ( Erlang )
#{keyexpression=>valueexpression||qualifiers1..N}
<- and <:-
<<bitstringexpression||qualifiers1..N>>
<= and <:=

[X||X<:-SOURCELIST,X>3] where the first expression is a generator
[{X,Y}||X<-SOURCE,Y<-SOURCE] will create a cartesian product
[{X,Y}||X<-SOURCE && Y<-SOURCE] will zip the lists

STRICT guards crash on match failure; RELAXED guards continue silently


[expression for item in iterable if condition] ( Python )

collection[start:stop:step] ( Python )
                            [::-1] walks a list in reverse 

see various list-wise functions : map, fold, acc etc.

a/\b        sort a on b, ascending
a\/b        sort a on b, descending
            ... sort in place?
            ... output sorted index only?
```

Pattern Matching
```
erlang, rust, haskell, ocaml, swift

"where" : declarations or statements

TODO : destructuring operators like ... in JS


unbound = source    will throw exception if unmatched ( Erlang ) 


native BNF ; consider SNOBOL
native REGEXC ; consider AWK

=~      regex match ( Perl )
!=~     negated 

```

OOP / Types
```
a<3b        a is instance of b ( see elementof in SETS )
```

Side Effects / Unboxing / variable binding
```
a>>b>>c         execute in sequence ( Haskell )
do{a;b;c}       restyled as C

a>>=( \b->      Form 1
c>>=( \d->
b+d ))
do{ b<-a;       Form 2 : equivalent, different syntax
    d<-c;
    b+d;}       

ISSUE : <- and -> are not symmetrical

```
Memory Pointers
```
Review C * and &

a<-     returns address which points to a
->a
a<?
?>a
@a      ( Pascal )

a@?     "at" : a in the context / space of ?
?[a]

a->     returns value at address a
<-a
a>?
?<a
a^      ( Pascal )


?@a     "at" : ? in the context / space of a
a[?]

```

Types, Kinds, Classes, hinting
```
a:b     variable a is of type b ( Rust )
a::b    ( Haskell )
```

Requested
```
replaces n%m==0 : returns true if perfectly divisible

walks a list in reverse instead of python's [::-1]

string-wise ( already under LIST-wise ? )

currying / partial application ... to allow pipinGg to work for N-adic
functions

re-UI all of Haskell's weird rubegoldberg fns

see Haskell &

as : cast
in : elementation
is : equivalence OR subtyping

```

SEE J LANG
- API : https://code.jsoftware.com/wiki/NuVoc
- https://sergeyqz.github.io/jcheatsheet/
- also APL in English :
  https://www.jsoftware.com/papers/APLDictionary.htm

# Overview

This section aims to quickly tour you through the following concerns :

-   How is a computer PROGRAM stored as SOURCE CODE?
-   How is SOURCE CODE transformed into MACHINE CODE? ( very briefly )
-   HOW is MACHINE CODE executed by the MACHINE?

## Source Code : form and meaning

#### The term 'character' is technically imprecise.

-   MACHINE-LEVEL STORAGE QUANTIFIERS
    -   BIT : a mark : "machine grapheme" : a minimal datum/"unit of
        information" : datum means "what is given" : storage atom
    -   CODE UNIT : for a system, a system-word constituting a certain
        number of atoms: "sized segment" : an assembly of BITS : e.g. in
        UTF-8, this is 8 bits; in UTF-16, this is 16 bits : storage
        quantum
-   HUMAN-LEVEL SEMANTIC QUANTIFIERS
    -   CODE POINT : "human grapheme" : for a system, a number
        representing the index position of a unit of meaning : may be
        stored across multiple CODE UNITS : "grapheme, linguistics" :
        semantic atom
        -   ( Unicode uses this medium to encode information about how
            CODE POINTS interact with each other, such as the concepts
            of ['zero-width non-joiners' and 'directional
            overrides'](https://stackoverflow.com/a/27331885/1378390) )
    -   GLYPH : a visual entity, which is likely to be perceived as a
        fundamental unit of information by a naive reader : e.g. the
        combination of a diacritic and a letter : "grapheme cluster" in
        Unicode, which may represent a SEQUENCE OF CODE POINTS :
        font-wise atom 
    -   FONT : a collection of glyphs

#### Programming languages typically mean CODE POINTS when referring to
'characters'

-   TOKENS : delimited sequences of CODE POINTS
-   EXPRESSIONS : sequences of TOKENS which abide by the rules of a
    GRAMMAR
    -   MACHINE-WISE, NON-NORMATIVE : "comments"
    -   MACHINE-WISE, NORMATIVE : "code" : this includes identifiers
        like filenames : to identify is to establish a norm of
        reference.
        -   DECLARATIONS : more ABSTRACT, less concrete
            -   abstract RULES / RELATIONS : IMPLICATIONS
                -   identifiers 
                -   exceptions / allowances
                -   disallowances / prohibitions
            -   reified OBJECTIVES without determinant methodology
        -   STATEMENTS : more CONCRETE, less abstract
            -   OPERATIONS : "ethics" : changes across time
                -   MACHINE INSTRUCTION SETS : at the lowest level
            -   OPERANDS : "metaphysics" : entities in N-space
                -   MACHINE STORAGE HIERARCHY : at the lowest level
                    -   while C can hint with `pragmas` and `register`,
                        it is probably best to focus on minimising
                        re/assignment of values, and just let the
                        compiler figure out memory hierarchy traversal

## Machine Code : structure

Conventional Mappings from EXPRESSIONS to OPERANDS

### OPERATING THEATRE

#### PROCESSORS : processors are the hardware which execute
INSTRUCTIONS.

-   MOTHERBOARDS : a motherboard may host multiple CPUs.
    -   CENTRAL PROCESSING UNITS : each CPU is a single chip/ integrated
        circuit.
        -   PHYSICAL CORES : each CPU may have multiple processing
            units/ physical cores.
            -   LOGICAL CORES : PHYSICAL CORES may time-slice their
                work, to capitalise on idleness, and so each PHYSICAL
                CORE may be presented to the KERNEL as two separate
                LOGICAL CORES/ hyperthreads ( more than two, is deemed
                impractical ).

#### MEMORY HIERARCHY

-   PRIMARY/ MAIN MEMORY : broadly refers to REGISTERS, CACHES, random
    access memory (RAM), and read-only memory (ROM), close to a
    computer's CPU.
    -   REGISTERS : fewest and fastest, each is tied to a PHYSICAL CORE.
    -   CACHES : less small, less fast, maybe shared between multiple
        [physical cores] on the SAME CHIP
    -   RAM : least small, least fast, shared by multiple chips on the
        SAME MOTHERBOARD
    -   ROM : typically for fundamental instructions.
-   SECONDARY MEMORY : broadly refers to external storage, such as hard
    disk drives (HDDs), solid-state memory drives (SSDs & USB flash
    drives), network attached storage (NASs), and optical drives (CDs,
    DVDs, etc.) which hold LARGE amounts of data and transfer it SLOWLY.

#### OPERATING SYSTEM KERNEL, as a SUPERVISOR 

: the KERNEL refers to the core control system of what is commonly
referred to as the OPERATING SYSTEM ( which may broadly refer to many
other things as well ).

##### OPERATING SYSTEM PROCESSES 

: a kernel will set up rudimentary environments which enable a PROGRAM
to use the CPUs. These are called PROCESSES and may have access to
multiple CPUs on a motherboard, and multiple physical CORES on each CPU. 

-   PROCESSES typically do not share their accessible memory with other
    processes : this is managed by the kernel. 
-   PROCESSES may elect to be PINNED TO A PHYSICAL CORE, or a LOGICAL
    CORE, but this is ultimately controlled by the KERNEL.
-   The KERNEL may have authority to INTERRUPT, to PAUSE and RESUME, or
    to KILL a running PROCESS.

##### OPERATING SYSTEM THREADS 

: a PROCESS may encapsulate the work it wants to run on a single LOGICAL
CORE, as a THREAD.  THREADS in the same PROCESS may share their
accessible memory. A PROGRAM can thus split its work across the THREADS
of its PROCESS.

-   THREADS may also elect to be PINNED TO A PHYSICAL CORE, or LOGICAL
    CORE, and again this is controlled by the KERNEL.
-   As many PROGRAMS may have simultaneous access to many CPUS and many
    CORES, the KERNEL will typically run a SCHEDULER which assigns
    THREADS to LOGICAL CORES, to get work done as quickly as possible.

##### VIRTUAL PROCESSES, and VIRTUAL THREADS 

: any PROGRAM may have its own internal encapsulation of work, divided
into nominal processes and threads, with varying semantics. This is
common for PROGRAMS which are VIRTUAL MACHINES, presenting a particular
computing architecture. For the most part the KERNEL would be ignorant
of these, interfacing only with their parent OPERATING SYSTEM THREADS
and PROCESSES.

### OPERATIONS

#### PROGRAMS 

: machine code, "a program/ routine/ procedure", is a graph (network) of
INSTRUCTIONS.  

-   The graph may be cyclic. The graph may be composed of disconnected
    subgraphs.  The nodes are instructions, and the graph may be
    visualised as if the instructions are written on paper, ala the
    canonical Turing machine. 
-   A machine executing the instructions can be said to have a reader/
    cursor/ eye/ finger/ pointer which is spatially "at" one instruction
    at a time.  Instructions are processed discretely. Instructions may
    instruct the machine to jump to any addressable instruction in the
    program. At the hardware level, this refers to a PROGRAM COUNTER, a
    REGISTER containing the MAIN MEMORY ADDRESS of the NEXT INSTRUCTION
    TO BE READ.
-   Beyond telling the reader to jump here and there within the program,
    the program will have side-effects, whereby the machine follows the
    program's instructions to do other things.  In some cases, these
    side-effects may include mutation of the program's own instructions.

#### FUNCTIONS 

: functions/ subroutines/ methods/ subprocedures are simply subprograms.

-   Functions may serve to encapsulate complexity, by way of spatially
    demarcating instructions which may be accessed according to a
    specific protocol/ interface/ set of parameters.
-   Calling/ applying/ running a function therefore requires definition
    of a meta-protocol that defines what must precede the machine jump
    TO the function's body, what happens while the machine is evaluating
    the function body, and what happens after the machine's evaluation
    of the function body is complete, particularly determining where the
    machine jumps to next.

### OPERANDS

-   DATA : is a STATE of memory; memory is a medium capable of
    state mutation.
-   LOCALITY OF REFERENCE : is the LOGISTICAL concept of keeping
    often-used DATA on the closest and fastest-to-access MEDIA

#### MEMORY ALLOCATION PATTERNS

-   Each PROGRAM may allocate memory for its work. The STACK and HEAP
    are two most common structures that are allocated by PROGRAMS for
    general work.
    -   STACK : a contiguous region of memory, with a LIFO access
        pattern.
    -   HEAP : a contiguous region of memory, with a RANDOM ACCESS pattern.
-   Other patterns such as POOLS, ARENAS, etc. may be employed, each
    containing multiple stacks, heaps, or other structures.

#### EXECUTION OF PROGRAMS/ SUBPROGRAMS

-   Programs usually require out-of-order references to various DATA.
    This data is typically grouped together, and referred to as an
    "activation record" / "environment of bound variables" / "stack
    frame", and placed on the STACK, though the HEAP may be used for
    larger data. 


# Surveys

# Survey : Explicit Env Passing

"stack frame", "activation record", "map of bound variables"

https://www.reddit.com/r/ProgrammingLanguages/comments/1kw1yhz/which_languages_allowrequire_explicit_management/

###### Notable Response : Bla

https://www.reddit.com/r/ProgrammingLanguages/comments/1kw1yhz/comment/munaly2/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

https://strlen.com/bla-language/

###### Notable Response : Kernel ( a Scheme )

https://www.reddit.com/r/ProgrammingLanguages/comments/1kw1yhz/comment/muflh1t/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

Link for anyone curious :
https://ftp.cs.wpi.edu/pub/techreports/pdf/05-07.pdf

Page 14.

"Operands to Kernel operatives are passed unevaluated, together in each
call with the dynamic environment from which the call is made. The
operative therefore has complete control over operand evaluation, if
any."

( continue )

# Perhaps Interesting

-   "[文言, or wenyan, is an esoteric programming language that closely
    follows the grammar and tone of classical Chinese literature.](https://wy-lang.org/)"
-   [Warning, link has no TLS, has 3 videos of :](http://calmeca.free.fr/calculmecanique_php/rubriques/Fichiers_Blaise_Pascal/Fichiers_technique/Pascaline_sautoir.php?lang=eng)
    1.  How clockwork -> computers
    2.  The information is stored as MASS, the accumulator builds up MASS
        as potential energy before releasing it
    3.  This allows the CARRY operation ( 01 + 09 => 10 )
    4.  I guess this is what those clowns are building in minecraft
        calculators ...
-   "[Lambda Diagrams are a graphical notation for closed lambda terms,](https://tromp.github.io/cl/diagrams.html)"
-   "[lambda-8cc - An x86 C Compiler Written in Untyped Lambda Calculus](https://github.com/woodrush/lambda-8cc)"
-   "[Computational Life: How Well-formed, Self-replicating Programs
    Emerge from Simple Interaction](https://arxiv.org/abs/2406.19108?)"
    - the paper is more interesting than the abstract ... it is based on
      [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck)
    - https://github.com/loophp/combinator

- https://five-embeddev.com/articles/2023/03/20/c-hardware-access/

#### Memory Management

https://verdagon.dev/grimoire/grimoire

#### Combinators

https://writings.stephenwolfram.com/2020/12/combinators-and-the-story-of-computation/
https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird

S-combinator in JavaScript :
```javascript
s = x => y => z => x(z)(y(z))   // defined

s(a=>b=>a+b)(c=>c+1)(1)         // applied
                                // result : 3

s(a=>b=>a+b)(c=>c*3)(10)        // applied
                                // result : 40
```
