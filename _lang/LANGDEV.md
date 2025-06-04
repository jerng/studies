# DRAFT

## Decisions
-    source code files implicitly form block?

## Lexemes

Being very literal : 
-   any new modern programming language, should provide implementations of many, if not all low-level [data structures](https://en.wikipedia.org/wiki/List_of_data_structures) which fit within its guarantees ( not too dangerous, not
    too slow, etc. )
    -    doing so transparently may allow the user to assemble compound data structures, from the rudimentary built-in ones
-   Here are some `linear` data structures
    -   at the bottom, `tuples of bits` can represent `registers` and `words`; tuples of `words` can represent `cache lines`, `chunks` etc.
        -   matrices are also representable as `tuples`
            -   (A) where `tuple` values are of equal size, a `rectangular array` is a fitting C implementation
            -   (B) otherwise, `arrays of pointers` will have to do?
    -   the `linked list` data structure ( IPL, 1956 ) has remained influential after its preponderance in LISP, 1958 ( `cons`, `car`, `cdr` ); this is not done in the v8 javascript engine; but it is natively done in Erlang, Haskell, Lisp, and OCaml, for example
        -   In Lisp, the canonical definitions hew towards the following being equivalent : `F`, `()`, `NIL`, representing boolean falseness and the empty list ... it is probably a bad idea to persist this pattern in a new language which isn't trying to be a traditional Lisp
-   Here are some popular 'non linear' data structuers
    -   `heap` i.e. `priority queue` implemented as a **balanced** binary search tree 
-   Numbers
    -    `Floats` are implemented using the `IEEE 754 double precision` standard, in various common languages including C, Julia, Go, OCaml, Java, and JavaScript : but not Erlang, or Haskell : it is probably best to go with `IEEE 754`
    -    `BigInt` is not standardised in implementation across languages; perhaps we should just consider JavaScript's implementation, since we are in this playground
-   Human Readable Characters
    -   `utf8` is the common format, which is also aligned with `ASCII` at 8-bits
    -   `bitstrings` is thoroughly implemented in Erlang, and provides ONE good example, however the alternatives need to be thought out a bit more, since these are so rare
        -  erlang goes with `data:size/typeSpecifierList`
-   consider
    -   the following types ( * consumes more space, if stored in the type, besides/instead of in the compiler )
        -   blocks, untyped (compiler knows the start address, and length* )
        -   blocks, typed (compiler knows the start address, type size* , and type count* )
        -   tuples (ordered, redundant)
            -   non-adjustable : total size is pre-allocated ( compiler knows the start address, start address* and lengths* of subsequent elements )
            -   adjustable : block of pointers ( compiler knows the start address, and length*, and optional pointer* to the next block of pointers )
        -   sets (unordered, unique) : hash table ( detail? )
        -   linked lists (ordered, redundant)
            -   singly-linked (awkward queues)
            -   doubly-linked (agile queues)
        -   CONTINUE HERE : WRITE COMPLETE EXAMPLES
            - 1 dimensional
            - \>1 dimensional
            - internally complete
            - dependent on compiler ( saves runtime space, but reduces runtime flexibility )      

###### Datatype Design considerations

-    underlying hardware architecture
    -    `pointers on 64bit systems are 64bits long, and respectively for 32bit systems` : a moderate, and practical, assumption; the evolution of growth in system-word-sizes has been mainly motivated by how much memory can be addressed with a single word. On 64bit systems one can compile with 32bit pointers, limiting a process' RAM and increasing the number of pointers you can stuff into cache, but that is an optimisation, not the baseline.
    -    `cache lines on 64bit systems are 64bits wide` : another moderate, and practical, assumption
-    `an 8-byte pointer, for a 1-byte datum` is the minimum addressable quantum, without dropping into bit-wise addresses ( which would need more address space, or a reinterpretation of the 64bit address space to be 32bits for the byte address, and 8bits for the bit address in the byte )

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
||
|Consider : ||runes, Arabic letters, other Hebrew letters, Old Italic Scripts ( various adapted from antiquity , International Phonetic Alphabet |
|`\|\|`,`\|\|`|Ææ|"air" in Englishy : from Dannish / Norwegian alphabet |
|`\|\|`,`\|\|`|Øø|"oohr" in Englishy : Dannish / Norwegian alphabet|
|`\|\|`,`\|\|`|
|`\|\|`,`\|\|`|
|`\|\|`,`\|\|`|



| Lexical Structures||
|-|-|
|`BOF` `EOF`|non-characters, merely used the notation of source code|
|`{...}`| delimiter, *maths* : set notation; unordered, unique |
|`(...)`| delimiter, *maths* : tuple notation; matrices are just nested tuples; ordered, redundant; probably the best place to start modelling data structures, i.e other data types : byte = `(8* bit)`etc.<br>Besides that, it is common for programming languages to use parentheses as `lexical delimiters` to group expressions at compile time. |
|`[...]`| delimiter, ? |
|`<...>`| delimiter ? |
|`\`...\``| delimiter ? |
|`'...'`| delimiter ? |
|`"..."`| delimiter ? |
||
|`DD...DD`, `DDD...DDD`| prior rows show delimiters as `D...D` but it maybe further desirable to use increase the variety of delimiter tokens simply by repeating the basic delimiters; it is possible of course to define any string of characters as a delimiter, but that might become overwhelmingly cumbersome later, unless the delimiters are very clear, for example `|BEGIN|` `|END|`, such tokens as defaults should be chosen judiciously if at all |
||
|`sigils` : are on a separate plane, allowing reuse of nearly all delimiters |
||
|`head:tail`, `head::tail`, `head|tail`| these are example of `cons` operators in Haskell, OCaml, and Erlang, respectively ; it seems feasible to apply this syntax to a `stack` linear data structure, also|
|`head:body:tail`, `head::body::tail`, `head|body|tail`| fancifully, head-tail syntax seems extensible to head-body-tail, for `doubly-linked-lists` and `unprioritised queues`|


|Character Sets|Domain|Details|
|-|-|-|
|`%.*$`|`comments`| beginning at `%` and ending at the end of the line; there are no block comments; this reduces one branch of decisions |
|`base_10_digits` `#` `underscore_separated_digits` `.` `undescore_separated_digits` `#` `e` `exponent`|`numeric literal`|base 1 to 36|
|`--` `type` `opening_delimiter` `utf8_string` `closing_delimiter`|`sigils`|Compile time or run time branching?|
|`{...}` `[...]` `(...)` `<...>`  |`sigil asymmetrical delimiters`|
|`[^-\s{}[]()<>]`|`sigil symmetrical delimiters`|no escapes?| 
|`?[_a-zA-Z]*[_a-zA-Z0-9]` or `quoted utf8`|`variable names`|

|Uses of the Underscore Character '_'|
|-|
|`variable names`|
|`unused variable` compiler hint|
|`visual spacer in sequences of digits`|

|Uses of the Exclamation Character '!'|
|-|
|`exception handling` : to be defined|


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

### Domain Specific Lexemes

###### List Phrasing
>    Can all iteration be unified with list comprehension?

|`1`|`1`|`M`|M|M|`1`|`1`|
|-|-|-|-|-|-|-|
|`[`|`input`|`fanout`|`map`|`fanin`|`output`|`]`|
|-|-|-|-|-|-|-|
|-|`0-to-N`|`N-to-P`|`P-to-P`|`P-to-Q`|`Q-to-0`|-|
|-|`pipe from source`|`unfold`|-|`fold`|`pipe to sink`|-|
|-|`generator expression`|`increase`|-|`reduce`|`consumer expression`|-|
|-|-|`infer`|-|`filter`|-|-|

visual mocks :
```
%% [ generate | fold | map ] 
[0..5|+|i^i+1]
[0..5|SUM|i^i+1]

%% [ generate | map | fold ] 
[0..5|i^i+1|+]
[0..5|i^i+1|SUM]
```
###### Axial Phrasing
visual mocks :
```
%% for comparison with list phrasing
[0..5|+|i^i+1]
[0..5|SUM|i^i+1]

%% Maths :
|SIGMA|__0^^5>>i^i+i
|SIGMA|__'0'^^'5'>>'i^i+i'
|SIGMA|__(i,0)^^(i,5)>>'i^i+i'
|SIGMA|__'i=0'^^'i=5'>>'i^i+i'
|SIGMA|__(i,=,0)^^(i,=,5)>>((i,^,i),+,i)
|SIGMA|__(i,'=',0)^^(i,=,5)>>(('i',^,'i'),+,'i')

%% Markdown :
||5
||SIGMA|i^i+1
||0

%% Markdown Pattern :
|TopLeft|Top|TopRight|
|Left|Axis|Right|
|BottomLeft|Bottom|BottomRight|

```
|Grapheme|Meaning|
|-|-| 
|`axyc`|`a` is for axis, `x` is for horizontal indication, `y` is for vertical indication, `c` is for content |
|`a^^c`|`c` is spatially North of `a`, to the Top of `a`|
|`a>^c`|`c` is spatially NorthEast, to the TopRight of `a`|
|`a>>c`|`c` is spatially East of centre, to the Right of `a` |
|`a>_c`|`c` is spatially SouthEast of centre, to the BottomRight of `a`|
|`a__c`|`c` is spatially South of centre, to the Bottom of `a`|
|`a<_c`|`c` is spatially SouthWest of centre, to the BottomLeft of `a`|
|`a<<c`|`c` is spatially West of centre, to the Left of `a` |
|`a<^c`|`c` is spatially NorthWest of centre, to the TopLeft of `a` |

### blocks / subroutines / functions / procedures / methods

applicative style :
```
e = c(b(a))(d)
```

tacit style :
```
d b c 
```

## Types

|Hinting,Declaration|
|-|-|
|`THING/type`, `THING::type`, `THING:type`, `THING<type>`, `type THING` | Erlang, Haskell & Rust, Python & TypeScript, C++, C, respectively |

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
||
|`function` |hopefully just one type of function|

|First-class Entities|
|-|
|This section needs to talk about how objects work, also. JavaScript
uses auto-boxing to methodise immutable primitives. |
||
|`any primitive`|
|`any object` : blocks, data structures, compiled regular expressions, unexecuted source or byte or machine code |
|`tail calls`|
|`continuations`|
|`environments`|
|``|

### Boxes / Containers 

|Data Structures|-|
|-|-|
|`records` or `map` of some sort|
|`linked list` of some sort|
|`raw array` of some sort|
|`smart array` of some sort|
|See Erlang's built-ins for other common types|
|There should be some sort of UI for configuring customisable memory
layouts of customised datatypes|


|Generics|
|-|
|`compile time` vs `runtime` : as yet undefined|

|Preprocessor Transformations|
|-|
|`allow enumerated passes`|


## Operations

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


|Enums|
|-|
|`utf8` characters (? codepoints? )|
|`ASCII` characters (as numbers?)|
|``|
|``|
|``|
|``|
|``|


|Structured Query Language|-|
|-|-|
|`S`|   ? select|
|`F` |  ? from|
|`W` |  ? where|
|`J`   |? join|
|`IJ`  |? inner join|
|`OJ` | ? outer join|
|`U`   |? update|
|`D`  |? delete|
|`@`  | ? as|
|||
