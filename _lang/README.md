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
# Operator Innovation

https://www.reddit.com/r/ProgrammingLanguages/comments/ya87l1/what_operators_do_you_wish_programming_languages/

https://en.wikipedia.org/wiki/Glossary_of_mathematical_symbols

https://www.cuemath.com/numbers/math-symbols/

AVOID OVERLOADING
= : comparison, assignment X
```
a:=b    assignment, from b to a ( ALGOL, Pascal, Python )
```

AVOID IMPLICIT PRECEDENCE HIERARCHY
Evaluation sequence should follow explicit spatial rules

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

|N0     ? set of natural numbers, beginning with 0
|N1     ? set of natural numbers, beginning with 1
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

Tuples
```
(a b)   ? math
v[a b]  ? vector

a(x)b   "cross product", "vector product" : a vector
a(.)b   "dot product", "scalar product" : a scalar
```

Functions
```
a:B->C  ( maths )
\a->b   ( Haskell )
```

Logic
```
E|a     ? there exists at least one a
E]a     ?

E|!a    ? there exists one, and only one, a
E]!a    ?

A|a     ? for all a

!a      NOT gate    ( B lang, C lang; conflict with Peano )
a\/b    OR gate     ( similar/same with Peano
a/\b    AND gate    ( conflict with Peano )

a=>b    implication ( similar to Peano )
a->b    ?

a<=>b   IF, AND, ONLY IF
a<>b    ditto

a|b     a such that b ( similar to set builder notation )
a:b     ditto
```

Russell-Peano notation :
```
~a      negation of a
a\/b    logical sum of a,b
a.b     logical product of a,b
a=)b    implication of b, by a

Consider :
a===b   equivalence of a,b;
a|||b       a=)b.b=)a
|-a     assertion of a's truth

a.a=)b.=)b  inference by modus ponens

a (E b  ? a is an element, in set b (einai=is, adopted by Cantor )
a (e b  ?
b E) a  ?
b e) a  ?

a (Z b  ? a is not an element, in set b
a (~ b  ? 
a (E' b ? perhaps too discreet
a (/E b ?
a (~E b ?
a (~e b ?
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

a(=b    a is a subset of b; b is a superset of a
a(set b ?

a=)b    a is a superset of b; b is a subset of a
a set)b ?

a U b   ? union of a,b ( Cantor ) 
a\set/b ?
a\/b    ?

a ^ b   ? intersection of a,b ( Cantor )
a/\b    ?
a/set\b ?

w       "first finite ordinal"
        order type of the natural numbers ( Cantor )

A_a     ? transfinite numbers of order a 
|A_a    ?
A/_a    ?

|a|     cardinality of a ; number of elements in a

P(a)    power set of a

```

Arithmetic
```
a+b     summation / addition of a,b
a*b     multiplication of a,b; addition of a, to a, b times
a-b     subtraction of b, from a
a/b     division of a by b; subtraction of b, from a, RESULT times

a%b     modulo OR remainder (CHECK LANGUAGES)
a-/b    ? ath root of b
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

```

Derivatives
```
a'(x)   "a prime of x" : first derivative of a, with respect to x
a''(x)  "a prime prime of x" : "second derivative of a, with respect to x

a(x)=x' "x prime" : the result of a transformation
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

```

Big Greek Letters
```
|int    long small s : integral symbol ( Leibniz )
|Sig    big sigma : summation of sequence
|Pi     big pi : multiplicative product of sequence
|Om     big omega : various context
|Del    big delta : change or difference
|Phi    big phi : golden ratio, etc.

... I think the following can be replaced with list comprehensions 

|Sig__from^^to:term     ? maths
                        e.g. |Sig__let i=1^^10:i^2+i

|Sig[above;right;below;left]        CSS convention : clockwise
|Sig{top:;bottom:;left:;right:;}                   : explicit

|Sig__(below)^^(above)<<left>>right      ? ( similar : LaTeX, relsize package )
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
