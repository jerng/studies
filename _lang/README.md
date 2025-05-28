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

## Memory Management

https://verdagon.dev/grimoire/grimoire













