# DRAFT

## Latest

-    UNDONE :
     -     [`APL/J analogues`](https://code.jsoftware.com/wiki/NuVoc), `proof assistant features / go read about it`
     -     |General error||
     -     |`let ... in ... ` `... where ...`||`to be unified with a bind syntax`|
     -     |[`BNF`](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form)||Context-free grammar notations|
     -     | data structures : interfaces vs implementation |
     -     `hwloc` vs `data structure configuration`
-    UNASSIGNED : 
     |` `|`<-- -->`|`<== ==>`| `->> <<-`| `<<= =>>`| 
     |-|-|-|-|-|
     |`\|-> <-\|`|`\|=> <=\|`|`:-> <-:`|` `|` `|
     |`;`|`\|\|`|`&&`|`::`|` `|
-    BROADLY :
     -   IMPLEMENTATION STRATEGY :
         -   Stage 1 : `JavaScript`
         -   Stage 2 : `C++`
     -   STYLING TENDENCIES :
         -   `Erlang's lexical style` ( desc. Prolog )
         -   `Haskell's currying semantics` ( desc. ML, ? )
         -   `C++'s containerisation flexibility` ( desc. C )
         -   `Idioms of utility` from : `JavaScript, Python, Erlang, J ( desc. APL )`
         -   `Manual memory` management : this is far, far away from the present, but I guess we should consider how `C++ and Rust` do things
         -   `graphical transliteration of logic notation, maths notation, etc. into ASCII`
     -   STRATEGY :
         -   `define lexing rules at the top-level context`,
         -   `define lexing rules which enter and exit subcontexts`,
         -   `define lexing rules for each subcontext`
     -   TOP LEVEL MODE :
      
          
|domain|char|designation|
|-|-|-|
|[`expressions`](#program-sources-consist-of-formal-expressions)|`non- normative`|`comments`|
|||`propositions` : <br> - `a priori` : true/ false based on rule definitions <br> - `a posteriori` : true/ false based on values at a memory address|
||`normative`|`operations` : resulting in mutation of values in memory addresses |
|analogs|`,`| additive connotation, from English ( C family, Erlang, etc. )|
||`.`| terminal connotation, from English ( Prolog, Erlang, COBOL )|
||`<-` `=>`|`travels to` `points at` `and then, in sequence or logic`|
|specified|`%%...EOL` `<%...%>`| comments | 
||` ` `\n` `\t`|- `IFS` internal field separator; whitespace should be regarded as significant <br> - in the interest of simplifying grapheme differentiation, the lexer should not have to guess too much about ambiguous grapheme-/ word-boundaries |
|`quotes`|unassigned |`         ` `         ` `         ` <br> `  ``...``  ` `  ''...''  ` `  ""...""  ` <br> ` ```...``` ` `         ` `"""..."""`|
||`` `...` ``|`backticks` are allowed in variables/symbol names and atoms|
||`'...'` <br> ala JS template literal strings|- single quotes : `\'` is the escape sequence for embedding the quote character <br> - `${...}` is the escape sequence for embedding code <br> - it is probably simplest to adopt JS's default UTF16 string semantics for the most common cases <br> - and Erlang's bitstring syntax later; Erlang's sugared-string linked-lists may be relevant under the 1LL data structure |
||`'''...'''`|- triple-single quotes : `indentation aware` see Erlang's triple quotes ( or other fancy features )|
||`"..."` <br> lexical modifier| - `a infixFunction b c` can be converted to prefix such that `"infixFunction" a b c` <br> - `prefixFunction a b` can be converted to infix such that `a "prefixFunction" b` <br> - Haskell uses two separate operators for this, `(infixFunction)` `` `prefixFunction` ``|
|||- other uses?|
||see `data structures`|- `(...)` `{...}` `[...]` `-[...]-` `+{...}+` `+[...]+` `*[...]*` <br> - `<` namespace : `<qr:...:qr>` form, `<q...q>` form, `<x>` form, `<xyz>` form |
|`\|` `:`|`analogs`|`where` `whereas` `such that` `onlyif` `and`|
||`\|`|`strict` : it is illegal to introduce lexical ambiguity : lexer should not have to guess about missing IFS `(TODO : example)`, and will quit upon ambiguity|
|||- `{ x \| P(a) }` set builder notation <br> - `[ head \| tail ]` singly-linked-list notation <br> - `-[ head \| body \| tail ]-` doubly-linked-list notation
||`:`|`sloppy` : it is legal to introduce lexical ambiguity : lexer may introduce implied IFS, for example where `(:::)` expands to `(undefined:undefined:undefined:undefined)`, `(::::atom)` is `(undefined : undefined : undefined : :atom)`, and `(:::atom)` being ambiguous MAY hurl a lexing error, not being specified to, but out of charity. Therefore other design decisions about `:` should enable contextual deduction of missing IFS |
|||- `fn name : signature : parameters : body end` `(:::)` function notation <br> `:valid_atom` <br> - `+{ key : value }+` JS-eque POJO notation - <br> `<<value:size/typespecifierlist>>` <br>- `<qr:...:qr>` form |
|specified|`value^Type` `Type^value`|type association, as opposed to `:` in OCaml, Python, `::` in Haskell, July, `type value` in C, `type<value>` in C++|
||`_`|- explicitly ignored `value`;|
|||- see `number` |
||`<` and `>`|- MANY|
||`=` `<-`|`Single Static Assignment`: LHS HANDLE points to a memory ADDRESS, whose FIRST AND ONLY value shall be RHS VALUE; ala maths ; assigns from RHS to LHS only|
||`:=`|`Destructive Assignment` : LHS HANDLE points to a memory ADDRESS, whose FIRST OR NEW value shall be RHS VALUE; ala ALGOL; disallow `=:` |
||`)=`|`Application & Destructive Reassignment` : LHS HANDLE points to a memory ADDRESS, whose NEW value shall be ( RHS FUNCTION applied to LHS's OLD value ); graphical reference to `)>` ; disallow `=(`|
||`.`|- `parent.child`: (valid_symbol)subscript, for subscoping : `container.index` `parent_container.child_container` `parent_type.child_type` `parent_env.child_env` ala JS, as opposed to `::` and `->` from C++|
|||-see `number`||
||`!`|reserved for future use as a `communications operator` : from Erlang's messaging operator, but with diversified use to include `throwing exceptions`, etc.|
||`true`, `false`, `null`, `undefined`| four-logic|
||e.g. `` `SIGMA` __0 ^^5 >>i^i+i`|axial phrasing `^^` `>^` `>>` `>_` `__` `<_` `<<` `<^` where `xy`-axis|
|pointers|`data_at_addr <(x)`|unambiguous dereferencing of pointer|
||`addr_of_data <(x)`|unambiguous indirection to data|
|charsets|`valid_symbol` `:valid_atom` |-  ``[`a-z]`` first character (Haskellism), `[_@a-zA-Z0-9]` middle characters (Erlangism), and ``[`@a-zA-Z0-9]`` last character <br> - symbols beginning with `` ` `` must have a minimum of `3` characters <br> - OK : `a` `` `aa `` `` `a` `` `` a` `` `a_b` `a@` `a0` <br> - ERROR : `` `a `` `0a` `_a` `a_` `@a` <br>- also see `<x>` and `<xyz>` patterns <br> - `:valid_atom`: literals, not symbols, in the Erlang style; notation ala symbols in Ruby, CommonLisp, Julia ... no further quoting is needed for disambiguation. <br> - no option to quote for more characters <br> - future : quotes for charset expansion? Unicode?|
|`ASCII -IPA`||https://en.wikipedia.org/wiki/Kirshenbaum|
|audio synthesis|| - https://github.com/lichen-community-systems/Flocking|
||`types / kinds / classes`|`[A-Z]` first character (Haskellism), `[_@a-zA-Z0-9]` middle characters (Erlang rules), `[_@a-zA-Z0-9]` last character
|numbers||`base_10_digits` `#` `underscore_separated_digits` `.` `underscore_separated_digits` `#` `e` `exponent`, base 1 to 36, from Erlang / Ada|
||`rune`|synonym for `Int32` in Golang|
||`byte`|synonym for `Uint8` in Golang|
||`_`|`1_000_000` digit spacer, `mycube_[3]_[2]_[4]` `[13,44,22]_[0]` box-address spacer <br> WARNING:UNSURE_GOOD|
||`<)` `(>`| sigil for arc values `∡∢`|
||`$.`|sugar for the respective ASCII code point as an integer ( from Erlang )|
||`\|(algebraic entity)\|`| modulus, also `abs( algebraic entity)`; `()` are required to avoid collision with `\|ordinary_variable\|` |
||`a..b`|`a(+1)..b` `a(a=>a+1)..b` `a (+1) UNTIL b`|
||`++` `--`|`de/increment and return OR the converse` are sugar for `i=i+/-1`, `j=i;i=i+/11;j` ; the sugar INTRODUCES subtlety ... not sure if this is worth keeping; for example FP-style SSA just does away with this entirely|
||`a + b`|a added to b, a plus b|
||`a - b`|b subtracted from a, a minus b|
||`a * b`|a multiplied by b, a times b|
||`a / b` `b \ a`| `quotient` of : a fractionally-divided by b |
||`a /int b` `b int\ a`| `quotient` of: a integrally-divided by b |
||`a /rem b` `b rem\ a`| `remainder` of: a integrally-divided by b |
||`a -/ b`| vinculum, ath root of b `√`|
||`a ^ b `| - superscript, a to the bth power; not to be confused with `value^Type` `Type^value`, which the lexer should deduce |
||`a _[b]`| - ([0-9].*)subscript, for subscoping : bth member of a ( 0-based indexing ) : idiomatically applies to ALL data structures <br> - ala JS, SEE : `parent.child` (valid_symbol)subscript <br> - do not confuse with : `[b]` a singly-linked list; `_` the pattern of ignorance  |
||`a )= function`| Application & Destructive Reassignment, where `a += b` would be `a )= + b` |
|||TODO : logarithms|
||||
||||
||||
|`operational` logic||for general computing (? what do you call this)|
||`and` `AND`|no synonymity with `&&`, which is an unassigned symbol|
||`or` `OR`|no synonymity with `||`, which is an unassigned symbol|
||`not` `NOT`|no synonymity with `!`, which is an operator for communcations; no synonymity with `~` which presents as bitwise `@~`|
||`in` `not in` `IN` `NOT IN`|SEE : data structures|
||`is` `is not` `IS` `IS NOT`|strict identity, like `<=>` `</>` in `order-0` logic; like `===` `!==` in JS |
||`eq` `not eq` `EQ` `NOT EQ`|equivalence, like `==` `!=` in JS |
|`bitwise operator prefix`|`@`|bitwise operations are `domain specific` given that they are mainly used in the computing context to regard `bits` and `bytes` and `words`. Here we avoid the conventional `\|` `&` `~` as these operators will be used in a more general context. Following the style of Erlang, bitwise operators all have a common prefix. [Swift docs are pretty](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/advancedoperators/). <br> - complicated : different operators in various languages are overloaded for different underlying data types|
|`logical` `unsigned`|`@` `@OR`||
||`@&` `@AND`||
||`@~` `@NOT`||
||`@^` `@XOR`||
||`@&^` `@CLEAR`|See Golang `AND NOT` : `a &^ y = a & ~y`|
||`@<` `@USL`|left shift |
||`@>` `@USR`|right shift|
|`circular` `rotary`|`@@<` `@CSL`|left shift |
||`@@>` `@CSR`|right shift|
|`arithmetic` `signed`|`@$<` `@SSL`|left shift |
||`@$>` `@SSR`|right shift|
|`order-0` `propositional` `boolean` logics|truth tables?|`NAND ⊼` `NOR ⊽` `XAND` : MORE CHECKING NEEDED|
||`a \| b` `OR`|`a \/ b` vee|
||`a & b` `AND`|`a /\ b` wedge|
||`~a` `NOT`||
||`a </> b` ?`UNLESS` |not equivalent, `a \/_ b` `⊻` veebar, `a (+) b` oplus|
||`a <=> b` `IFF` |equivalent, `a IFF b` if and only if |
||`a <= b` `BECAUSE`|a because b, `a -\| b`, `a \./ b` `∵`| 
||`a </= b` ?`DESPITE`|a not because b, `a -/\| b` | 
||`a => b` `THEREFORE`|a therefore b, `a \|- b`, `a /.\ b` `IMPLY` `∴`|
||`a =/> b` ?`NEVERTHELESS`|a does not imply b, `a \|/- b` `NIMPLY`|
||`_\|_` `FALSE` |bottom, `false`, `F`|
||`^\|^` `TRUE` |top, `true`, `T`|
||`\|-`|single turnstyle|
||`\|=`|double turnstyle ( [difference](https://www.quora.com/What-is-the-difference-between-Turnstile-and-double-Turnstile-in-Logic) ) |
|`order-1` `predicate` logics|e.g.|`<E>(x,y):A(x),B(y),other_condition`, `<A>z:X(z)`|
||`<A>`|for ALL, all, universal quantification, `\-/` `∀`|
||`<E>`|for SOME, there exists, existential quantification `∃`|
||`<E>!`|there exists exactly ONE, unique quantification `∃!`|
|`order-N` `higher order` logics|e.g.|`<E>x<A>y:F(x) NAND B(y) AND <A>z:NOT Q(z) AND Y(x)` WARNING:NOT_SURE_IF_GOOD_MODEL|
|`model logic`|`[]<E>x:P(x)`|it is necessary that something has property P `□` ; `modal` operator for “necessary”@`alethic`, “provable”@`provability`, “obligatory”@`deontic`, “believed”@`doxastic` ... `logics`|
||`<><E>x:P(x)`|it is possible that something has property P `◇`; `~[]~` not necessarily not, in most modal logics|
|`fn` ||sigil for quote ( as in Lisp `QUOTE` ) , may be conceptually sound : a function is just a block you quote now, and maybe run later|
||Signature only |`(name : TypeA TypeB TypeC TypeD)` <br> `fn name : TypeA TypeB TypeC TypeD end`|
||Function only |`(name : arg1 arg2 arg3 : body )` <br> `fn name : arg1 arg2 arg3 : body end`|
||Oneline Signed Function : pretty, returns final term| `( name : TypeA TypeB TypeC TypeD : arg1 arg2 arg3 : body )` <br> `fn name : TypeA TypeB TypeC TypeD : arg1 arg2 arg3 : body end` |
||e.g. Shortest Unsigned Function, returns `undefined`|`(:::)` `fn ::: end`|
|`iteration`|| - general form : `while expression : performance_with_optional_state_modification end` <br> - general function : `fn whiledoloop : (Int->Bool) Fun Int (Int->Int) : condition performance init_state mod_state : if not condition<(init_state) : :done : performance, whiledoloop <( condition performance mod_state<(init_state) mod_state ) end end` |
|`selection`|simple `if`|`if expression : branch_if_true : branch_if_false end`|
||complex `if`|`case expression : pattern and condition -> branch_if_matched : default_branch end `|
||||
||||
||||
|Data Structures|Unassigned Symbols | RULE : symmetrical opening/ closing tags <br> `    ` `-{}-` `*{}*` `#{}#` `... @$%^` <br>  `    ` `    ` `    ` `#[]#` `... @$%^`<br> `    ` `-()-` `*()*` `#()#` `... @$%^` <br> `    ` `-<>-` `*<>*` `#<>#` `... @$%^`
|`()` 1|| tuples, `cpp structr` unless they fix `cpp std::tuple`, CONSIDER : <br>  -   `mytuple_[5]_[17]` <br>  -   `mytuple.5.17` <br>  -   `mytuple_5_17` <br>  -   `mytuple_(5)_(17)` |
||| - for associative resolution ( lexing or parsing ); giving it this sort of explicit role is quite important! <br>- Reason : the `output of lexing`, is a `single nested tuple of strings`, so adding parentheses in source code are just hints / more explicit input for the lexer <br> - `lexical_block<(operand)` for function application <br> - `a b c` `a,b,c` `(a b c)` `(a,b,c)` `(((((a),(b),(((c)))))))`: all equivalent, `except in special contexts` |
|`[]`|| nilist, the protolist  |
|`[]` 2/LL || singly linked lists, `cpp std::forward_list`, CONSIDER : `[ n * 2 FOR n IN <N~0> WHERE n < 10 ]` |
|`-[]-` 3/LL || doubly linked lists, `cpp std::list` |
|`{}` 4||ZFC sets, `cpp std::unordered_set`, CONSIDER : builder notation <br>  -   `{ x \| predicate(x) }`  <br> -   `{ expression(x) FOR x IN iterable WHERE }` <br>  -   `{ x<e<N~0> \| x<10 /\ x/%2==0 }` <br> -   `{ x <e <N~0> WHERE x < 10 AND x /% 2 == 0 }` <br> `{ x \| x <e N and x modulo 2 = 0 }`|
||`a <* b` `b *> a`|a is an element of b, derived from `<e` epsilon for *Latin : est* "is a" element-of boolean operator, Peano `∈ ∋` |
||`a </* b` `b */> a`|a is not an element of b `∉ ∌`|
||`a <= b` `b => a`|a is a PROPER subset of b `⊂ ⊃` |
||`a </= b` `b =/> a`|a is not a PROPER subset of b `⊄ ⊅`|
||`a <_= b` `b =_> a`|a is a subset of b `⊆ ⊇`|
||`a </_= b` `b =_/> a`|a is not a subset of b `⊈ ⊉`|
||`{}`|empty set `(/)` `∅`|
||`{a,b}`|a set containing elements a and b ( Cantor )|
||`{a\|b(a)}`| set-builder notation; set of as defined by property b ( Cantor )|
||`a'` |"a prime" : the complement of set a ( Cantor ) `a^c` ( Cantor )|
||`a \|_\| b`|union of a and b `∪`|
||`a \|^\| b`|intersection of a and b (Cantor)`a ^ b`, `a /\ b`, `a /set\ b` `∩`|
||`a /_\ b`|  "xor", "symmetric difference" of a,b; "elements in a, or b, but not not in both a and b"; "disjunction union" "set sum"`a \|del\| b`, `a (+) b` `△`|
||`` `omega` `` |  ω : "first finite ordinal"; order type of the natural numbers ( Cantor )|
||`` `ALEPH`_a ``| א :aleph-a : transfinite numbers of order a `A_a`, `A/_a`, `N_a` `ℵ_a`|
||`#a`|cardinality of a ; number of elements in a ( Lua, maths ) `\|a\|`|
||`P(a)`|    power set of a|
|`+{}+` 5/JS || JavaScript objects, `cpp std::unordered_map`, `mypojo.prop1` `mypojo['prop1']` `mypojo[integer_would_be_coerced_to_string]`|
|`+[]+` 6/JS ||JavaScript arrays, `cpp std::unordered_map`, `mypoja[99]`|
|`*[]*` 7/JS || JavaScript's ArrayBuffer + TypedArray, `cpp std::vector or std::array`, later swap this to a non-JS API if there are gains to be had |
|`<...>` | |namespace for SIGILS / QUOTES|
|`<q...q>`|Unassigned Symbols| `quote` : where q is a single character : `<!...!>` `<@...@>` `<#...#>` `<$...$>` `<%...%>` `<^...^>` `<&...&>` `<=...=>` `<_..._>` `<+...+>` `<]...[>` `<[...]>` `<{...}>` `<}...{>` `<\|...\|>` `<\.../>` `</...\>` `<"...">` `<'...'>` `<~...~>` `<?...?>` |
||Symbols used elsewhere|`<*` `*>` for *est* element-of boolean operator <br> `->` pattern-matched branching (`<-` is unused) <br> `<)` `(>` values of arc <br> `<(` `)>` function application <br> |
||`<%...%>`|`multi-line comment`|
|8|`<<...>>`|- bitstring syntax from Erlang `<<value:size/typespecifierlist>>` <br> - WARNING : possible collision with `<<` `>>` for axial phrasing |
||`<asm: ... :asm>`|reserved for `assembly languages`|
|`<qr:...:qr>`|Unassigned Symbols| `quote` : where qr is >1 character|
|`<x>`||`symbol` not `quote` : where x is exactly 1 character|
|`<xyz>`||`symbol` not `quote` :where x and z are not the same character; and y is >0 characters, none of which are `:`|


### CONSIDERATIONS
             -   `,`
                 - in `expression sequences` : [C-fam](https://en.wikipedia.org/wiki/Comma_operator) : a binary operator that executes the LHS and discards the result, then executes the RHS 
                 - in `function calls for arguments` 
             -   `.` `;` `()`
         -   PATTERN MATCHING :
             -   Semantics should match Erlang's ( simplest ! ) see : `term`, `pattern`, `_`, `compound pattern operator` 
             
-    -    `!` error
-    `??` nullish coalescing ( see JavaScript )
-    `a ??= b` ensure assignment of b to a, if a is falsy ( see JavaScript )

   
-    `ZFC-sets` : `{}`, `{{}}`, `{0}`, `{0,3,null,(),['a','bb',3,undefined]}`,
     -    `{ x | x <e N and x modulo 2 = 0 }`, assume affinity with `cpp std::unordered_set`,
     -    `a <= b`, `a =/> b`, `a e> b`, `a </e b`, `a =_> b`, `a </_= b`
     -    
-    `tuples` contiguous in memory : `()`, `(0,3,null,(),['a','bb',3,undefined])`,
     -    `<(124,4,55)>` bitstrings copied from Erlang, or Rust, or Go?,
     -    `cpp struct` unless they have fixed the bug in `cpp std::tuple`?
     -    punning / sugaring :
          -   `(a,b,c)` is the unsugared form, `evaluate a, then b, then c`
          -   `a,b,c` is this viable, as sugar?
          -   `c b a` what about this? `put c on the stack, then b, then a`
-    `singly or doubly linked lists`
     -    `[]` nilist, `[head:tail]` singly, `<[head:body:tail]>` doubly,
     -    `[ not_the_empty_list:[] ]` implicit nilist at CDR position of a singly linked list,
     -    `<[ []:not_the_empty_list:[] ]>` implicit nilist at HEAD and TAIL position of final element WARNING:NOT_SURE_IF_GOOD_MODEL
     -    `[ x | { x | x <e N AND x MODULO 2 = 0 } | pipeable | pipeable | terminal_pipeable ]` WARNING:NOT_SURE_IF_GOOD_MODEL,
     -    `[0,3,null,(),['a','bb',3,undefined]]`
     -
-    `$` any JavaScript analog
     -    `cpp std::unordered_map` : ${ a:1, b:2, ${ something:'else'}, f:44, h:88 }`, `${lazyAssign}`
     -    `cpp std:vector` : `$['asd', 'aad', 125]`
          -    `varname[i][j]`, `varname_[i]_[j]` sugared, WARNING:NOT_SURE_IF_GOOD_MODEL
          -    
-    `.` `[]` dereference, `?.` `?[key]` weak dereference
     -    `{}.random()`,
     -    `[22,33,44,55].3` 0-based-index:3,
     -    `<[22,33,44,55]>.1` 0-based-index:1,
     -    `(22,33,44).2` 0-based-index:2,
     -    `${a:1,b:2}.a`,
     -    `$[23,56,643].2` 0-based-index:2 
 
     -    


```
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

VECTOR NOTATION?
```


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
-   `Boxes / containers` : these get complicated VERY fast
    -   on 64-bit systems commonly, words are `8 bytes` and cache lines are `64 bytes`
    -   a small system doesn't need 8 bytes for addressing
    -   8 bytes pointing at 1 byte is a significant ratio!
        -   For consideration : Erlang's [datatypes in memory](https://www.erlang.org/doc/system/memory.html), [illustrations](https://arunramgt.medium.com/elixir-internal-data-representation-7ad49389e9ea) : show how a word can hold both data and metadata ( "tagging" )
            -   Example : `tuple base word` -> contiguous: [ `tuple header word` -> `one word per element` ] + `any element data that does fit into a single word`    
    -   languages such as Rust store structs ( and tuples ) as contiguous blocks of memory, with their elements in descending order of size
    -   arrays are rectangular, not jagged, in many cases : C, Go, Rust, etc.
    -   `the lazy answer for writing good-enough higher-level code` : an automatically resizing array in CPP is the `std::vector` ... allocation is typically doubled at a new location, the old data is copied to the new location, bound variables are repointed to the new location, and the old location is deallocated
-   Numbers
    -    `Floats` are implemented using the `IEEE 754 double precision` standard, in various common languages including C, Julia, Go, OCaml, Java, and JavaScript : but not Erlang, or Haskell : it is probably best to go with `IEEE 754`
    -    `BigInt` is not standardised in implementation across languages; perhaps we should just consider JavaScript's implementation, since we are in this playground
-   Human Readable Characters
    -   `utf8` is the common format, which is also aligned with `ASCII` at 8-bits
    -   `bitstrings` is thoroughly implemented in Erlang, and provides ONE good example, however the alternatives need to be thought out a bit more, since these are so rare
        -  erlang goes with `data:size/typeSpecifierList`
        -  Go : `rune` is synonymous with `int32` for utf8 code points; `byte` is synonymous with `uint8` 
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
-   `JavaScript` particularly V8 : since we are using this as a simple runtime prior to any compilation
    -   normal types :
        -   `boolean` : `8 bytes`
        -   `number` : `8 bytes`     
        -   `utf8char` : `2 bytes`
        -   `Objects` including `Array`, `Map`, etc. : `32 to ~130 bytes`
        -   JS also has `TypedArray` which we can use for contiguous storage, in the future
    -   LDD should have two dataflows :
        1.  implement TPL in `JavaScript`
        2.  implement TPL in `C++`, perhaps with liberal use of `std::variant`, `std::vector`, `std:unordered_map`, `std::map`, `std::list`, `std::forward_list`, `std::set`, `std::unordered_set`, `std::unique_ptr`, `std::shared_ptr`, `std::weak_ptr` etc.
        3.  ... beyond C++, one should probably look at Assembler already rather than bothering with C?   
      
### Glyphs

|Code|Glyphs|Predefined
|-|-|-|
|`` `ALPHA` ``,`` `alpha` ``|Αα|
|`` `BETA` ``,`` `beta` ``|Ββ|
|`` `GAMMA` ``,`` `gamma` ``|Γγ|γ is Euler's constant ( Julia )|
|`` `DELTA` ``,`` `dekta` ``|Δδ|
|`` `EPSILON` ``,`` `epsilon` ``|Εε|
|`` `ZETA` ``,`` `zeta` ``|Ζζ|
|`` `ETA` ``,`` `eta` ``|Ηη|
|`` `THETA` ``,`` `theta` ``|Θθ|
|`` `IOTA` ``,`` `iota` ``|Ιι|
|`` `KAPPA` ``,`` `kappa` ``|Κκ|
|`` `LAMBDA` ``,`` `lambda` ``|Λλ|
|`` `MU` ``,`` `mu` ``|Μμ|
|`` `NU` ``,`` `nu` ``|Νν|
|`` `XI` ``,`` `xi` ``|Ξξ|
|`` `OMICRON` ``,`` `omicron` ``|Οο|
|`` `PI` ``,`` `pi` ``|Ππ|π is Archimedes' constant ( Julia )|
|`` `RHO` ``,`` `rho` ``|Ρρ|
|`` `SIGMA` ``,`` `sigma` ``|Σσς|
|`` `TAU` ``,`` `tau` ``|Ττ|
|`` `UPSILON` ``,`` `upsilon` ``|Υυ|
|`` `PHI` ``,`` `phi` ``|Φφ|φ is golden ratio ( Julia )|
|`` `CHI` ``,`` `chi` ``|Χχ|
|`` `PSI` ``,`` `psi` ``|Ψψ|
|`` `OMEGA` ``,`` `omega` ``|Ωω|
||
|`` `ALEPH` ``|א| ALEPH is the base for transfinite numbers|
|`` `e` ``|e|Euler's number, Napier's constant ( Julia )|
|`` `G` ``|G|Catalan's constant ( Julia )|
|`` `infinity` ``|∞|
|`` `NaN` ``|IEEE 754 ( 1985 )|
||
|Consider : ||runes, Arabic letters, other Hebrew letters, Old Italic Scripts ( various adapted from antiquity , International Phonetic Alphabet |
|`` ` ` ``,`` ` ` ``|Ææ|"air" in Englishy : from Dannish / Norwegian alphabet |
|`` ` ` ``,`` ` ` ``|Øø|"oohr" in Englishy : Dannish / Norwegian alphabet|
|`` ` ` ``,`` ` ` ``|
|`` ` ` ``,`` ` ` ``|
|`` ` ` ``,`` ` ` ``|

### Lexical Structures

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
|`head:tail`, `head::tail`, `head|tail`| these are example of `cons` operators in Haskell, OCaml, and Erlang, respectively ; it seems feasible to apply this syntax to a `stack` linear data structure, also|
|`head:body:tail`, `head::body::tail`, `head|body|tail`| fancifully, head-tail syntax seems extensible to head-body-tail, for `doubly-linked-lists` and `unprioritised queues`|

### Character Sets

###### Exclamation Mark

|Uses of the Exclamation Character '!'|
|-|
|`exception handling` : to be defined|


### Top-level Syntax
```
build: env:
    from: fs.something
    from: lib.something
    from: src.something

run: block: with env: with type: signature
    ensure: condition
    blocky block block code
```

### Block Syntax 
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

|`1`|`1`|`M`|`M`|`M`|`1`|`1`|
|-|-|-|-|-|-|-|
|`[`|`input`|`fanout`|`map`|`fanin`|`output`|`]`|
|-|-|-|-|-|-|-|
|-|-|`<\|`|`\|=\|`|`\|>`|-|-|
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
`SIGMA`__0^^5>>i^i+i
`SIGMA`__'0'^^'5'>>'i^i+i'
`SIGMA`__(i,0)^^(i,5)>>'i^i+i'
`SIGMA`__'i=0'^^'i=5'>>'i^i+i'
`SIGMA`__(i,=,0)^^(i,=,5)>>((i,^,i),+,i)
`SIGMA`__(i,'=',0)^^(i,=,5)>>(('i',^,'i'),+,'i')

%% Markdown :
||5
|`SIGMA`i^i+1
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

## Types

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

## Types ( continued )


#### Datatype Design considerations

-   System should maintain the easiest & safest path, at the cost of performance, for the dumbest user.
-   System should allow opt-ins to longer & riskier paths, with performance benefits, for a smarter user.

###### Ways to Layout an Array in Memory

>    2025-06-04 : This is ... not as insightful as I'd hoped it'd be. There are very many permutations of how to create an array. The array's metadata can be stored in the array's own address space (data plane) or in the address space occupied by working memory of the compiled instructions (control plane). 
I am not even sure that I captured what I wanted! But it was a good exercise. Somewhat stressy tho 

- `start` : address where the array starts
- `end` : address where the array ends; alternatively, `length` : length of the array in some unit ( bytes, or multiples of bytes )
- `control` : metadata is stored in runtime's working memory (? stackframe) not the memory the runtime is allocating to
- `data` : metadata is stored in the memory the runtime is allocating to
- `N points to P` : 1-dimensional or >1-dimensional depth
- `Total OH` : total overhead metadata stored in either `control` plane or `data` plane

`64-bit system` :
|Max Data Stored `B`|NStart @control `B`|NEnd `B` @control|NEnd `B` @dataN|NLength `B` @control|NLength `B` @data |N PointsTo P|TotalOH `B` @control|TotalOH `B` @data|Note|
|-|-|-|-|-|-|-|-|-|-|
|`2^8`              |`x<8` (weird)|                       |               |`1` (min)           |                  |No          |`(x<8)+1` (min)     |             |
|`2^8`              |`x<8` (weird)|                       |               |                    |`1` (min)         |No          |`x<8`     (min)     |`1` (min)    |
|`2^8`              |`8` (norm)   |                       |               |`1` (min)           |                  |No          |`8+1`     (min)     |             | small VLA|
|`2^8`              |`8` (norm)   |                       |               |                    |`1` (min)         |No          |`8`                 |`1` (min)    | small VLA|
|-|skip weird cases|-|-|-|-|-|-|-|-|
|`(2^64)-y`         |`8`          |`8` (min)              |               |                    |                  |No          |`8+8`     (min)     |             | big VLA |
|`(2^64)-y`         |`8`          |                       |`8` (min)      |                    |                  |No          |`8`                 |`8` (min)    | big VLA |

`64-bit system`, expanding just on the VLA examples :
|Max Data Stored `B`|NSt @con `B`|NEn `B` @con|NEn `B` @dat|NLen `B` @con|NLen `B` @dat|N Pts To P|PEn `B` @con|PEn `B` @dat|PLen `B` @con|PLen `B` @dat |Tot `B` @con|Tot `B` @dat|
|-|-|-|-|-|-|-|-|-|-|-|-|-|
|`2^(8+8)`          |`8` (norm)   |                       |               |`1` (min)           |                  |Yes         |                 |               |`1` (min)           |                  |`8+1+(1*1)` (min)   |                 |
|`2^(8+8)`          |`8` (norm)   |                       |               |                    |`1` (min)         |Yes         |                 |               |                    |`1` (min)         |`8`                 |`1+(1*1)` (min)  |       
|`(2^64)-y`         |`8`          |`8` (min)              |               |                    |                  |Yes         |`8` (min)        |               |                    |                  |`8+8+(8*1)`(min)    |                 |
|`(2^64)-y`         |`8`          |                       |`8` (min)      |                    |                  |Yes         |                 |`8` (min)      |                    |                  |`8`                 |`8+(8*1)` (min)  |                


-   underlying hardware architecture
    -    `pointers on 64-bit systems are 64-bits long, and respectively for 32-bit systems` : a moderate, and practical, assumption; the evolution of growth in system-word-sizes has been mainly motivated by how much memory can be addressed with a single word. On 64-bit systems one can compile with 32-bit pointers, limiting a process' RAM and increasing the number of pointers you can stuff into cache, but that is an optimisation, not the baseline.
    -    `cache lines on 64-bit systems are 64-bits wide` : another moderate, and practical, assumption



|Generics|
|-|
|`compile time` vs `runtime` : as yet undefined|

|Preprocessor Transformations|
|-|
|`allow enumerated passes`|

# Program Sources Consist of Formal Expressions

||Formal `program sources` consist of formal `expressions`.|
|-|-|                
|`1`|Formal `comments` are `non-normative` expressions.|
|`2`|   Formal `propositions` are `non-normative` expressions. <br> -   `a priori` propositions are true/false (have epistemic determination) `based on assumed rules`. <br> -   `a posteriori` propositions are true/false `based on values in a memory buffer ( space ), at a specific ( time )`. |
|`3`|   Formal `operations` are `normative` expressions <br> -   for all practical purposes, computational operations occur upon `data in a memory buffer` so those are `the only operands` <br> -   e.g. `var a = 1` means `label 'a' refers to address 'A' in memory, address 'A' has value '1'`.|
||If anything is lexed / parsed in a `program source` and `not assigned to one of the three` types above, then `an error results`. |
||What are generally referred to as `declarations` or `statements` in the vernacular of computer programming, are assumed under the breadth of `3`, whereas `2` covers the intuitive category of propositions which may be evaluated for truth or falsity by the sorts of programs casually referred to as automated theorem provers, proof assistants, or artifical intelligence agents.|
 

# lexical disambiguation of function application

-  Could it be simply implicit that all white space implies a tuple?
   -   `()>` means, `the contents of this tuple are lexically bound in a closure, and the block I point to shall be executed immediately in that closure`
   -   a variety of tuple is `(a,b)>c` the `function operative tuple` / [lexical disambiguation](#lexical-disambiguation-of-function-application)
         -   Polish notation : `afunction<( argument1, argument2 )`
              -   `afunction <( argument1 argument2 )` *sugared*
              -   `afunction <( argument1 argument2` *more sugared*
         -   Semi-reverse Polish notation : `( argument1, argument2 )> afunction`
         -   Reverse Polish notation :
              -   `( argument2 argument1 )> afunction`   *sugared*
         -   `argument2 argument1 )> afunction`   *more sugared*
   -   |general lexing pattern|`traditional` notation, results in a passive tuple|
       |-|-|
       |`a <- 1`    |`fn1(a)`   |   
       |`(a, <-, 1)`|`(fn1,(a))`|

   -   |`      fn1 <(   a    `| This is Haskell-ian syntax.  |-|`      fn1 <( a )    `|This is modified traditional f(a)|
       |-|-|-|-|-|
       |`(   ( fn1 <( ) a   )`| Hence, it would be confusing.|-|`( fn1_applied_to_a )`||
       |`(   ( <( ) fn1 a   )`|                              |-|                      ||
       |`( fn1_applied_to_a )`|                              |-|                      ||

   -   We can remove the SASL/ML-lian rule of `function application by juxtaposition`
     
   -   Of particular concern are the asymmetrical rules, `<(a b) = <(a,b)` and `(b a)> = (a, b)>`

   -   |`    fn1 <( a      `|`   fn1 <( a b    )`|`    b  a )> fn1  )`|
       |-|-|-|
       |`    fn1 <( a )    `|`   fn1 <( a, b ) )`|`  ( b  a )> fn1  )`|
       |`(   fn1 <( a )   )`|`(  fn2 <(    b ) )`|`  ( a, b )> fn1  )`|
       |`(fn1_applied_to_a)`|`(fn2_applied_to_b)`|`( (    b )> fn2  )`|
       |                    |                    |`(fn2_applied_to_b)`|

   -   |Then, we have to address lexing ambiguity.|-|-|
       |-|-|-|
       |||Lexing :|
       |`  notarg  fn1 <(  a b     notarg  `|`  notarg     b a  )> fn1  notarg  `|`ambiguous`  |
       |`  notarg  fn1 <(  a b   , notarg  `|`  notarg,    b a  )> fn1  notarg  `|`ambiguous`  |
       |`  notarg  fn1 <(  a b  )  notarg  `|`  notarg  (  b a  )> fn1  notarg  `|`unambiguous`|
       |||Parsing :|
       |`  notarg, fn1 <(  a b  ), notarg  `|`  notarg, (  b a  )> fn1, notarg  `|`step 1`|
       |`( notarg, fn1 <( (a,b) ), notarg )`|`( notarg, ( (a,b) )> fn1, notarg )`|`step 2`|
       |`( notarg, fn1 <(a) <(b),  notarg )`|`( notarg,  (b)> (a)> fn1, notarg )`|`step 3`|
       |||Execution :|
       |`( notarg,      fn2 <(b),  notarg )`|`( notarg,       (b)> fn2, notarg )`|`step 1`|
       |`( notarg,      all_done,  notarg )`|`( notarg,       all_done, notarg )`|`step 2`|

   -   Without adjacent tokens, lexer can make more inferences.
       
   -   |||Lexing :|
       |-|-|-|
       |`fn1 <( a b`|`b a )> fn1`|`unambiguous`  |
       |as above    |as above    |               |

## infix operators, as applied functions

LEXING PRECEDENCE RULES ?!

[Follow Haskell's style](https://wiki.haskell.org/index.php?title=Infix_operator).
-   `a infix_fn b = (infix_fn)<(a, b)` *infix becomes ordinary*
-   `ordinary_fn a b c = a \`ordinary_fn\` b c` *ordinary becomes infix*
-   `operand1 infixfunction operand2` *sugared*
-   Parsing
    -   Step 1 : `( operand1, infixfunction, operand2 ) `
    -   Step 2 : `( (infixfunction) <(operand1) <(operand2) ) `
-   Execution
    -   Step 1 : `( fn_partially_applied_to_o1 <(operand2) )`
    -   Step 2 : `( all_done )`

#### recap : infix operators are just ordinary dyadic functions

The difference between `infix dyadic` and `ordinary dyadic` functions, is a difference in lexing rules. After lexing, they are parsed the same.

-   PN and RPN
    -   PN : `(infixfunction)<( operand1, operand2 )`  
         -   `(infixfunction) <( operand1 operand2 )`  *sugared*
         -   `(infixfunction) <( operand1 operand2`  *more sugared*
    -   semi-RPN : `(operand1, operand2)>(infixfunction)`  
    -   RPN :
         -   `( operand2 operand1 )> (infixfunction)`  *sugared*
         -   `operand2 operand1 )> (infixfunction)`  *more sugared*

## partially applied functions

[Follow Haskell's style](https://wiki.haskell.org/index.php?title=Infix_operator).

-    Following Haskell's styles, ALL `N-adic functions where N>1` may be partially applied, but `unless you (write a wrapper / use other flippy floppy operators) to rearrange the order of operands`, you may `only partially apply the function by the order of its operands`.
-    `(1 +) = (+ 1)` *Haskell compiler will do extra work to make this possible, however*
     -    not sure if we should adopt this lock-stock
     -    on the other hand, we could go full retard and implement wildcards : `f = \a b c d -> a+b+c+d; g = f _ 1 _ 1; g 2 2` *returns 6* : seems like a bug generator, though. Not nice.


# Function Definition & Type Signatures

```
( name : TypeA TypeB TypeC TypeD )                                         <% sugared, sig only %>
( name :: body )                                                           <% sugared, fun only %>

( name : TypeA TypeB TypeC TypeD : body )                                  <% sugared, signed fun %>

fn name                                                                    <% sugared, signed fun %>
: TypeA TypeB TypeC TypeD
: arg1 arg2 arg3
: body
end    

( name : TypeA TypeB TypeC TypeD : body ) <( 1 2 3     <% IIFE %>
  
                                                       <% long fn, fully typed, oneline, returns TypeD final term %>

(::: body )                                             <% sugared pretty, short fn, returns final term %>

fn :                                                   <% sugared %>
: arguments
: pattern and condition -> branch_if_matched
: default_branch
end

fn :                                                  <% intermediate desugaring %>
: arguments
: case (arguments)
  : pattern and condition -> branch_if_matched
  : default_branch
  end
end

(:::)                                                   <% sugared pretty, shortest fn, returns undefined %>
fn ::: end
fn ::: undefined end                                    <% intermediate desugaring %>

fn name
: TypeA TypeB TypeC TypeD
: arg1 arg2 arg3
:   case (arg1 arg2 arg3)
    : pattern and condition -> branch_if_matched
    : pattern and condition -> branch_if_matched
    : default_branch
    end
end


```

# Explicit Environments & Bindings

```
fn definition       <% => ( "fn", "definition" ) %>
F = fn definition
F<()                <% execute F with no bound vars? %>
F<(a,b,c)           <% execute F, binding lexically local vars a,b,c to a,b,c in the execution env %>
F<((((a,b,c))))     <% will auto-unbox to an infinite degree %>
F<(<ENV>)           <% passes the entire lexical env to the execution env %>
<ENV> )> F          <% an idiom %>


"theorem" : Only 6 distinct species are needed to derive
all common scoping concerns in contemporary computer programming

i.e. applying a function /1,2 to arguments /3,
     is the same as running code with "where" or "let";

     to that, we add only namespaces /4, classes /5, and objects /6;

     is it further reduceable? not sure!

(i)
UNPARAMETERISED_BLOCK /1
     <=> ADDRESS_OF_INSTRUCTIONS_IN_MEMORY
(i)
( PARAMETERS /1 ){ UNPARAMETERISED_BLOCK }
     <=> PARAMETERISED_BLOCK
     <=> SUB/PROCEDURE
     <=> SUB/ROUTINE
     <=> FUNCTION
      
(ii)
( PARAMETERS <- ARGUMENTS /3 )
     <=> ENVIRONMENT_MAP
     <=> DEFINITIONS 

(iii)
FUNCTION <( ARGUMENTS
     <=> let DEFINITIONS in UNPARAMETERISED_BLOCK          %% BASIC 1964
     <=> UNPARAMETERISED_BLOCK where DEFINITIONS           %% ISWIM 1966

(iv)
NAMESPACE /4 .OBJECT /5
     <- cloneAndInit PARAMETERISED_BLOCK_TEMPLATE /6       %% Simula 1962

NAMESPACE.OBJECT.METHOD /1,2
     <- PARAMETERISED_BLOCK


E0     Define Class
E0N    > Define Sub Classes
E1     > Instantiate Object, with ENV     : let E(1) in construction
E2     > Apply Method, with ENV           : let E(2) in application




```   

# Selection

```
if expression
: branch_if_true
: branch_if false
end

case expression                              <% a.k.a complexif %>
: pattern and condition -> branch_if_matched
: pattern and condition -> branch_if_matched
: default_branch
end

```

# Iterables
```
while expression
: performance_with_optional_state_modification 
end

fn whiledoloop
: (Int->Bool) Fun Int (Int->Int)
: condition performance init_state mod_state
: if not condition<(init_state)
  : :done
  : performance,
      whiledoloop <( condition performance mod_state<(init_state) mod_state )
  end     
end

```
-   ITERABLE DATA STRUCTURE MODES : CONSIDER : `unified utility library interface`, including generators `eager/lazy`
   -   Loops :
       -   0 Do all looping control structures get abstracted to something like this?
           -   `while () do {}`
               -   `for a, change a, until b do {}`
           -   `do {} while ()`
               -   `for a, change a, do {} until b `
           -   `for key in/of b do {}`
               -   `for value in/of b do {}`

# Lexing Algorithm

1.  Let there be variables :
    -   `wm` : a working memory : initialised to an empty string
    -   `tt` : a token tree : initialised to an empty graph, with one `rn` : root node
2.  To maintain an account of the `tt`'s state, let there be
    -   `soe` : a stack of expectations, upon which `eoe` : expectations of ending are pushed  
    -   `cpt` : a pointer to the current parent token : initialised to point to the `rn` 
4.  Let there be a data source :
    -   `sc` : source code
      
5.  `Characters` read from `sc` shall be written to `wm` one by one

<% the first pass is to 
     (i) recognise tokens 
     (ii) reject unrecognised tokens 
     (iii) reject mismatched opening and closing tokens 
               ... however, it is not yet clear if this is possible without 
                    deeper semantic parsing
     %>

6.  Evaluate `wm` after each `character` is read, to branch upon
    -   identification of
        -   a `token beginning`
        -   a `token ending`
        -   any `error`, which may be
            -   an `unrecognised character`
            -   an `ecrt`(A) which has no corresponding `eoe` at the TOP of the `soe`
            -   an exhaustion of `sc`'s characters while there remain any `eoe` upon the `soe`,
                 with the exception of an `eoe` created implicitly when the first non-IFS character
                 of `sc` was `(`
    -   where the types of recognised `character` are
        -   `bcrt` : beginning `character` of a recognised token
             -   if this character is known as its own `ecrt`,
                 -   then GOTO the `ecrt` procedure below
                 -   otherwise, read the next character from `sc`
        -   `icrt` : intermediate character of a recognised token
             -   this must be checked against `wm` and a whole bunch of rules
        -   `ecrt` : ending character of a recognised token(A)
             -   first, append to `cpt`'s children a new node,
                      corresponding to the newly recognised token 
             -   second, only if the `rt` : recognised token : is a `cb` : context beginning
                 -   then
                     -   move `cpt` to point to the new node
                     -   and, push a corresponding `eoe` upon the `soe`
             -   clear `wm`, and read the next `character` from `sc`
                 
7.  If the first `non-IFS` character read is `(`
    -   then
        -   create an `eoc` upon the `soe`
        -   clear `wm`
    -   otherwise, proceed as usual
      
8.  
