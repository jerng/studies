# Tacit Programming

What is it? Basically ...

### This ...

-   [tacit programming](https://en.wikipedia.org/wiki/Tacit_programming)
    -   a.k.a. **point-free style**
    -   a.k.a. [function-level
        programming](https://en.wikipedia.org/wiki/Function-level_programming)
    -   characteristic of [concatenative programming
        languages](https://en.wikipedia.org/wiki/Concatenative_programming_language)
    -   functions are
        [composed](https://en.wikipedia.org/wiki/Function_composition)
-   Examples :
    -   [FORTH](https://en.wikipedia.org/wiki/Forth_(programming_language\))-based
    -   [APL](https://en.wikipedia.org/wiki/APL_(programming_language))
    -   partially
        -   [Haskell](https://en.wikipedia.org/wiki/Haskell) have composition operators : `.`
        -   POSIX [Shell Command
            Language](https://pubs.opengroup.org/onlinepubs/9799919799/) has pipes: `|`

###### General Example
```
input : fn1 : fn2 : fn3 : print
```

###### JavaScript Example 
... from Google AI
```
function compose(...fns) {
  return function(x) {
    return fns.reduceRight((acc, fn) => fn(acc), x);
  };
}

const add = x => x + 2;
const multiply = x => x * 3;
const subtract = x => x - 1;

const composedFunction = compose(subtract, multiply, add);

const result = composedFunction(5); // (5 + 2) * 3 - 1 = 20
console.log(result); // Output: 20
```

### ... not this ...

-   [von Neumann programming
    languages](https://en.wikipedia.org/wiki/Von_Neumann_programming_languages)
    -   a.k.a [value-level
        programming](https://en.wikipedia.org/wiki/Value-level_programming)
    -   functions are
        [applied](https://en.wikipedia.org/wiki/Function_application)
-   Examples : most other programming languages
    -   [ALGOLl](https://en.wikipedia.org/wiki/ALGOL)-based
    -   [LISP](https://en.wikipedia.org/wiki/Lisp_(programming_language\))-based

###### General Example
```
print( fn3( fn2( fn1( input ) ) ) )
```
### Prior work

I guess the this is already kinda in points-free style : 
      [https://github.com/jerng-org/ruthenium]
 
