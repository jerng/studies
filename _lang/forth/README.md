###### Decent References
- Classic [textbook](https://www.forth.com/starting-forth/) by Brodie
- [Memory management](https://stackoverflow.com/a/40050230/1378390)
- [Illustrated introduction](https://ratfactor.com/forth/forth_talk_2023.html) to Forth 

### Briefly


- WORDS refer to tokens of the language 
  - the lexing is very simple, and whitespace matters, e.g. `1+1` is interpreted as one token, not three
- the DICTIONARY stores words and their meanings
  - COMPILING refers to creating or updating meanings to the dictionary
  - INTERPRETING refers to reading meanings from the dictionary, applying the replacement rules, prior to excution
    - `INTERPRET` is an internally defined word, whose function is to find a dictionary meaning
    - `EXECUTE` is an internally defined word, which is fed the dictionary meening
    - `NUMBER` is an internally defined word, 
  - the dictionary is a linked-list, searched as a LIFO, so words can be redefined; previous references to words are not changed in meaning, only subsequent references to words refer to new definitions
  - algorithm (wikipedia / Brodie, 1987 ) :
    > When the interpreter finds a word, it looks the word up in the dictionary.
    >   - If the word is found,
    >     - the interpreter executes the code associated with the word,
    >     - and then returns to parse the rest of the input stream.
    >   - If the word isn't found,
    >     - the word is assumed to be a number and an attempt is made to convert it into a number and push it on the stack;
    >     - if successful,
    >       - the interpreter continues parsing the input stream.
    >   - Otherwise, if both the lookup and the number conversion fail,
    >     - the interpreter prints the word followed by an error message indicating that the word is not recognised, flushes the input stream, and waits for new user input.
  - `\` precedes full-line comments 
  - `(` ... `)` surrounds inline comments
  - `:` means, ( enter compile mode ), the next word will be added to the
      dictionary, and the words after that will be saved in the dictionary as the meaning of the word
  - `;` means, ( exit compile mode ), the meaning has been specified 
  - `.` means, POP the top of the stack, and print it
  - `."` ... `"` prints the contained text
  - `if` ... `then` is implemented as ( see definition )
  - `(` ... `)` is implemented as ( see definition )
  - `VARIABLE VAR` means, assign an address
- syntax for binary operators follows Reverse Polish Notation / RPN, such that,
  > ` operand1 operand2 binaryOperator`, e.g. `1 1 +` means, PUSH 1, PUSH 1, POP 1, POP 1, ADD 1 to 1, PUSH 2
  >
  > and with left-associativity e.g. `1 1 + 3 *` means "(1+1)*3", etc.
- programming involves explicit control of a stack ( a LIFO )
  - the human notation for items on the stack is `( before -- after )`
    > example :
    > - `. ( n - - )` means, 1 item before `.` executes, and 0 after
    > - `+ ( n1 n2 -- sum )` means, 2 itesm before `+` executes, and ` after
    - the numbering of items in "stack notation" is the order that they were previously pushed upon the stack ( n1 was pushed before n2, etc. )
- stack manipulation functions exist
  - `DROP` is `( n -- )`
  - `DUP` is `( n -- n n )`
  - `OVER` is `( n1 n2 -- n1 n2 n1 )`
  - `SWAP` is `( n1 n2 -- n2 n1 )`
  - `ROT` is `( n1 n2 n3 -- n2 n3 n1 )`
  - etc.
