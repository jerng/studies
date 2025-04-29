###### Decent References
- Classic [textbook](https://www.forth.com/starting-forth/) by Brodie
- [Memory management](https://stackoverflow.com/a/40050230/1378390)
- [Illustrated introduction](https://ratfactor.com/forth/forth_talk_2023.html) to Forth 

### Briefly


- WORDS refer to tokens of the language 
  - the lexing rules are very simple, and whitespace matters, e.g. `1+1` is interpreted as one token, not three
- the DICTIONARY stores words and their meanings
  - COMPILING refers to creating or updating meanings to the dictionary
  - INTERPRETING refers to reading meanings from the dictionary
  - the dictionary is searched as a LIFO, so words can be redefined; previous references to words are not changed in meaning, only subsequent references to words refer to new definitions
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
  - `if` ... `then` is implemented as ( see definition )
  - `(` ... `)` is implemented as ( see definition )
  - `VARIABLE VAR` means, assign an address
- syntax for binary operators follows Reverse Polish Notation / RPN, such that,
  > ` operand1 operand2 binaryOperator`, e.g. `1 1 +` means, PUSH 1, PUSH 1, POP 1, POP 1, ADD 1 to 1, PUSH 2
  >
  > and with left-associativity e.g. `1 1 + 3 *` means "(1+1)*3", etc.
- programming involves explicit control of a stack
