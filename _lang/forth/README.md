https://www.forth.com/starting-forth/1-forth-stacks-dictionary/

Memory management :
https://stackoverflow.com/a/40050230/1378390

Illustrated introduction to Forth 
https://ratfactor.com/forth/forth_talk_2023.html



- tokens are commonly referred to as WORDS ;
  - the lexing rules are very simple, and whitespace matters, e.g. `1+1` is interpreted as one token, not three ;
- the meaning of words is stored in a DICTIONARY ;
  -     (wikipedia / Brodie, 1987 : ) When the interpreter finds a word, it looks the word up in the dictionary. If the word is found, the interpreter executes the code associated with the word, and then returns to parse the rest of the input stream. If the word isn't found, the word is assumed to be a number and an attempt is made to convert it into a number and push it on the stack; if successful, the interpreter continues parsing the input stream. Otherwise, if both the lookup and the number conversion fail, the interpreter prints the word followed by an error message indicating that the word is not recognised, flushes the input stream, and waits for new user input.
- full-line comments are preceded by `\`
  - inline comments are between `(` ... `)`
- programming involves explicit control of a stack
  - . means, POP the top of the stack, and print it
- syntax for adding to the dictionary
  -
- syntax for binary operators follows Reverse Polish Notation / RPN, such that,
  ` operand1 operand2 binaryOperator`, e.g. `1 1 +` means, compute "1+1", then
  PUSH the result onto the stack ( 2 and 3 are first PUSHED, then POPPED, then
  added ), and with left-associativity e.g. `1 1 + 3 *` means "(1+1)*3", then
  PUSH
