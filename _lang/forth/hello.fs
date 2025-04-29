\ Forth 
\
\       full-line comments are preceded by \ ;
\       inline comments are between ( ... ) ;
\       the lexing rules are very simple, and whitespace matters,
\               e.g. "1+1" is interpreted as one token, not three ;
\
\       programming involves explicit control of a stack ;
\
\       . means, POP the top of the stack, and print it ;
\
\       syntax for binary operators follows Reverse Polish Notation 
\               / RPN, such that,
\
\               operand1 operand2 binaryOperator
\               e.g. 1 1 + means, compute "1+1", then PUSH 
\                       the result onto the stack
\                       ( 2 and 3 are first PUSHED, then POPPED, 
\                                               then added )
\
\               , and with left-associativity
\               e.g. 1 1 + 3 * means "(1+1)*3", then PUSH

CR .\" Hello, World!\" ( comment inline )

\ CR means "print carriage return"

\ variables can be defined, and redefined

\ VARIABLE VAR  means "define the variable named VAR"
\ !             is pronounced "store"
\ 12 VAR !      means "store the value 12, in VAR"


\ Words : are names for subroutines
\
\ Words are defined using the (: ... ;) syntax ;
\ the text between the colons becomes the body of the word ;
\ words are executed by typing their name and pressing enter.
