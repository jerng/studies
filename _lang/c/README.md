c-studies
=========

C family language studies

# 10 Rules from JPL

https://en.wikipedia.org/wiki/The_Power_of_10:_Rules_for_Developing_Safety-Critical_Code

>   1. Avoid complex flow constructs, such as goto and recursion.
>   2. All loops must have fixed bounds. This prevents runaway code.
>   3. Avoid heap memory allocation after initialization.
>   4. Restrict functions to a single printed page.
>   5. Use a minimum of two runtime assertions per function.
>   6. Restrict the scope of data to the smallest possible.
>   7. Check the return value of all non-void functions, or ~cast to void (obsolete)~ [apply a trivial wrapping function](https://stackoverflow.com/questions/11888594/ignoring-return-values-in-c/30099727#30099727) to
>      indicate the return value is useless.
>   8. Use the preprocessor only for header files and simple macros.
>   9. Limit pointer use to a single dereference, and do not use function
>       pointers.
>   10. Compile with all possible warnings active; all warnings should then be
>       addressed before release of the software.
