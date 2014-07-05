Erless
======

Erlang preprocessor studies, towards making the language's frontend more
accessible to the programming community at large.

Problem Statement
-

It has been suggested in [at least one
thread](https://news.ycombinator.com/item?id=7277797) that Erlang's syntax
(which uses commas, semi-colons, and periods, as they are used in English), may
be providing unnecessary friction to new students of Erlang.

Both the following examples will compile in standard Erlang, as demonstrated in
this repository's `main.erl`.

Example 1:

    % tidy/1 is a named function.
    %
    %   The first expression is 'doNothing', an atom, followed by a comma
    %     signifying the end of the expression.
    %
    %   The second expression is an if-end clause.
    %     Its result is returned as the result of tidy/1.
    %
    %%

    tidy(Arg1)-> 
      doNothing, 
      if
        Arg1 == 1 -> oneBranch; 
        Arg1 == 0 -> zeroBranch; 
        true      -> trueBranch  
      end.

Example 2:

    % messy/1 is isomorphic with tidy/1.
    %
    %   But messy/1's definition below shows that white space is of little
    %   consequence in stock Erlang lexing.
    %
    %%

    messy(Arg1)-> doNothing, if
    Arg1==1->oneBranch
    ; Arg1
    ==0 
    -> zeroBranch; 
    true ->
    trueBranch  end.

Proposal
-

Example 3:

    % lstyled/1 is isomorphic with tidy/1
    %
    %   But lstyled/1 uses Landin's 'off-side rule' to infer expression endings.
    %   This removes commas, semi-colons, periods, and 'end' keywords.
    %
    %%
    
    lstyled(Arg1)-> 
      doNothing 
      if  Arg1 == 1 -> oneBranch
          Arg1 == 0 -> zeroBranch
          true      -> trueBranch
    
    % Also acceptable:

    lstyled(Arg1)-> 
      doNothing 
      if
        Arg1 == 1 -> oneBranch
        Arg1 == 0 -> zeroBranch
        true      -> trueBranch
    
Example 4:

    % cstyled/1 is isomorphic with tidy/1
    %
    %   But cstyled/1 uses ALGOL-style ';' and BCPL-style '{}' to define
    %   blocks. This removes commas, semi-colons, periods, and 'end' keywords.
    %
    %%
    
    cstyled(Arg1) {
      doNothing;
      if (Arg1 == 1) {
        oneBranch;
      } else if (Arg1 == 0) {
        zeroBranch;
      } else {
        trueBranch;
      }
    }

    % A questionable innovation:

    cxstyled(Arg1) {
      doNothing;
      (Arg1 == 1) ? oneBranch 
      (Arg1 == 0) ? zeroBranch : trueBranch;
    }

