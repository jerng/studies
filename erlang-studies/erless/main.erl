-module(main).
-export([tidy/1,messy/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tidy/1 is a named function.
%
%   The first expression is 'noNothing', an atom, followed by a comma
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
