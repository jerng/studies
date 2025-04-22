-module(army).
-export([a/0,b/1]).

% Studies in distributed and/or concurrent programming.

a()->
  spawn(army, b, [10000000])
  ,
  spawn(army, b, [10000000])
  .

b(N)->
  Fn = 
    fun 
      (_, 0) ->
        io:format(["\nJob done.\n"]);
      (F, Iterations) ->
        case (Iterations rem 1000000) of
          0 -> io:format(["\n", integer_to_list(Iterations), " iterations remaining in ",pid_to_list(self()),"\n"]);
          _ -> doNothing 
        end,
        F(F, Iterations-1)
    end
  ,
  Fn( Fn, N )
  ,
  ok.
