-module(my_mod).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type,_Args) -> 
io:fwrite("hello\n"),
{ok,self()}.

stop(_State)->
ok.
