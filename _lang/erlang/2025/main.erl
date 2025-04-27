%
%   test with :
%       erlc main.erl && erl -noshell -s main main -s init stop -init_debug -boot start_clean
%

-module(main).
-behaviour(application).

-export([start/0, start/2, stop/1]).

start()->
start(a,b).

start(_Type,_Args) -> 

%io:fwrite("hello\n")

io:format("~s",[<<"\nerlang:system_info(allocated_areas) :\n">>]),
    io:format("~p",[erlang:system_info(allocated_areas)]),

    io:format("~s",[<<"\n\nerlang:memory(total) :\n">>]),
    io:format("~p",[erlang:memory()]),
{ok,self()}.

stop(_State)->
ok.
