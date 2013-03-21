-module(demo_application).
-behaviour(application).
-export([start/2, stop/1]).
-compile(native).

start(_Type,_Args) ->
  demo_supervisor:start_link().

stop(State) ->
  {ok,State}.
