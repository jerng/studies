-module(demo_gen_server).
-export([start_link/0, add/1, subtract/1, init/1, handle_call/3]).
-compile(native).

start_link()->
  gen_server:start_link({local,demo_gen_server_instance1},demo_gen_server,[],[]).

init(_Args) -> {ok, 0}.

add(Num) ->       gen_server:call( demo_gen_server_instance1, {add, Num}).
subtract(Num) ->  gen_server:call( demo_gen_server_instance1, {subtract, Num}).

handle_call({add, Num}, _From, State) ->      {reply, State + Num, State + Num};
handle_call({subtract, Num}, _From, State) -> {reply, State - Num, State - Num}.
