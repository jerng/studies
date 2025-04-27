-module(demo_supervisor).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(native).

start_link() ->
  supervisor:start_link(
    { local,
      demo_supervisor_instance1
    },
    demo_supervisor,
    []
  ).

init(_Args) ->
  { ok, 
    { { one_for_one, 
        1, 
        60
      },
      [ { demo_gen_server_child_id, 
          { demo_gen_server, 
            start_link,
            []  
          },
          permanent, 
          brutal_kill, 
          worker, 
          [ demo_gen_server ]
        }
      ]
    }
  }.
