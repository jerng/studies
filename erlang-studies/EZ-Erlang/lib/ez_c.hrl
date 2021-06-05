%%%-----------------------------------------------------------------------------
%%% Header for EZ controllers. Contains macros that reduce boilerplate;
%%% TODO: figure out if there's a better way to reduce it.
%%%
%%%-----------------------------------------------------------------------------
-define( BEGIN, 
  (Session,Env,Input)-> 
    Request 
    = [ {session, Session},
        {env, Env},
        {input, Input}
      ],
).

-define( END,
  % combines the user's Response with some defaults
  % TODO: pull defaults ezutils:defaults()?
  FinalResponse
  = [ { view,
        case proplists:get_value(view,Response) of
          undefined -> default.template.view;
          _ -> proplists:get_value(view,Response) end },
      { headers,
        case proplists:get_value(headers,Response) of
          undefined -> [{"Content-Type","text/html"}];
          _ -> proplists:get_value(headers,Response)
               ++ [{"Content-Type","text/html"}] end },
      { bindings,
        case proplists:get_value(bindings,Response) of
          undefined -> [{'Request',Request}];
          _ -> proplists:get_value(bindings,Response)
               ++ [{'Request',Request}] end }
    ], 
  ez:render(Session,FinalResponse)
).
