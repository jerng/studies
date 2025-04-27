%%%-----------------------------------------------------------------------------
%%% TODO: debug framework - think CakePHP? 
%%% TODO: improve default headers: proplists & build_headers()? 
%%%
%%%
%%% router/3
%%% |
%%% |-request/3
%%% | |
%%% | |-sub_path_to_params/1
%%% | |
%%% | |-query_string_to_params/1
%%% | 
%%% |
%%% |-response/1
%%% | |
%%% | |-erlang:apply/3 
%%% | | ... is used to call Controller:Action/1
%%% | | ... each Action is a function that needs to return a 4-tuple:
%%% | |      {view, Headers, ViewModule, ViewBindings } 
%%% | | ... a HTTP header string (not binary) is built
%%% | |
%%% | |-erlang:apply/3
%%% | | ... is used to call ViewModule:interpolate/1
%%% | | ... each ViewModule:interpolate/1 returns HTTP content as an Io_list
%%% | | 
%%% | |- HTTP header is combined with HTTP content, and returned
%%% |
%%% |-mod_esi:deliver/3
%%%   ... in standard fashion, is used to send response to the client
%%%
%%%
%%%
%%%
%%% Test URL for GET request processing.
%%% http://localhost:8000/ez:router/default/default/ersdf/sd/a:3/b:4/:ad/asd:?C=weg&fd=&=dfg&=&dfsdf&d=43gs;sdfg=4;23gw=ve;kmdfs#sadksdg

%%% Timer reporting template
%%%
%%%  T1 = now(),
%%%  {ms, timer:now_diff(now(),T1)}
%%%-----------------------------------------------------------------------------
-module(ez).
-export([router/3]).

%%------------------------------------------------------------------------------
%% Currently all requests should be piped through this function. We'll have to 
%% figure out how to do without, later; either by hacking Inets or by hacking 
%% around it (port mapping whatnot).
%%------------------------------------------------------------------------------
router(SessionID, _Env, _Input) ->
  mod_esi:deliver(
    SessionID, 
    response( request(SessionID,_Env,_Input)  ) ).

%%------------------------------------------------------------------------------
%%
%%------------------------------------------------------------------------------
response(Request)->
  {view, Headers, ViewModule, ViewBindings } 
    = apply(
        proplists:get_value(controller, Request),
        proplists:get_value(action, Request),
        [Request]  ),  
  [ [ lists:map(
        fun({Name,Value})-> [Name,": ",Value,"\r\n"] end, 
        Headers ),
      "\r\n"  ],
    apply(ViewModule,interpolate,[ViewBindings])  ].

%%------------------------------------------------------------------------------
%% 
%%------------------------------------------------------------------------------
request(SessionID, _Env, _Input)->
  case re:split(_Input, "\\?") of
    [SubPath]               -> QueryString = <<>>;
    [SubPath, QueryString]  -> continue end,
  SubPathParams = sub_path_to_params(SubPath),
  QueryStringParams = query_string_to_params(QueryString),
  [ {session, SessionID},
    {environment, _Env},
    {input, _Input},
    {subpath, SubPath},
    {query_string,QueryString},
    proplists:lookup(controller, SubPathParams),
    proplists:lookup(action, SubPathParams),
    proplists:lookup(anon_params, SubPathParams),
    { named_params, 
      QueryStringParams 
        ++ proplists:get_value(named_path_params, SubPathParams) }  ].  


%%------------------------------------------------------------------------------
%% TODO: figure out if re:split() is better than binary:split()
%% Action name "module_info" is prohibited.
%%------------------------------------------------------------------------------
sub_path_to_params(SubPath)->
  SubPathParams = binary:split(SubPath,<<"/">>,[global]),
  PartitionedSubPathParams =  
    lists:foldl(
      fun(E,[Anon,Named])->
        case binary:split(E,<<":">>) of
          [Z]     -> [[Z | Anon], Named];
          [<<>>,Y]-> [[Y | Anon], Named];
          [X,W]   -> [Anon, [{X,W} | Named]]  end end,
      [[],[]],
      SubPathParams ),
  NamedPathParams = lists:reverse(lists:nth(2,PartitionedSubPathParams)),
  case lists:reverse(lists:nth(1,PartitionedSubPathParams)) of
    [] ->
      Controller      = default,
      Action          = default,
      AnonParams  = [];
    [L] ->
      Controller      = case L of <<>> -> default; _ -> L end,
      Action          = default,
      AnonParams  = [];
    [L, M] ->
      Controller      = case L of <<>> -> default; _ -> L end,
      Action          = case M of <<>> -> default; _ -> M end,
      AnonParams  = [];
    [L | [M | AnonParams]] ->
      Controller      = case L of <<>> -> default; _ -> L end,
      Action          = case M of <<>> -> default; _ -> M end end,
  CheckedController = 
    case Controller of 
      default.c -> default.c; 
      _ -> binary_to_atom(list_to_binary([Controller,<<".c">>]),utf8) end,
  CheckedAction =
    case Action of 
      <<"module_info">> -> default; % SECURITY CONSIDERATION!
      default -> default; 
      _ -> binary_to_atom(Action,utf8) end,
  [ {controller, CheckedController},
    {action, CheckedAction},
    {anon_params, AnonParams},
    {named_path_params, NamedPathParams}  ].

%%------------------------------------------------------------------------------
%% TODO: figure out if re:split() is better than binary:split()
%%
%%------------------------------------------------------------------------------
query_string_to_params(QueryString)->
  QueryStringParams = binary:split(QueryString,[<<"&">>,<<";">>],[global]),
  lists:foldl(
    fun(E,Acc)->
      case binary:split(E,<<"=">>) of
        [_] ->
          Acc;
        [<<>>,_] ->
          Acc;
        [L,M] ->
          [{L,M} | Acc] end end,
    [],
    QueryStringParams ).
