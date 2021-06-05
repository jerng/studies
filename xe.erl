%%%-----------------------------------------------------------------------------
%%% Experiments in compiling dialects of Erlang, to classical Erlang (or lower)
%%%-----------------------------------------------------------------------------
-module(xe).
-compile([export_all,native]).

%%------------------------------------------------------------------------------
%% TODO:  can we hash the module name? 
%%        can we use attributes to load/unload modules predictably, unlimitedly?
%%------------------------------------------------------------------------------
onthefly_modules()->
  Dir = "./",
  Filename = "1.xe",
  {ok,Bin} = file:read_file(Dir++Filename),
  
  BinForm1 = <<"-module(test).">>,
  BinForm2 = <<"-compile(export_all).">>,
  BinForm3 = Bin,

  BinFormsToAFForms = fun
    (E) ->
      {ok,Tokens,_EndLocation} = erl_scan:string(binary_to_list(E)),
      {ok,ExprList} = erl_parse:parse_form(Tokens),
      ExprList
  end,

  BinForms = [BinForm1,BinForm2,BinForm3],
  AFForms = lists:map(BinFormsToAFForms,BinForms),
  {ok,Module,CompiledForms} = compile:forms(AFForms),

  {module,Module} = code:load_binary(Module,Filename,CompiledForms),
  timer:tc(fun test:test/0).

%%------------------------------------------------------------------------------
%% Done! Unfortunately, as of R15B 'erl_eval' is incomplete, and does not handle
%% 'receive' operations properly. Resort to onthefly_modules() for completeness.
%%------------------------------------------------------------------------------
onthefly_expressions()->
  Dir = "./",
  Filename = "1.xe",
  {ok,Bin} = file:read_file(Dir++Filename),
  {ok,Tokens,_EndLocation} = erl_scan:string(binary_to_list(Bin)),
  
  F = fun 
    (FromErlScanString) ->
      Y = fun
        (_Yfun,[],[Final,_Temp]) ->           
          lists:reverse(Final);
        (Yfun,[{dot,Line}|T],[Final,Temp]) -> 
          Yfun(Yfun,T,[[lists:reverse([{dot,Line}|Temp])|Final],[]]);
        (Yfun,[H|T],[Final,Temp]) -> 
          Yfun(Yfun,T,[Final,[H|Temp]])
      end,
      Y(Y,FromErlScanString,[[],[]])
  end,
  ExprsAsTokens = F(Tokens),
  
  F1 = fun (E) -> {ok,ExprAsAF} = erl_parse:parse_exprs(E), ExprAsAF end,
  ExprsAsAF = lists:map(F1,ExprsAsTokens),

  lists:map(fun (E) -> erl_eval:exprs(E,[]) end, ExprsAsAF).
