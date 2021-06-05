-module(compiling_records).
-compile(export_all).

x()->
  
  SF1 = "-record(test,{f1=a,f2=b}).",
  SF2 = "A=1,A+2,#test{}.",
  
  {ok, TF1, _} = erl_scan:string(SF1),
  {ok, TF2, _} = erl_scan:string(SF2),


  {ok, AF1} = erl_parse:parse_exprs(TF1),
  {ok, AF2} = erl_parse:parse_exprs(TF2),

  { [SF1,SF2],
    [TF1,TF2],
    [AF1,AF2],
    erl_expand_records:module(AF2,[{record,AF1}]) }.

%  
%  erl_eval:exprs( AF1, [] ).
