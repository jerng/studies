% shortcuts
%
r1(A)-> r_parse_eval(A).

%R stuff
%
% cat(serialize(THEDATA,connection=NULL),sep="")
% # this helps to print R object in something like binary
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%temp()->
%  {Dates,AdjCloses} = model_daily_prices_for_r(<<"AAPL">>),
%
%  RCodeLines = [
%    r_code_assign_vector("dates",Dates), 
%    r_code_assign_vector("prices",AdjCloses), 
%    "x = data.frame(dates, prices)",
%    "summary(x)"
%    %"plot(x)"
%  ],
%
%  io:format("\nGot data; calling r_parse_eval/1 at ~f",[timer:now_diff(now(),T1)/1000000]),
%  Returned = r_parse_eval(list_to_binary(string:join(RCodeLines,"\n"))),
%  Returned.







%% returns { [dates as strings], [adj_close as strings] }
%%
%model_daily_prices_for_r(Code) when is_binary(Code) ->
%  SortedRecords = lists:keysort(3,mnesia:dirty_read(yi2,Code)),
%  AdjCloses = [ io_lib:format("~f",[element(9,X)]) || X <- SortedRecords ],
%  Dates = [ io_lib:format("as.Date(\"~w/~w/~w\")", [
%      element(1,element(3,X)),
%      element(2,element(3,X)),
%      element(3,element(3,X))
%    ]) 
%    || X <- SortedRecords 
%  ],
%  {Dates,AdjCloses}.







% Runs S code, using Rscript.
%
% Returns a binary, of the last item that Rscript sent to stdout.
%
r_parse_eval(RCode) when is_binary(RCode)->

  TempFilename = "job.r",
  Command = "Rscript --vanilla --no-init-file " ++ TempFilename,

  RCode_escaped = re:replace(RCode, <<"\"">>, <<"\\\"">>, [global]),
  RCode_de_tailed = re:replace(RCode_escaped, <<"; {0,}\$">>, <<"">>, [global]),

  file:write_file(TempFilename,list_to_binary([RCode_de_tailed,"; cat(\"RscriptEnded\")"])),
  Result = list_to_binary( re:replace(os:cmd(Command), "RscriptEnded\$", "") ),
      % The "RscriptEnded" delimiter is required, otherwise 
      % if you just do "A=3", then Rscript returns nothing.
  file:delete(TempFilename),
  
  Result
  . 

r_code_assign_vector(VariableName, Data) when is_list(VariableName), is_list(Data)->
  [VariableName, " = c(", string:join(Data,","), ")"].


