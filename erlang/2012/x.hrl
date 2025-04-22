% shortcuts
%
%
o(A)-> os:cmd(A).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


csv_at_url(Url)->
  {ok,{{_,StatusCode,_},_,ResponseBody}} = httpc:request(Url),
  io:format("HTTP Status Code: ~p, ", [StatusCode]),
  if 
    StatusCode == 200 ->
      csv_to_list(ResponseBody,[remove_empty,equal_length]);
    true ->
      io:format(["skipping...\n"]),
      []
  end.

if_not_empty_bin(B,ThenApplyThisFun) 
  when is_binary(B) andalso is_function(ThenApplyThisFun)->
  if B == <<>>  -> <<>>;
     true       -> ThenApplyThisFun(B)
  end.

binary_to_number(B) when is_binary(B) ->
  L = binary_to_list(B),
  try list_to_float(L)
  catch error:R1-> 

    try list_to_integer(L)
    catch error:R2 ->
      exit({
        binary_to_number,
        {list_to_float,R1},
        {list_to_integer,R2},
        erlang:get_stacktrace()
      })
    end

  end.

%
%
%
% Returns a list-of-lists, for further processing.
%
%
%

%test()->
%
%  % approachs: character by character, then regex
%  % approachs: binary, then list
%  
%  Bin = 
%  
%  % Tests for 
%  % \r\n
%  % \n\r
%  % \n
%  % \r
%  % ,,
%  % ,"n"
%  % ,"keratin ""hair""",
%  % ,"hello, kitty",
%  %
%  <<"a,b,c\r\nd,e,f\n\rg,\"hello, kitty\",i\rj,\"keratin \"\"hair\"\"\",l\nm,\"n\",o\np,q\n\nr,s,t,\r\r,u,v,w\r\r">>,
%  
%
%  io:format("\n~p\n\n",[Bin]),
%
%  csv_to_list(Bin).

csv_to_list(List) when is_list(List)-> 
  csv_to_list(list_to_binary(List));
csv_to_list(Bin) when is_binary(Bin)-> 
  csv_to_list(Bin,[],[],<<>>,false).

% Options:
% - remove_empty: removes [<<>>] rows
% - equal_length (requires remove_empty): 
%     fails if rows have inconsistent lengths
%
csv_to_list(List,Options) when is_list(List), is_list(Options) ->
  csv_to_list(list_to_binary(List),Options);
csv_to_list(Bin,Options) when is_binary(Bin), is_list(Options) ->
  List = csv_to_list(Bin,[],[],<<>>,false),

  % refactor with lists:foldr( Options); remove case checks?

  % definition
  %
  EqualLength = fun (ListX)->
    case lists:member(equal_length,Options) of
      true ->
        lists:foldl(
          fun(E,{{row_0_length,Length}, {row_index,Index}})-> 
            if  
              (Length==unset)    -> 
                {{row_0_length,length(E)}, {row_index,Index+1}};
              (Length==length(E))-> 
                % io:format(", ~p",[length(E)]), 
                {{row_0_length,Length}, {row_index,Index+1}};
              true            -> 
                erlang:exit({inconsistent_length_row, Index,E}) 
            end 
          end,
          {{row_0_length,unset}, {row_index,0}},
          ListX
        ),
        ListX;
      false -> ListX
    end
  end,

  % deployment
  %
  case lists:member(remove_empty,Options) of
    true  -> 
      List1 = lists:filter(fun(E)->(E /= [<<>>])end, List),
      EqualLength(List1);  
    false -> 
      List
  end.

% handle end-of-binary
csv_to_list(<<>>,Tab,Row,Field,_)->
  %io:format("end of binary\n"),
  lists:reverse([[Field|Row]|Tab]);

% handle line-breaks
csv_to_list(<<H:16,T/binary>>,Tab,Row,Field,InQuotedField) 
  when (not InQuotedField),  
       (<<H:16>> == <<"\n\r">>) orelse (<<H:16>> == <<"\r\n">>)
       ->
    %io:format("2-character line break\n"),
    csv_to_list(
      T,
      [lists:reverse([Field|Row])|Tab], % end this row
      [],
      <<>>,
      InQuotedField
    );
csv_to_list(<<H:8,T/binary>>,Tab,Row,Field,InQuotedField) 
  when (not InQuotedField), 
       (  <<H>> == <<"\n">>
          orelse <<H>> == <<"\r">> ) 
       ->
    %io:format("1-character line break\n"),
    csv_to_list(
      T,
      [lists:reverse([Field|Row])|Tab], % end this row
      [],
      <<>>,
      InQuotedField
    );

% handle double-quotes
csv_to_list(<<H:8,T/binary>>,Tab,Row,Field,InQuotedField) 
  when (not InQuotedField), 
       (<<H>> == <<"\"">>)
       ->
    %io:format("double quote: enter quoted field \n"),
    csv_to_list(
      T,
      Tab,
      Row,
      Field,
      true % state change
    );
csv_to_list(<<"\"\"",T/binary>>,Tab,Row,Field,InQuotedField) 
  when InQuotedField ->
    %io:format("double double quote in quoted field\n"),
    csv_to_list(
      T,
      Tab,
      Row,
      <<Field/binary,"\"">>,
      InQuotedField
    );
csv_to_list(<<"\"",T/binary>>,Tab,Row,Field,InQuotedField) 
  when InQuotedField ->
    %io:format("double quote: exit quoted field \n"),
    csv_to_list(
      T,
      Tab,
      Row,
      Field,
      false % state change
    );

% handle commas
csv_to_list(<<",",T/binary>>,Tab,Row,Field,InQuotedField)
  when InQuotedField ->
    %io:format("comma: in quoted field\n"),
    csv_to_list(
      T,
      Tab,
      Row,
      <<Field/binary,",">>,
      InQuotedField
    );
csv_to_list(<<",",T/binary>>,Tab,Row,Field,InQuotedField) ->
    %io:format("comma: out of quoted field \n"),
    csv_to_list(
      T,
      Tab,
      [Field|Row],
      <<>>,
      InQuotedField
    );

% handle other characters
csv_to_list(<<H:8,T/binary>>,Tab,Row,Field,InQuotedField) ->
    %io:format("other character: ~p \n",[<<H>>]),
    csv_to_list(
      T,
      Tab,
      Row,
      <<Field/binary,H>>,
      InQuotedField
    ).



get_file_bin(Dirname, Filename)->
  {ok, Bin} = file:read_file( filename:join(Dirname, Filename) ),
  Bin.

%
%
%
% Loose work
%
%
%

  % study in compiling a Ms
  %
%
%  % Consider input, define field variables
%  Tab = yi2,
%  Code = <<"TM">>,
%  Date = '$1',
%  
%  Open = '_',
%  High = '_',
%  Low = '_',
%  
%  Close = '_',
%  Volume = '_',
%  Adj_close = '$2',
%
%  % Matchhead
%  Mh = {    
%    Tab,
%    Code,
%    Date,
%
%    Open,
%    High,
%    Low,
%    
%    Close,
%    Volume,
%    Adj_close
%  },
%
%  % MatchConditions
%  Mc = [],
%  
%  % MatchBody
%  Mb = [{{'$1','$2'}}],
%
%  % MatchFunction
%  Mf = {
%    Mh,
%    Mc,
%    Mb
%  },
%  
%  % MatchExpression
%  Me = [Mf],
%  
%  mnesia:dirty_select(Tab,Me)

%  attempt to delete duplicates in a duplicate bag
%
%  First = mnesia:dirty_first(yi2),
%  Unknown = mnesia:dirty_read(yi2, First),
%  Unique = sets:from_list(Unknown),
%  Extras = Unknown - Unique,
%  lists:foreach(
%    fun(Extra)->
%      mnesia:dirty_delete_object(Extra)
%    end,
%    Extras
%  )
%
% another approach may be to copy the whole table to a bag, then back to a duplicate bag


% copying a table ( change of structure ) 
%
%  Exchange = mnesia:dirty_all_keys(klse),
%  KeyCount = length(Exchange),
%
%  T1 = now(),
%  lists:foldl(
%    fun(Code,Acc)->
%      io:format("\n~f minutes remaining; getting ~p... ",[timer:now_diff(now(),T1)/Acc*(KeyCount-Acc)/1000000/60,Code]),
%      CleanCode = binary:replace(Code,<<"\"">>,<<"">>,[global]),
%      Rows = mnesia:dirty_select(yi,[{
%        {
%          yi,
%           {CleanCode,'$1'},
%          '$2',
%          '$3',
%          '$4',
%          '$5',
%          '$6',
%          '$7'
%        },
%        [],
%        [{{yi2,CleanCode,'$1','$2','$3','$4','$5','$6','$7'}}]
%      }]),
%      io:format(["setting... "]),
%      lists:foreach(
%        fun(Row)->
%          mnesia:dirty_write(Row)
%        end,
%        Rows
%      ),
%      Acc+1
%    end,
%    1,
%    Exchange
%  )


%
%
%
% Consider deprecating the following.
%
%
%

%deTailedBinSplit(Bin, Delimiter) when is_binary(Bin), is_binary(Delimiter)->
%  DeTailedBin =
%    re:replace(
%      Bin,
%      <<Delimiter/binary, "$">>,
%      <<>>,
%      [ {return, binary} ] ),
%  binSplit(DeTailedBin, Delimiter).
%
%binSplit(Bin, Delimiter) when is_binary(Bin), is_binary(Delimiter)->
%  binSplit(Bin, byte_size(Bin), Delimiter, fun binSplit/6, [], {0,0}).
%
%binSplit(_, _, _, _, Acc, nomatch)->
%  lists:reverse(Acc);
%binSplit(Bin, BinSize, Delimiter, Fn, Acc, {PrevMatchPos, PrevMatchLen})->
%  case PrevMatchPos rem 100000 of
%    X when PrevMatchPos /= 0, X =:= 0 ->
%      io:format("\n binSplit/6 is at ~fMB",[PrevMatchPos/1024/1024]);
%    _ ->
%      carry_on
%  end,    
%  SearchPos =           PrevMatchPos + PrevMatchLen,
%  SearchLen = BinSize - PrevMatchPos - PrevMatchLen,
%  MatchResult = binary:match(
%    Bin, 
%    Delimiter, 
%    [ {scope, {SearchPos, SearchLen}} ] ), 
%  case MatchResult of
%    nomatch ->
%      EndPos = BinSize - SearchPos;
%    {MatchPos, _} ->
%      EndPos = MatchPos - SearchPos
%  end,
%  Fn(
%    Bin, 
%    BinSize, 
%    Delimiter,
%    Fn, 
%    [binary:part(Bin, SearchPos, EndPos) | Acc], 
%    MatchResult )
%  .
%
