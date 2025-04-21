-module(f).
-compile(export_all).
-include("../x.hrl"). % general utilities 
-include("../m.hrl"). % math, stats, data structuring
-include("../w.hrl"). % web service 

temp()->

  %
  % Routing, Web Services, Authentication
  %

  web_server([f]), % http://localhost:8000/f:viewAtom  
  
  T1 = now(),
  
  %
  % Models, Database Access
  %
  
  UnsortedRecords = mnesia:dirty_select(yi2,
    [ { {
          '_',    <<"AAPL">>,   '$3',
          '_',    '_',          '_',
          '_',    '_',          '$9'
        },
        [],
        [{{'$3','$9'}}]
      }
    ]
  ),

  SortedRecords = lists:keysort(1,UnsortedRecords),

  Prices = [ element(2,X) || X <- SortedRecords ],
  
  { StdDevSam_Prices, 
    Variance_Prices, 
    MeanA_Prices, 
    Count_Prices } = std_dev_sam(Prices),

  %
  % Controller, Access Control
  %

  Out = {
    { count, Count_Prices },
    { mean, MeanA_Prices },
    { variance, Variance_Prices },
    { std_dev, StdDevSam_Prices }
  },


  %
  % View, I/O
  %

  io:format(
    "\nended at ~f seconds\n",
    [timer:now_diff(now(),T1)/1000000]
  ),

  Out
.


view1(SessionID, _Env, _Input) ->
  mod_esi:deliver(SessionID, [
    "Content-Type: text/html\r\n\r\n", 
    <<"<html><title>f:view1</title><body><pre>">>,
    io_lib:print({SessionID,_Env,_Input}),
    <<"</pre></body></html>">>
  ]).





%getNasdaqData()->
%  inets:start(),
%  Bin = get_file_bin(".","nasdaq_temp.csv"),
%  List = csv_to_list(Bin,[remove_empty,equal_length]),
%  Codes = lists:foldl(fun(E,Acc)-> [lists:nth(1,E)|Acc] end,[],List),
%  %Codes = mnesia:dirty_select(klse,[{{'_','$1','_','_','_','_','_','_','_'},[],['$1']}]),
%
%  lists:foreach(
%    fun(E)->
%      io:format("Ticker Code: ~p, ",[E]),
%      Rows = yi_data(
%        E,
%        {2000,1,1},
%        {2012,7,9},
%        <<"d">>
%      ),
%      case Rows of
%        [] ->
%          do_nothing;
%        _ ->
%          io:format("Rows obtained: ~p, saving...\n",[length(Rows)]),
%          lists:foreach(fun mnesia:dirty_write/1 ,Rows)
%          %io:format(io_lib:print(Rows))
%      end
%    end,
%    Codes
%  ),
%  inets:stop()
%  .

% returns fields:
% date, open, high, low, close, volume, adj_close
%
% formatting examples for KLSE tickers: DIGI.KL, DIGI-CI.KL 
%
yi_data(Code, From, To, Interval) when is_binary(Code) ->
  {FromYear, FromMonth, FromDay} = From,
  {ToYear, ToMonth, ToDay} = To,
  Url =
    lists:flatten([
      "http://ichart.yahoo.com/table.csv?s=",
      <<Code/binary>>,
      "&a=",
      integer_to_list(FromMonth - 1),
      "&b=",
      integer_to_list(FromDay),
      "&c=",
      integer_to_list(FromYear),
      "&d=",
      integer_to_list(ToMonth - 1),
      "&e=",
      integer_to_list(ToDay),
      "&f=",
      integer_to_list(ToYear),
      "&g=",
      Interval,
      "&ignore=.csv"
    ]),
  CsvAsList = csv_at_url(Url),
  case CsvAsList of
    [] -> 
      [];
    _ ->
      lists:map(
        fun(E)->
          {
            yi,
            {
              Code,
              {
                binary_to_number(binary:part(lists:nth(1,E),0,4)),
                binary_to_number(binary:part(lists:nth(1,E),5,2)),
                binary_to_number(binary:part(lists:nth(1,E),8,2))
              }
            },
            binary_to_number(lists:nth(2,E)),
            binary_to_number(lists:nth(3,E)),
            binary_to_number(lists:nth(4,E)),
            binary_to_number(lists:nth(5,E)),
            binary_to_number(lists:nth(6,E)),
            binary_to_number(lists:nth(7,E))
          }
        end,
        lists:nthtail(1, CsvAsList)
      ) 
  end
  .

% http://ichart.yahoo.com/table.csv?s="&BN1&"&a="&(month(BP1)-1)&"&b="&day(BP1)&"&c="&year(BP1)&"&d="&(month(BR1)-1)&"&e="&day(BR1)&"&f="&year(BR1)&"&g="&BT1&"&ignore=.csv

%tickersToMnesia()->
%  Bin = getFileBin(".","2012-06_klse.csv"),
%  List = csvToList(Bin,[remove_empty,equal_length]),
%  CleanList = lists:nthtail(2,List),
%
%  Months = [
%    {<<"Jan">>,1},
%    {<<"Feb">>,2},
%    {<<"Mar">>,3},
%    {<<"Apr">>,4},
%    {<<"May">>,5},
%    {<<"Jun">>,6},
%    {<<"Jul">>,7},
%    {<<"Aug">>,8},
%    {<<"Sep">>,9},
%    {<<"Oct">>,10},
%    {<<"Nov">>,11},
%    {<<"Dec">>,12}
%  ],
%
%  ShortList = lists:map(
%    fun(E)->
%      [
%        klse,
%        lists:nth(1,E),
%        lists:nth(2,E),
%        lists:nth(3,E),
%        if_not_empty_bin(
%          lists:nth(4,E),
%          fun (B)->
%            {
%              binary_to_number(
%                binary:part(B,byte_size(B),-4)
%              ),
%              element(
%                2,
%                lists:keyfind(
%                  binary:part(B,byte_size(B)-5,-3),
%                  1,
%                  Months
%                )
%              ),
%              binary_to_number(
%                binary:part(B,byte_size(B)-9,9-byte_size(B))
%              )
%            }
%          end
%        ),
%        if_not_empty_bin(
%          lists:nth(5,E),
%          fun binary_to_number/1
%        ),
%        if_not_empty_bin(
%          lists:nth(6,E),
%          fun binary_to_number/1
%        ),
%        lists:nth(7,E),
%        lists:nth(8,E)
%      ]
%    end,
%    CleanList
%  ),
%  ShortList
%  %lists:nth(1,ShortList)
%  %lists:foreach(fun(E)->io:format(", ~p",[lists:nth(5,E)]) end,ShortList)
%  .

yMultipleQuote()->

%  string:join(
%    lists:map(
%      fun(E)->binary_to_list(E)end,
%      TickerCodes
%    ),
%    ","
%  ).
  inets:start(),
  
  Url =
    "http://finance.yahoo.com/d/quotes.csv?s=7164CK.KL,BRK-B,1651CH.KL,2488CE.KL,5210CA.KL,4715CS.KL,3417CC.KL,6947CG.KL,0500CD.KL,0528C5.KL,5202CA.KL,1961CS.KL,&f=abl1",
  
  {ok,{_,_,ResponseBody}} = httpc:request(Url),
  
  %[Bin,_] = re:replace(ResponseBody,"\r\n",""),

  %list_to_float(binary_to_list(Bin)),
  
  ResponseBody.

%aapl()->
%  AAPL = ySingleQuote("AAPL","l1").
%
%appleC5()->
%  AAPL = ySingleQuote("AAPL","l1"),
%  USDMYR = ySingleQuote("USDMYR=X","l1"),
%  Spot = ySingleQuote("0528C5.KL","b"),
%  Intrinsic = (AAPL-388) / 1500 * USDMYR,
%  Premium = 
%    lists:flatten(
%      io_lib:format(
%        "~.2f%", 
%        [(Spot / Intrinsic - 1) * 100])),
%  { {spot,Spot},
%    {intrinsic,Intrinsic},
%    {premium,Premium}
%  }.

ySingleQuote(UnderlierCode,FieldCode)->

  inets:start(),
  
  Url =
    lists:flatten([
      "http://finance.yahoo.com/d/quotes.csv?s=",
      UnderlierCode, 
      "&f=", 
      FieldCode]),
  
  {ok,{_,_,ResponseBody}} = 
    httpc:request(Url),
  
  [Bin,_] = re:replace(ResponseBody,"\r\n",""),

  list_to_float(
    binary_to_list(Bin)).
  

% "Symbol","Name","LastSale","MarketCap","ADR TSO","IPOyear","Sector","Industry","Summary Quote",

%nasdaqCsvToMnesia()->
%
%  [_|Rows] = 
%    deTailedBinSplit(
%      getFileBin(".","2012-06-22_nasdaq.csv"),
%      <<"\r\n">>),
%
%  RecordsAsLists = 
%    lists:map(
%      fun(E)->
%        [nasdaq|deTailedBinSplit(E,<<",">>)] end,
%      Rows),
%
%  RecordsAsTuples = 
%    lists:map(
%      fun list_to_tuple/1,
%      RecordsAsLists),
%  
%  lists:foreach(
%    fun(E)->
%      mnesia:dirty_write(nasdaq, E) end,
%    RecordsAsTuples),
%
%  io:format("\n\n~p\n",[length(RecordsAsTuples)]).
