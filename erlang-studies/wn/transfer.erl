-module(transfer).
-export([main/0]).

main()->

  DataDir = "raw",
  {ok,Cwd} = file:get_cwd(),
  Tab = 'cntlist.rev',
  Pattern = '$1',


%   {ok,EtsFileNames} = file:list_dir(filename:join(Cwd,DataDir)),
%   LoadEtsTab = 
%     fun(EtsFileName)-> 
%       {ok,EtsTab} = ets:file2tab(filename:join([Cwd,DataDir,EtsFileName])),
%       io:format("'~s'\n",[EtsTab])
%       %io:format("\t ~p...\n",[ets:lookup(EtsTab,ets:first(EtsTab))])
%     end,
% 
%   lists:foreach(LoadEtsTab,EtsFileNames).



  case ets:info(Tab) of
    undefined -> 
      ets:file2tab(filename:join([Cwd,DataDir,atom_to_list(Tab)++".ets"]));
    _ -> 
      ignore
  end,
  Matched = ets:match(Tab,Pattern),



%  Fn = 
%    fun([{K,V}])-> 
%      if  length(V) == 8 -> ok; 
%          true -> io:format("inconsistency detected\n")
%      end
%    end,
%  Out = lists:foreach(Fn, Matched),

%  Fn = 
%    fun([{K,V}])-> 
%      length(proplists:get_value(gloss,V))
%    end,
%  Out = lists:map(Fn, Matched),
 
  PropListToBSON = 
    fun(PL)->
      list_to_tuple(lists:reverse(lists:foldl(
        fun
          ({K1,V1},Acc) when is_atom(K1) -> 
            [V1| [K1|Acc] ];
          ({K1,V1},Acc) when is_binary(K1) -> 
            [V1| [list_to_atom(binary_to_list(K1))|Acc] ] 
        end,
        [],
        PL
      )))
    end,

  Fn = 
    fun([{K,Va0}])-> 

      Va1 = 
        Va0,

      Va2 = 
        Va1,

%        lists:keyreplace( pointers, 
%                          1,
%                          Va1,
%                          { pointers,
%                            lists:map(
%                              fun tuple_to_list/1, 
%                              proplists:get_value(pointers,Va1)
%                            )
%                          }
%                        ),
                        
      Va3 = 
        Va2,

%        lists:keydelete(frames,1,Va2),

%      Va4 =[{exception,K}|Va3],
%      PropListToBSON(Va4)
%      {list_to_atom(binary_to_list(K)),element(2,lists:nth(1,Va0))}
      PropListToBSON([{sense_key,K}|Va0])
%      length(Va0)
    end,


%  Out = lists:map(Fn, [lists:nth(1,Matched)]),
  Out = lists:map(Fn, Matched),
%  Out = lists:nth(1,Matched),
%  Out = Matched,

%  io:format("~p\n",[Out]),
  Out.





    % data.verb 
    % { binary_key, list_of_8 }
    % 
    % list_of_8:  words: list
    %             pointers: list
    %             frames: list




% 'data.verb'
% 'data.noun'
% 'data.adj'
% 'data.adv'
%
% 'index.verb'
% 'index.noun'
% 'index.adv'
% 'index.sense'
% 'index.adj'
%
% 'adj.exc'
% 'noun.exc'
% 'adv.exc'
% 'verb.exc'
%
% 'sents.vrb'
% 'frames.vrb'
% 'sentidx.vrb'
% 'lexnames'
% 'cntlist.rev'


