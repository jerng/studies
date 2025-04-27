%test conclusions:
%- Gb_trees are the fastest to read from, by a huge margin
%- Dicts are marginally smaller, marginally faster to append, and 2-3x slower to read from than Gb_trees
%- Proplists take the smallest amount of memory, but are >10x slower to read from than Gb_trees
%- Proplists are the fastest to append to, but not much better than Dicts or Gb_trees
%- Orddicts are big, and slow
%
% The multiples are for a set of 10000 random floats as data, with 10000 integers as keys, looking up the key 8759.
% This is obviously an abstraction - the big-O notation would be more descriptive.

-module(hashes).
-compile(export_all).

bench()->
  Index =     lists:seq(1,10000),
  Data  =     [ random:uniform() || _ <- Index ],
  KeyToFind = 8759,
  
  EmptyProplist = [],
  EmptyOrddict =  orddict:new(),
  EmptyDict =     dict:new(),
  EmptyGbTree =   gb_trees:empty(),
  
  {ProplistCreateTime,Proplist}
    = timer:tc( 
        lists,
        foldl,
        [ fun(E,Acc)-> [{E,lists:nth(E,Data)}|Acc] end, 
          EmptyProplist, 
          Index ] ),
  {OrddictCreateTime,Orddict}
    = timer:tc(
        lists,
        foldl,
        [ fun(E,Acc)-> orddict:append(E,lists:nth(E,Data),Acc) end, 
          EmptyOrddict, 
          Index ] ),
  {DictCreateTime,Dict}
    = timer:tc( 
        lists,
        foldl,
        [ fun(E,Acc)-> dict:append(E,lists:nth(E,Data),Acc) end, 
          EmptyDict, 
          Index ] ),
  {GbTreeCreateTime,GbTree}    
    = timer:tc(
        lists,
        foldl,
        [ fun(E,Acc)-> gb_trees:insert(E,lists:nth(E,Data),Acc) end, 
          EmptyGbTree, 
          Index ] ),

  ProplistSize =  byte_size(term_to_binary(Proplist)),
  OrddictSize =   byte_size(term_to_binary(Orddict)),
  DictSize =      byte_size(term_to_binary(Dict)),
  GbTreeSize =    byte_size(term_to_binary(GbTree)),

  {ProplistReadTime,_}
    = timer:tc( 
        proplists,
        get_value,
        [ KeyToFind,
          Proplist  ] ),
  {OrddictReadTime,_}
    = timer:tc(
        orddict,
        fetch,
        [ KeyToFind,
          Orddict ] ),
  {DictReadTime,_}
    = timer:tc( 
        dict,
        fetch,
        [ KeyToFind,
          Dict ] ),
  {GbTreeReadTime,_}    
    = timer:tc(
        gb_trees,
        get,
        [ KeyToFind,
          GbTree ] ),

  [ 
    {proplist,ProplistSize/1024, ProplistCreateTime/1000000,  ProplistReadTime},
    {orddict, OrddictSize/1024,  OrddictCreateTime/1000000,   OrddictReadTime},
    {dict,    DictSize/1024,     DictCreateTime/1000000,      DictReadTime},
    {gb_tree, GbTreeSize/1024,   GbTreeCreateTime/1000000,    GbTreeReadTime}
  ]
  .
