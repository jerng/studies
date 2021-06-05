-module(wn).
-export([a/1]).

a(Input)->
  
  T1 = now(),

  
  InputBin = list_to_binary(Input),
  
  loadWNDataFromETSFiles(),

  Output =
    check_offsets()
  ,
      
  { { miliseconds, timer:now_diff(now(), T1)/1000 }, 
    Output }
  .

check_offsets()->
  
  N = sets:from_list(ets:foldl(fun(E, AccIn)->{Key,Value}=E,[Key|AccIn] end,[],data.noun)),
  V = sets:from_list(ets:foldl(fun(E, AccIn)->{Key,Value}=E,[Key|AccIn] end,[],data.verb)),
  A = sets:from_list(ets:foldl(fun(E, AccIn)->{Key,Value}=E,[Key|AccIn] end,[],data.adj)),
  R = sets:from_list(ets:foldl(fun(E, AccIn)->{Key,Value}=E,[Key|AccIn] end,[],data.adv)),
  
  { 
    sets:to_list( sets:intersection(N,V) ),
    sets:to_list( sets:intersection(N,A) ),
    sets:to_list( sets:intersection(N,R) ),

    sets:to_list( sets:intersection(V,A) ),
    sets:to_list( sets:intersection(V,R) ),

    sets:to_list( sets:intersection(A,R) )
  }
  .

linkMorphemeToMorphemes(Binary)->
  
  SynsetOffsetsByPartOfSpeech = linkMorphemeToSynsets(Binary),
  
  Morphemes = lists:foldl(
    fun (E, Acc) ->
      [ linkSynsetToMorphemes(E) | Acc ] end,
    [],
    SynsetOffsetsByPartOfSpeech)

  .

linkMorphemeToSynsets(Binary)->
  
  IndexResults = searchETSIndicesForMorpheme(Binary),

  SynsetOffsetsByPartOfSpeech = lists:foldl(
    fun 
      ([], Acc)->
        Acc;
      (E, Acc)->
        [ { _, TupleList } ] = E,
        { part_of_speech, PartOfSpeech } = lists:nth(1, TupleList),
        { synset_offsets, SynsetOffsets } = lists:last(TupleList),
        
        [ {PartOfSpeech, SynsetOffsets}  | Acc ]
        end,
    [],
    IndexResults )

  .

linkSynsetToMorphemes({PartOfSpeech, SynsetOffsets}) ->
  
  DataResults = searchETSDataForSynset(PartOfSpeech, SynsetOffsets),
  
  Morphemes = lists:map(
    fun (E)->
      [ { X , TupleList } ] = E,
      {words, Words} = lists:nth(4, TupleList),
      lists:map(
        fun(E)->
          { Word, LexId } = E,
          Word
          end, 
        Words)
      end,
    DataResults )
  .

searchETSDataForSynset(PartOfSpeech, SynsetOffsets)->
  
  Table = if 
    PartOfSpeech == <<"n">> ->
      data.noun;
    PartOfSpeech == <<"v">> ->
      data.verb;
    PartOfSpeech == <<"a">> orelse PartOfSpeech == <<"s">> ->
      data.adj;
    PartOfSpeech == <<"r">> ->
      data.adv end,

  Matches = lists:foldl(
    fun (SynsetOffset, Acc)->
      [ ets:lookup(Table, SynsetOffset)
        | Acc ] end,
    [],
    SynsetOffsets )
  .

searchETSIndicesForMorpheme(Binary)->
  Matches = lists:foldl(
    fun (E, Acc)->
      [ ets:lookup( E, Binary )
        | Acc ] end,
    [],
    [ index.noun,
      index.verb,
      index.adj,
      index.adv ])
  .

loadWNDataFromETSFiles()->

  FilePath = "/home/jerng/Desktop/erlang/ets/",

  FileNames = [
    
    "noun.exc.ets",
    "verb.exc.ets",
    "adj.exc.ets",
    "adv.exc.ets",
    
    "index.noun.ets",
    "index.verb.ets",
    "index.adj.ets",
    "index.adv.ets",
    
    "data.noun.ets",
    "data.verb.ets",
    "data.adj.ets",
    "data.adv.ets",
    
    "frames.vrb.ets",
    "sentidx.vrb.ets",
    "sents.vrb.ets",
    
    "lexnames.ets",
    "cntlist.rev.ets",
    "index.sense.ets" ],

  MakeFilePaths = fun
    (_, _, _, 0, AccPaths)->
      AccPaths;
    (Fn, Path, Names, FileCount, AccPaths)->
      Fn(
        Fn,
        Path,
        Names,
        FileCount -1,
        [ [Path, lists:nth(FileCount,Names)] | AccPaths ] ) end,
  
  FilePaths = lists:map(
    fun ets:file2tab/1,
    MakeFilePaths(
      MakeFilePaths, 
      FilePath,
      FileNames, 
      length(FileNames), 
      [] ) )
  
  .
