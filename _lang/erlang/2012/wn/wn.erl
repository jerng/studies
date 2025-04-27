-module(wn).
-export([dictionary/1]).

a(Input)->
  
  T1 = now(),

  
  InputBin = list_to_binary(Input),

  Output = 
    %find_duplicates()
    %make_new_data_table()
    %make_new_index_table()
    make_pointer_symbol_table()  
  ,
      
  { Output, 
    { miliseconds, timer:now_diff(now(), T1)/1000 } }
  .

dictionary(Input)->
  Bin = list_to_binary(Input)
  ,
  case ets:info(wn_index) of
    undefined ->
      ets:file2tab(
        "/home/jerng/Desktop/erlang/ets/wn_index.ets"
        %"C:\\Users\\jerng\\Google Drive\\2012\\erlang\\wn_index.ets"
        );
    Tx when is_list(Tx)->
      ignore 
  end
  ,
  case ets:info(wn_data) of
    undefined ->
      ets:file2tab(
        "/home/jerng/Desktop/erlang/ets/wn_data.ets"
        %"C:\\Users\\jerng\\Google Drive\\2012\\erlang\\wn_data.ets"
        );
    Ty when is_list(Ty)->
      ignore
  end
  ,

  lists:map(
    fun(E)->
      io:format( 
        [ 
          "\nConcept:\t", 
          lists:map(
            fun({Word,_})->
              [ Word, "; " ]
            end,
            element(2, element(3,E)) ), 
          "\n\t\t",
          element(2, element(7,E)),
          ";\n\t\t",
          integer_to_list(list_to_integer(binary_to_list(element(2, element(4,E))))),
          " pointers",
          ".\n\n"

        ] )
    end,
    ets:select( 
      wn_data, 
      lists:flatten(
        lists:map(
          fun(IndexResult)->
            { PartOfSpeech, SynsetOffsets } = IndexResult,
            [ { { { SynsetOffset, PartOfSpeech }, '$1' },
                [],
                ['$1'] }
              || SynsetOffset <- SynsetOffsets ] 
          end,
          ets:select( 
            wn_index,
            [ 
              { { Bin, '$1', '$2' },
                [],
                [ { { '$1', '$2' } } ] } 
            ] ) ) ) ) )
  .

make_pointer_symbol_table()->

  Data =
    [ { n,  [ {<<"!">>,   antonym}, 
              {<<"@">>,   hypernym}, 
              {<<"@i">>,  instance_hypernym}, 
              {<<"~">>,   hyponym}, 
              {<<"~i">>,  instance_hyponym}, 
              {<<"#m">>,  member_holonym },
              {<<"#s">>,  substance_holonym },
              {<<"#p">>,  part_holonym },
              {<<"%m">>,  member_meronym },
              {<<"%s">>,  substance_meronym },
              {<<"%p">>,  part_meronym },
              {<<"=">>,   attribute },
              {<<"+">>,   derivationally_related_form},
              {<<";c">>,  domain_of_synset_TOPIC },
              {<<"-c">>,  member_of_domain_TOPIC },
              {<<";r">>,  domain_of_synset_REGION },
              {<<"-r">>,  member_of_domain_REGION },
              {<<";u">>,  domain_of_synset_USAGE },
              {<<"-u">>,  member_of_domain_USAGE } ] },

      { v,  [ {<<"!">>,   antonym }, 
              {<<"@">>,   hypernym },
              {<<" ~">>,  hyponym },
              {<<"*">>,   entailment },
              {<<">">>,   cause },
              {<<"^">>,   also },
              {<<"$">>,   verb_group },
              {<<"+">>,   derivationally_related_form },        
              {<<";c">>,  domain_of_synset_TOPIC },
              {<<";r">>,  domain_of_synset_REGION },
              {<<";u">>,  domain_of_synset_USAGE } ] }, 

      { a, [  {<<"!">>,   antonym },
              {<<"&">>,   similar_to}, 
              {<<"<">>,   participle_of_verb}, 
              {<<"\\">>,  pertainym_to_noun}, 
              {<<"=">>,   attribute}, 
              {<<"^">>,   also_see}, 
              {<<";c">>,  domain_of_synset_TOPIC}, 
              {<<";r">>,  domain_of_synset_REGION}, 
              {<<";u">>,  domain_of_synset_USAGE} ] },    
      
      { r, [  {<<"!">>,   antonym}, 
              {<<"\\">>,  derived_from_adjective}, 
              {<<";c">>,  domain_of_synset_TOPIC}, 
              {<<";r">>,  domain_of_synset_REGION},
              {<<";u">>,  domain_of_synset_USAGE} ] } ]
  ,

  % Safely create empty table for new data
  case ets:info(wn_pointer_symbols) of
    undefined ->
      ignore;
    T when is_list(T) ->
      ets:delete(wn_pointer_symbols) end
  ,

  ets:new(wn_pointer_symbols, [set, named_table])
  ,

  % Save data to a single Ets table 
  lists:map(
    fun (E)-> ets:insert(wn_pointer_symbols, E) end,
    Data )
  ,

  % Save Ets table to file
  ets:tab2file(wn_pointer_symbols, 
    %"/home/jerng/Desktop/erlang/wn_data.ets"
    "C:\\Users\\jerng\\Google Drive\\2012\\erlang\\ets\\wn_pointer_symbols.ets"
    )
  
  .

find_duplicates()->
  case ets:info(wn_data) of
    undefined ->
      ets:file2tab(
        %"/home/jerng/Desktop/erlang/wn_data.ets"
        "C:\\Users\\jerng\\Google Drive\\2012\\erlang\\wn_data.ets"
        );
    T when is_list(T)->
      ignore end
  ,

  Keys = ets:select(wn_data,[{ 
    {'$1', ['$2',{'$3','$10'},'$4','$5','$6','$7','$8','$9']},
    [],
    [{{'$1','$10'}}] }])
  ,
  
  lists:foldl(
    fun(K,Counter)->
      case Counter rem 1000 of
        C when C == 0 ->
          io:format([integer_to_list(Counter)," rows done; "]);
        C when C /= 0 ->
          ignore end,
      lists:foldl(

        fun(E,AccIn)->
          if 
            E == K ->
              AccOut = AccIn + 1,
              if
                AccOut > 1 ->
                  io:format(["\n", K," found ", integer_to_list(AccOut), " times;\n"]);
                AccOut =< 1 ->
                  carryon end;
            E /= K ->
              AccOut = AccIn end,
          AccOut end,

        0,
        Keys),
      Counter + 1 end,
    0,
    Keys)
  .

make_new_data_table()->

  % Load source data
  loadWNDataFromETSFiles()
  ,

  % Safely create empty table for new data
  case ets:info(wn_data) of
    undefined ->
      ignore;
    T when is_list(T) ->
      ets:delete(wn_data) end
  ,
  ets:new(wn_data, [bag, named_table])
  ,

  OldData =
    lists:flatten(
      lists:map(
        fun(E1)->
          lists:map(
            
            fun(E2)->
              [ { SynsetOffset, Tuples } ] = E2,
              { { 
                  SynsetOffset, 
                  case element(2, lists:nth(2, Tuples)) of
                    Bin when Bin == <<"s">> ->
                      <<"a">>;
                    Bin when Bin /= <<"s">> ->
                      Bin
                  end
                },
                { 
                  lists:nth(1, Tuples),
                  lists:nth(3, Tuples),
                  lists:nth(4, Tuples),
                  lists:nth(5, Tuples),
                  lists:nth(6, Tuples),
                  lists:nth(7, Tuples),
                  lists:nth(8, Tuples),  
                  lists:nth(2, Tuples)  
                  } } end,
            
            ets:match(E1,'$1') ) end
        ,
        [ data.noun,
          data.verb,
          data.adj,
          data.adv ] ) )
  ,

  % Save OLD data to a single Ets table 
  lists:map(
    fun (E)-> ets:insert(wn_data, E) end,
    OldData )
  ,

  % Save Ets table to file
  ets:tab2file(wn_data, 
    "/home/jerng/Desktop/erlang/ets/wn_data.ets"
    %"C:\\Users\\jerng\\Google Drive\\2012\\erlang\\ets\\wn_data.ets"
    )
  
  .

make_new_index_table()->

  % Load source data
  loadWNDataFromETSFiles()
  ,
  
  % Safely create empty table for new data
  case ets:info(wn_index) of
    undefined ->
      ignore;
    T when is_list(T) ->
      ets:delete(wn_index) end
  ,
  ets:new(wn_index, [bag, named_table])
  ,

  OldData =
    lists:flatten(
      lists:map(
        fun(E)->ets:match(E,'$1')end
        ,
        [ index.noun,
          index.verb,
          index.adj,
          index.adv ] ) )
  ,
  
  NewData = lists:map( 
    fun (E)->
      { Morpheme, TupleList } = E,
      { Morpheme, 
        element( 2, lists:nth(1, TupleList) ),
        element( 2, lists:nth(7, TupleList) )
        } end, 
    OldData )
  ,

  % Save new data to Ets table 
  lists:map(
    fun (E)-> ets:insert(wn_index, E) end,
    NewData )
  ,

  % Save Ets table to file
  ets:tab2file(wn_index, "/home/jerng/Desktop/erlang/wn_index.ets")
  .

loadWNDataFromETSFiles()->

  FilePath = 
     "/home/jerng/Desktop/erlang/ets/"
    %"C:\\Users\\jerng\\Google Drive\\2012\\erlang\\ets\\"
  ,

  FileNames = [
    
    %"noun.exc.ets",
    %"verb.exc.ets",
    %"adj.exc.ets",
    %"adv.exc.ets",
    
    %"index.noun.ets",
    %"index.verb.ets",
    %"index.adj.ets",
    %"index.adv.ets",
    
    "data.noun.ets",
    "data.verb.ets",
    "data.adj.ets",
    "data.adv.ets",
    
    %"frames.vrb.ets",
    %"sentidx.vrb.ets",
    %"sents.vrb.ets",
    
    %"lexnames.ets",
    %"cntlist.rev.ets",
    %"index.sense.ets" 

    "wn_index.ets",
    "wn_data.ets",
    "wn_pointer_symbols.ets"
    ],

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
  
  lists:map(
    fun ets:file2tab/1,
    MakeFilePaths(
      MakeFilePaths, 
      FilePath,
      FileNames, 
      length(FileNames), 
      [] ) )
  ,
  io:format("\n\nLoading data.\n\n"),
  ets:i(),
  io:format("\nData loaded.\n\n")
  .
