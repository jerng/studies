-module(wnd).
-export(
  [
    a/0
  ]).

% Conventions:
%
% POS: Parts of Speech
-define(POS_ATOMS,[noun,verb,adj,adv]).

a()->
  etsify_wn_index_sense_file()
  .

etsify_wn_index_sense_file()->
  PATH_IN   = "/home/jerng/Desktop/WordNet-3.0/dict_temp/",
  PATH_OUT  = "/home/jerng/Desktop/erlang/",
  FILE_IN   = "index.sense",
  FILE_OUT  = "index.sense.ets",
  {ok,Source_file_bin} = 
    file:read_file(
      PATH_IN ++ FILE_IN )
  ,
  Trimmed_rows_list = scrub_bin(Source_file_bin)
  ,
  Appropriated_list = 
    lists:map(
      fun(E)->
        [ 
          Sense_key,
          Synset_offset,
          Sense_number,
          Tag_count ] = chop(E,3),
        {
          Sense_key,
          [
            {synset_offset, Synset_offset},
            {sense_number, Sense_number},
            {tag_count, Tag_count } ] } end, 
      Trimmed_rows_list )
  ,
  ets:new(
    list_to_atom(FILE_IN), 
    [ set, named_table ])
  ,
  lists:map(
    fun ({K,V}) ->
      ets:insert(
        list_to_atom(FILE_IN),
        {K, V} ) end,
    Appropriated_list )
  ,
  ets:tab2file(
    list_to_atom(FILE_IN),
    PATH_OUT ++ FILE_OUT )
  ,
  io:format("\nJob done.")
  .

% can also be used for frames.vrb
%
etsify_wn_sents_file()->
  PATH_IN   = "/usr/share/wordnet/",
  PATH_OUT  = "/home/jerng/Desktop/erlang/",
  FILE_IN   = "sents.vrb",
  FILE_OUT  = "sents.vrb.ets",
  {ok,Source_file_bin} = 
    file:read_file(
      PATH_IN ++ FILE_IN )
  ,
  Trimmed_rows_list = scrub_bin(Source_file_bin)
  ,
  Appropriated_list = 
    lists:map(
      fun(E)->
        [Template_sentence_number, Template_sentence] = chop(E,1),
        {
          Template_sentence_number,
          [
            {template_sentence, Template_sentence } ] } end, 
      Trimmed_rows_list ) 
  ,
  ets:new(
    list_to_atom(FILE_IN), 
    [ set, named_table ])
  ,
  lists:map(
    fun ({K,V}) ->
      ets:insert(
        list_to_atom(FILE_IN),
        {K, V} ) end,
    Appropriated_list )
  ,
  ets:tab2file(
    list_to_atom(FILE_IN),
    PATH_OUT ++ FILE_OUT )
  ,
  io:format("\nJob done.")
  .

etsify_wn_sentidx_file()->
  PATH_IN   = "/usr/share/wordnet/",
  PATH_OUT  = "/home/jerng/Desktop/erlang/",
  FILE_IN   = "sentidx.vrb",
  FILE_OUT  = "sentidx.vrb.ets",
  {ok,Source_file_bin} = 
    file:read_file(
      PATH_IN ++ FILE_IN )
  ,
  Trimmed_rows_list = scrub_bin(Source_file_bin)
  ,
  Appropriated_list = %fugly?
    lists:map(
      fun(E)->
        case 
          (binary:match(E, <<" ">>)) of
            (nomatch)->
              Sense_key = E,
              Template_sentence_numbers = [];
            (_)->
              [ Sense_key,
                Template_sentence_numbers ] = chop(E,1) end,
        {
          Sense_key,
          [
            {template_sentence_numbers, Template_sentence_numbers } ] } end, 
      Trimmed_rows_list ) 
  ,
  ets:new(
    list_to_atom(FILE_IN), 
    [ set, named_table ])
  ,
  lists:map(
    fun ({K,V}) ->
      ets:insert(
        list_to_atom(FILE_IN),
        {K, V} ) end,
    Appropriated_list )
  ,
  ets:tab2file(
    list_to_atom(FILE_IN),
    PATH_OUT ++ FILE_OUT )
  ,
  io:format("\nJob done.")
  .

% can be easily modified for lexnames
%
etsify_wn_cntlist_rev_file()->
  PATH_IN   = "/usr/share/wordnet/",
  PATH_OUT  = "/home/jerng/Desktop/erlang/",
  FILE_IN   = "cntlist.rev",
  FILE_OUT  = "cntlist.rev.ets",
  {ok,Source_file_bin} = 
    file:read_file(
      PATH_IN ++ FILE_IN )
  ,
  Trimmed_rows_list = scrub_bin(Source_file_bin)
  ,
  Appropriated_list = 
    lists:map(
      fun(E)->
        [ 
          Sense_key,
          Sense_number,
          Tag_count ] = chop(E,2),
        {
          Sense_key,
          [
            {sense_number, Sense_number},
            {tag_count, Tag_count } ] } end, 
      Trimmed_rows_list )
  ,
  ets:new(
    list_to_atom(FILE_IN), 
    [ set, named_table ])
  ,
  lists:map(
    fun ({K,V}) ->
      ets:insert(
        list_to_atom(FILE_IN),
        {K, V} ) end,
    Appropriated_list )
  ,
  ets:tab2file(
    list_to_atom(FILE_IN),
    PATH_OUT ++ FILE_OUT )
  ,
  io:format("\nJob done.")
  .

etsify_wn_data_file()->
  PATH_IN   = "/usr/share/wordnet/",
  PATH_OUT  = "/home/jerng/Desktop/erlang/",
  FILE_IN   = "data.adv",
  FILE_OUT  = "data.adv.ets",
  {ok,Source_file_bin} = 
    file:read_file(
      PATH_IN ++ FILE_IN )
  ,
  Trimmed_rows_list = scrub_bin(Source_file_bin)
  ,
  % data.verb, test with row 101
  % data.adv, test with row 70
  Test_row = lists:nth(101,Trimmed_rows_list)
  ,
  io:format("\nTest Row:\n\n")
  ,
  io:put_chars(Test_row)
  ,
  io:format("\n\n")
  ,
  Appropriate = fun
    (Bin)->
      First_5_bins = chop(Bin, 4),
      Word_count = 
        list_to_integer(
          binary_to_list(
            lists:nth(4, First_5_bins) ),
          16)
      ,
      Get_words = fun
        % branch 2 (final)
        (Rem_bin, 0, Acc_list, Fn) ->
          {Rem_bin, lists:reverse(Acc_list)};
        % branch 1
        (Source_bin, N, Acc_list, Fn) ->
          [ 
            Word, 
            Lex_id, 
            Rem_bin ] = chop(Source_bin, 2),
          Fn(
            Rem_bin, 
            N-1, 
            [{Word, Lex_id}|Acc_list],
            Fn) end
      ,
      {Post_words_bin, Words} = Get_words(
        lists:last(First_5_bins),
        Word_count,
        [],
        Get_words)
      ,
      Pointer_count_etc_bins =  
        chop(Post_words_bin, 1)
      ,
      Pointer_count =  
        list_to_integer(
          binary_to_list(
            lists:nth(1,Pointer_count_etc_bins) ) )
      ,
      Get_pointers = fun
        % branch 2 (final)
        (Rem_bin, 0, Acc_list, Fn) ->
          {Rem_bin, lists:reverse(Acc_list)};
        % branch 1
        (Source_bin, N, Acc_list, Fn) ->
          [ 
            Pointer, 
            Synset_offset, 
            Part_of_speech, 
            Source_target, 
            Rem_bin ] = chop(Source_bin, 4),
          Fn(
            Rem_bin, 
            N-1, 
            [
              { Pointer, Synset_offset, Part_of_speech, Source_target}
              | Acc_list ],
            Fn) end
      ,
      {Post_pointers_bin, Pointers} = Get_pointers(
        lists:last(Pointer_count_etc_bins),
        Pointer_count,
        [],
        Get_pointers)
      ,
      if 
        (FILE_IN == "data.verb") ->
          Frame_count_etc_bins =
            chop(Post_pointers_bin,1),
          Frame_count =  
            list_to_integer(
              binary_to_list(
                lists:nth(1, Frame_count_etc_bins) ) )
          ,
          Get_frames = fun
            % branch 2 (final)
            (Rem_bin, 0, Acc_list, Fn) ->
              {Rem_bin, lists:reverse(Acc_list)};
            % branch 1
            (Source_bin, N, Acc_list, Fn) ->
              [ 
                _, 
                Frame_number, 
                Word_number, 
                Rem_bin ] = chop(Source_bin, 3),
              Fn(
                Rem_bin, 
                N-1, 
                [
                  { Frame_number, Word_number}
                  | Acc_list ],
                Fn) end
          ,
          {Post_frames_bin, Frames} = Get_frames(
            lists:last(Frame_count_etc_bins),
            Frame_count,
            [],
            Get_frames)
          ,
          Gloss_bins = chop(Post_frames_bin,1)
          ;
        (FILE_IN /= "data.verb") ->
          Frame_count_etc_bins = ignore,
          Frame_count = ignore,
          Post_frames_bin = ignore,
          Frames = ignore,
          Gloss_bins = chop(Post_pointers_bin,1) end
      ,
      {
        lists:nth(1,First_5_bins),
        [
          {lex_filenum,
            lists:nth(2,First_5_bins)},
          {synset_type,
            lists:nth(3,First_5_bins)},
          {word_count,
            lists:nth(4,First_5_bins)},
          {words, Words},
          {pointer_count,
            lists:nth(1,Pointer_count_etc_bins)},
          {pointers, Pointers},
          {frames,Frames},
          {gloss,lists:last(Gloss_bins)}
        ]
      }
    end
  ,
  io:write(length(Trimmed_rows_list))
  ,
  io:format(" rows detected. Parsing...\n\n")
  ,
  { Appropriated_list, _ } = lists:mapfoldl(
    fun(E,Acc)->
      case (Acc rem 5000) of
        0 ->
          io:write(Acc),
          io:format(" rows processed; \n");
        _ ->
          pass
      end,
      {Appropriate(E), Acc+1} end,
    1,
    Trimmed_rows_list)
  ,
  io:format("\nSaving to ets, then to file...")
  ,
  ets:new(
    list_to_atom(FILE_IN), 
    [ set, named_table ])
  ,
  lists:map(
    fun ({K, V}) ->
      ets:insert(
        list_to_atom(FILE_IN),
        {K, V} ) end,
    Appropriated_list )
  ,
  ets:tab2file(
    list_to_atom(FILE_IN),
    PATH_OUT ++ FILE_OUT )
  ,
  io:format("\nJob done.")
  .

etsify_wn_index_file()->
  PATH_IN   = "/usr/share/wordnet/",
  PATH_OUT  = "/home/jerng/Desktop/erlang/",
  FILE_IN   = "index.adv",
  FILE_OUT  = "index.adv.ets",
  {ok,Source_file_bin} = 
    file:read_file(
      PATH_IN ++ FILE_IN )
  ,
  Trimmed_rows_list = scrub_bin(Source_file_bin)
  ,
  Appropriate = fun
    (Bin)->
      First_5_bins = chop(Bin, 4),
      Synset_count = 
        list_to_integer(
          binary_to_list(
            lists:nth(3, First_5_bins) ) ),
      Pointer_count = 
        list_to_integer(
          binary_to_list(
            lists:nth(4, First_5_bins) ) ),
      Pointers_etc_bins = 
        chop(
         lists:last(First_5_bins),
         Pointer_count ),
      Two_post_pointer_bins = 
        chop(
          lists:last(Pointers_etc_bins),
          2),
      Synset_offset_bins = 
        chop(
          lists:last(Two_post_pointer_bins),
          Synset_count -1 ),
      { 
        lists:nth(1,First_5_bins),
        [ 
          {part_of_speech,
            lists:nth(2,First_5_bins)},
          {synset_count,
            lists:nth(3,First_5_bins)},
          {pointer_count,
            lists:nth(4,First_5_bins)},
          {pointers,
            lists:takewhile(
              fun(E)-> E /= lists:last(Pointers_etc_bins) end,
              Pointers_etc_bins)},
          {sense_count,
            lists:nth(1,Two_post_pointer_bins)},
          {tagsense_count,
            lists:nth(2,Two_post_pointer_bins)},
          {synset_offsets, Synset_offset_bins}
        ]
     } end
  ,
  io:write(length(Trimmed_rows_list))
  ,
  io:format(" rows detected. Parsing...\n\n")
  ,
  { Appropriated_list, _ } = lists:mapfoldl(
    fun(E,Acc)->
      case (Acc rem 5000) of
        0 ->
          io:write(Acc),
          io:format(" rows processed; \n");
        _ ->
          pass
      end,
      {Appropriate(E), Acc+1} end,
    1,
    Trimmed_rows_list)
  ,
  io:format("\nSaving to ets, then to file...")
  ,
  ets:new(
    list_to_atom(FILE_IN), 
    [ set, named_table ])
  ,
  lists:map(
    fun ({K, V}) ->
      ets:insert(
        list_to_atom(FILE_IN),
        {K, V} ) end,
    Appropriated_list )
  ,
  ets:tab2file(
    list_to_atom(FILE_IN),
    PATH_OUT ++ FILE_OUT )
  ,
  io:format("\nJob done.")
  .

etsify_wn_exc_file()->
  PATH_IN   = "/usr/share/wordnet/",
  PATH_OUT  = "/home/jerng/Desktop/erlang/",
  FILE_IN   = "adv.exc",
  FILE_OUT  = "adv.exc.ets",
  {ok,Source_file_bin} = 
    file:read_file(
      PATH_IN ++ FILE_IN ),
  Rows_list = binary:split(
    Source_file_bin, 
    <<"\n">>,
    [global] )
  ,
  Split_rows_list = lists:map(
    fun (A)->
      binary:split(
        A,
        <<" ">>,
        [global] ) end,
    Rows_list )
  ,
  ets:new(
    list_to_atom(FILE_IN), 
    [ set, named_table ])
  ,
  lists:map(
    fun ([H|T]) ->
      ets:insert(
        list_to_atom(FILE_IN),
        {H,T} ) end,
    Split_rows_list )
  ,
  ets:tab2file(
    list_to_atom(FILE_IN),
    PATH_OUT ++ FILE_OUT )  
  .

% Utility function.
%
scrub_bin(Source_file_bin)->
  Rows_list = binary:split(
    Source_file_bin, 
    <<"\n">>,
    [global] )
  ,
  io:format("Rows obtained. Filtering...\n")
  ,
  Filtered_rows_list =
    lists:filter(
      fun ( Row ) ->
        case ( re:run( Row, "^  |^$") ) of
          nomatch ->
            true;
          _ ->
            false
        end end,
      Rows_list )
  ,
  io:format("Rows filtered. Trimming...\n")
  ,
  Trimmed_rows_list = 
    lists:map(
      fun ( Row ) ->
        re:replace( 
          Row, 
          " {1,}$", 
          "", 
          [ {return, binary }] ) end,
      Filtered_rows_list )
  ,
  io:format("Rows trimmed. Tuple-ising...\n")
  ,
  Trimmed_rows_list
  .

% Utility function.
%
chop(Bin, N) ->
  % a. define inner function
  Fn = fun
    % a.3. this branch handles the final result
    (_, Result, 0 ) -> 
      if
        is_list(Result) ->
          lists:flatten(Result);
        is_binary(Result) ->
          [Result];
        true ->
          chop_is_confused end
    ;
    % a.2. this branch handles the inner loop
    (Fn, [Att, NakedBin], InnerN) -> 
      [Tok,  Rem] = binary:split(NakedBin, <<" ">>),
      Fn(Fn, [[Att, Tok] , Rem], InnerN-1)
    ;
    % a.1. this branch adds an attractor to the naked binary
    (Fn, NakedBin, InnerN) when is_binary(NakedBin) ->
      Fn(Fn, [[], NakedBin], InnerN) end
  ,
  % b. fire inner function
  Fn(Fn, Bin, N)
  .
