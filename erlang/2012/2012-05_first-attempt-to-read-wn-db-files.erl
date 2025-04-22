-module(wni).
-export(
  [
    wn/1,
    speedtest/1
  ]).
-define(WN_DB_DIR_PATH, "/usr/share/wordnet/").

% Conventions:
%
% POS: Parts of Speech
-define(POS_ATOMS,[noun,verb,adj,adv]).

% wn/1
%
% Exposes an interface for this application.
% Currently used for testing functions in development.
%
% CALLS: load_data/0
% CALLS: normalise_search_term/0
%
% RESPEC:
%
%   Single Words
%     Search exception files
%       RETURN possible lemmas
%     Apply rules of detachment
%     Apply suffix:ful hack
%       RETURN possible lemmas
%
%   REDUCE returned lemmas to be unique per Part of Speech
%   SEARCH WordNet!! (hopefully by Tuesday)
%
% GRAND TODO:
%   Colocations (CL)
%     Break up individual words (IW)
%       Search exception files for IW
%       Apply rules of detachment for IW
%       Apply suffix:ful hack
%         Permutate CL with all possible IW lemmas & hyphen delimitation (?!?)
%           RETURN possible lemmas
%           

wn(Dirty_string)-> 
  load_data(),
  Morpheme_bin = 
    retain_first_morpheme( 
      normalise_search_term(
        Dirty_string)), % TODO: ignore > 1 words
  {
    % this is a quick hack for REPL prototyping - in production, use binaries instead
    <<"You queried: '", Morpheme_bin/binary, "'; now testing only the first morpheme; test output returned: ">>,
    
    % run tests here
    search_for_a_morpheme( Morpheme_bin )
    %get_lemmas_via_detachment_rules(Search_bin),
    %get_lemmas_via_exception_lists(Search_bin)
  }.

%% Scaffolded data loading
%

% load_data/0
%
% Loads ets table 'wn_files'
%
% CALLS: get_pos_exc_bin/1
%
% TODO: Move this out into its own application / gen_server
%
load_data()->

  case ets:info(wn_files) of
    undefined ->
      ets:new(wn_files,[set, named_table]),

      % TODO: refactor to another function X, with list comprehension?
      % where: init(  X(  get_pos_exc_bin()))
      ets:insert(wn_files,{"noun.exc", get_pos_exc_bin(noun)}),
      ets:insert(wn_files,{"verb.exc", get_pos_exc_bin(verb)}),
      ets:insert(wn_files,{"adj.exc",  get_pos_exc_bin(adj) }),
      ets:insert(wn_files,{"adv.exc",  get_pos_exc_bin(adv) }),
      
      ets:insert(wn_files,{"index.noun",  get_pos_index_bin(noun) }),
      ets:insert(wn_files,{"index.verb",  get_pos_index_bin(verb) }),
      ets:insert(wn_files,{"index.adj",   get_pos_index_bin(adj)  }),
      ets:insert(wn_files,{"index.adv",   get_pos_index_bin(adv)  }),

      ets:insert(wn_files,{"data.noun",  get_pos_data_bin(noun) }),
      ets:insert(wn_files,{"data.verb",  get_pos_data_bin(verb) }),
      ets:insert(wn_files,{"data.adj",   get_pos_data_bin(adj)  }),
      ets:insert(wn_files,{"data.adv",   get_pos_data_bin(adv)  }),

      ets:insert(wn_files,{"cntlist.rev", get_misc_bin("cntlist.rev") }),
      ets:insert(wn_files,{"sentidx.vrb", get_misc_bin("sentidx.vrb") }),
      ets:insert(wn_files,{"sents.vrb",   get_misc_bin("sents.vrb")   });
    [_|_] ->
      table_already_loaded
    end,
  
  case ets:info(wn_detachment_rules) of
    undefined ->
      ets:new(wn_detachment_rules, [named_table]),
      ets:insert(wn_detachment_rules, { all, 
        [
          { noun,<<"s">>,<<"">>},
          { noun,<<"ses">>,<<"s">>},
          { noun,<<"xes">>,<<"x">>},
          { noun,<<"zes">>,<<"z">>},
          { noun,<<"ches">>,<<"ch">>},
          { noun,<<"shes">>,<<"sh">>},
          { noun,<<"men">>,<<"man">>},
          { noun,<<"ies">>,<<"y">>},
          { verb,<<"s">>,<<"">>},
          { verb,<<"ies">>,<<"y">>},
          { verb,<<"es">>,<<"e">>},
          { verb,<<"es">>,<<"">>},
          { verb,<<"ed">>,<<"e">>},
          { verb,<<"ed">>,<<"">>},
          { verb,<<"ing">>,<<"e">>},
          { verb,<<"ing">>,<<"">>},
          { adj,<<"er">>,<<"">>},
          { adj,<<"est">>,<<"">>},
          { adj,<<"er">>,<<"e">>},
          { adj,<<"est">>,<<"e">>}
        ]});
    [_|_] ->
      table_already_loaded
    end.

% get_pos_exc_bin/1
%
% Obtains binary data of exception files 
% of the filename form POS.exc 
% from WN_DB_DIR_PATH
% for the first stage of morphological processing.
%
get_pos_exc_bin(Pos_atom)-> 
  {ok,Bin} = file:read_file( 
    ?WN_DB_DIR_PATH ++ atom_to_list(Pos_atom) ++ ".exc"
  ),
  Bin.

% get_pos_index_bin/1
%
get_pos_index_bin(Pos_atom)-> 
  {ok,Bin} = file:read_file( 
    ?WN_DB_DIR_PATH ++ "index." ++ atom_to_list(Pos_atom)
  ),
  Bin.

% get_pos_data_bin/1
%
get_pos_data_bin(Pos_atom)-> 
  {ok,Bin} = file:read_file( 
    ?WN_DB_DIR_PATH ++ "data." ++ atom_to_list(Pos_atom)
  ),
  Bin.

% get_misc_bin/1
%
get_misc_bin(Filename_string)-> 
  {ok,Bin} = file:read_file( 
    ?WN_DB_DIR_PATH ++ Filename_string
  ),
  Bin.

%
%%%%

% normalise_search_term/1
%
% SCAFFOLD - expect this to be wobbly, and to change.
% Converts list to binary, cleans, underscores.
%
% NOTE: the .exc(eption) files contain hyphenated terms, not dealt with here
%
% TODO: Scrub out punctuation EXCEPT for hyphens and apostrophes
% TODO: Figure out if there's shorter/faster code to do this
%
normalise_search_term(Search_string)->
  re:replace(       % 3. leading and trailing undescores to nought
    re:replace(     % 2. multiple to single underscores
      re:replace(   % 1. line-breaks to underscores
        list_to_binary(Search_string),
        <<"\n">>,
        <<"_">>,
        [global, {return, binary}]
      ),
      <<"\s{1,}">>,
      <<"_">>,
      [global, {return, binary}]
    ),
    <<"^_{1,}|_{1,}$">>,
    <<"">>,
    [global, {return, binary}]
  ).

% retain_first_morpheme/0
%
% Input must be normalised first, spaces converted to underscores.
% We assume hyphens, under-scores, and apostrophes, as morpheme delimiters.
%
% TODO: move delimiters into -define()
%
retain_first_morpheme(Search_bin)->
  case binary:match(Search_bin, [<<"_">>, <<"-">>,<<"'">>]) of
    nomatch ->
      Search_bin;
    {Match_position, _}->
      binary:part(Search_bin, {0, Match_position})
  end.

% search_for_a_morpheme
%
search_for_a_morpheme(Morpheme_bin)->
  Lemma_bin = Morpheme_bin,
  {
    get_derived_lemmas(Morpheme_bin),
    "search 'index.noun' for '" ++ binary_to_list(Lemma_bin) ++ "':",
    search_index_for_lemma(Lemma_bin),
    wip
  }.

% search_index_for_lemma/1
%
% TODO: account for header copyright notice in data files
%
search_index_for_lemma(Lemma_bin)->
  { 
    case 
    (
      binary:match( 
        ets:lookup_element(wn_files, "index.noun", 2),
        <<"\n", Lemma_bin/binary, " ">> )
    ) of
      nomatch ->
        [];
      { Row_start_at, Match_length}->
        Excs_start_at = 
          Row_start_at + Match_length,
        Bin = 
          ets:lookup_element(
            wn_files, 
            "index.noun",
            2 ),
        { Row_end_at, _ } = 
          binary:match(
            Bin,
            <<"\n">>,
            [ { scope,{ Excs_start_at, (byte_size(Bin)-Excs_start_at) } } ] ),
        [ Fields 
          || Fields <- 
            binary:split(
              binary:part( 
                Bin, 
                Excs_start_at, 
                (Row_end_at-Excs_start_at) ),
              <<" ">>,
              [global] ) ]
    end
  }.

% get_derived_lemmas/1
%
% lists:fold1( Scrubber(), Acc, List_to_scrub)
%
get_derived_lemmas(Morpheme_bin)->
  { lemmas, 
    lists:foldl
    (
      fun( {Pos_atom, Lemma}, Acc) -> 
        case ( 
          Lemma == nomatch 
          orelse Lemma == <<>> 
          orelse lists:member({Pos_atom,Lemma},Acc) ) of
            true ->
              Acc;
            false ->
              [ {Pos_atom, Lemma} | Acc ]
        end          
      end,
      [],
      lists:flatten
      (
        lists:map
        (
          fun get_lemmas_from_exception_files/1, 
          get_morpheme_offsets_from_exception_files(Morpheme_bin) 
        ),
        lists:map
        (
          fun get_lemmas_via_detachment_rules/1,
          [ { Morpheme_bin, Rule }
            || Rule <- ets:lookup_element(wn_detachment_rules, all, 2) ] 
        ) 
      ) 
    )
  }.

% get_lemmas_from_exception_files/1
%
get_lemmas_from_exception_files( { Pos_atom, nomatch } ) ->
  [ { Pos_atom, nomatch } ];
get_lemmas_from_exception_files( { Pos_atom, { Row_start_at, Match_length } } ) ->
  Excs_start_at = 
    Row_start_at + Match_length,
  Bin = 
    ets:lookup_element(
      wn_files, 
      atom_to_list(Pos_atom)++".exc",
      2 ),
  { Row_end_at, _ } = 
    binary:match(
      Bin,
      <<"\n">>,
      [ { scope,{ Excs_start_at, (byte_size(Bin)-Excs_start_at) } } ] ),
  [ {Pos_atom, Exceptions } 
    || Exceptions <- 
      binary:split(
        binary:part( 
          Bin, 
          Excs_start_at, 
          (Row_end_at-Excs_start_at) ),
        <<" ">>,
        [global] ) ].

% get_morpheme_offsets_from_exception_files/1
%
% Searches for exceptions in exception files
% for the first stage of morphological processing.
%
% READS FROM: ets table 'wn_files'
%
% TODO: bug - matches \n but not ^; replace binary:match with re:match
%
get_morpheme_offsets_from_exception_files(Morpheme_bin) when is_binary(Morpheme_bin)->
  lists:flatmap(  
    fun get_morpheme_offsets_from_exception_files/1, 
    [ {P_a, Morpheme_bin} || P_a <- ?POS_ATOMS]
  );
get_morpheme_offsets_from_exception_files({Pos_atom, Morpheme_bin})-> 
  [{  
    Pos_atom, 
    binary:match( 
      ets:lookup_element(wn_files, atom_to_list(Pos_atom)++".exc", 2),
      <<"\n", Morpheme_bin/binary, " ">>
    )
  }].

% get_lemmas_via_detachment_rules/1
%
% Handles single morphemes, not colocations.
%
get_lemmas_via_detachment_rules
( 
  { Morpheme_bin, { Pos_atom, Suffix_bin, Replacement_bin } } 
) ->
  { Pos_atom, 
    re:replace(
      Morpheme_bin, 
      <<Suffix_bin/binary,"$">>, 
      Replacement_bin,
      [{return, binary}] ) }.

% speedtest/1
%
% Utility function for benchmarking.
%
% TODO: move this to its own module.
%
speedtest(Cycles_int)->
  speedtest(now(),Cycles_int).
speedtest(Start_time,0)->  
  Now = now(),
  % 1:megaseconds; 2:seconds; 3:microseconds
  float_to_list( 
    element(2,Now) 
    + element(3,Now)/1000000
    - element(2, Start_time) 
    - element(3,Start_time)/1000000 
  ) ++ " seconds";
speedtest(Start_time, Cycles_int)->
  % code to be benchmarked goes here
  ets:lookup('index.noun',<<"apple">>),
  speedtest(Start_time, Cycles_int - 1).
