-module(sen).
-export([a/1]).

% Processes one sentence.
% Assumes clean input.
% Tries to infer a logical proposition.
% Attemps to detect compound sentences, and handle the compound proposition.

a(String)->
  CleanedBin = list_to_binary(String)
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

  Thought = think(CleanedBin)
  ,
  io:format(["\n", Thought, "\n\n"])
  ,
  Thought
  .

%  Assumes space delimitation
tokenise(Bin)->
  binary:split(Bin, <<" ">>, [global])
  .

think(Sentence)->
  BaseOntology = []
  ,
  WordList = tokenise(Sentence)
  ,
  case length(WordList) of
    0 ->
      "I don't know what to say.";
    1 ->
      oneWordSentence(WordList);
    2 ->
      twoWordSentence(WordList);
    _ ->
      nWordSentence(WordList)
  end
  .

oneWordSentence([Bin])->
  X =
    lists:map(
      fun(E)->
        [
          %element(2, element(8,E)),
          %element(2, element(3,E)),
          %element(2, element(6,E)),
          element(2, element(7,E)),
          "; "
        ]
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
  ,
  [X, "I heard a one-word-sentence."]
  .

twoWordSentence(WordList)->
  "I heard a two-word-sentence."
  .
nWordSentence(WordList)->  
  [
    lists:map(fun(Word)->["\t", oneWordSentence([Word]),"\n"] end, WordList),
    "I don't understand sentences that are longer than two-words-long yet."
  ]
  .
