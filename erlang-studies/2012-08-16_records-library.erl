%%%----------------------------------------------------------------------------
%%% This attempts a hash library, with a front-end that looks like Erlang's
%%% records, and a back-end that maximally utilises Erlang's R15 Stdlib
%%%
%%% Dev util:
%%% c(r), r:define(test,{field1,field2,{field3,default3}}).
%%%
%%%----------------------------------------------------------------------------
-module(r).
-compile(export_all).

%%-----------------------------------------------------------------------------
%% This should work like the form -record(name,{field1,field2... fieldN})
%%
%% Fields should be a proplist
%%
%% TODO: question if there is too much defensive coding here
%%
%%-----------------------------------------------------------------------------
define(Name,Fields) ->
  {Mapped,_Acc} 
    = lists:mapfoldl(
        fun(E,Acc)-> 
          case E of 
              A when is_atom(A) ->
                { {A,undefined,Acc}, Acc+1 };
              {A,X} when is_atom(A) ->
                { {A,X,Acc}, Acc+1 } end end, 
        1,
        tuple_to_list(Fields) ),
  { definition, { Name, Mapped } }.

%%-----------------------------------------------------------------------------
%% AssignedValue should be a proplist
%%-----------------------------------------------------------------------------

create(RD) -> create(new,RD,[]).

create(R,RD,AssignedMap) ->

  Wild = proplists:get_value('_',AssignedMap),
  {Name,DefaultMap} = RD,

  case R of
    new -> 
      AssumedMap = DefaultMap;
    R -> 
      case element(1,R) of
        Name -> ok;
        _ -> erlang:error(record_name_mismatch) end,
      case (size(R) == (1+length(DefaultMap))) of
        true -> ok;
        false -> erlang:error(record_arity_mismatch) end,
      ExtantMap 
        = lists:map(
            fun({FieldName,_DefaultValue,FieldPosition})-> 
              {FieldName,element(FieldPosition+1,R),FieldPosition} end,
            DefaultMap),
      AssumedMap = ExtantMap end,

  ValuesOut 
    = lists:map(
        fun({FieldName,DefaultValue,_FieldPosition})-> 
          (case proplists:get_value(FieldName,AssignedMap) of
            undefined -> 
              (case Wild of
                undefined -> DefaultValue;
                Wild -> Wild end);
            AssignedValue -> 
              AssignedValue end) end,
        AssumedMap ),

  list_to_tuple( [ Name | ValuesOut ] ).

%%-----------------------------------------------------------------------------
%%
%%-----------------------------------------------------------------------------
position(Field,RD)->
  {_Name,DefaultValues} = RD,
  case proplists:lookup(Field,DefaultValues) of
    none -> none; %TODO: check how records do it
    Tuple -> element(3,Tuple) end.

%%-----------------------------------------------------------------------------
%%
%%-----------------------------------------------------------------------------
test()->
  
  {definition,RD} = define(test,[field1,field2,{field3,default3}]),
  ExtantInstance = {test,extant1,extant2,extant3}, 

  { { definition, RD },
    { default_instance, create(RD) },
    { partially_assigned_instance, create(new,RD,[{field2,value2}]) },
    { fully_assigned_instance, create(new,RD,[{field2,value2},{field1,value1},{field3,value3}]) },
    { wild_assigned_instance, create(new,RD,[{field2,value2},{'_',wild_value}]) },
    { from_extant_instance, create(ExtantInstance,RD,[{field2,updated_value}]) },
    { position_of_field3, position(field3,RD)}
  }.
