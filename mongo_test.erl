% git clone git://github.com/TonyGen/bson-erlang.git bson
% git clone git://github.com/TonyGen/mongodb-erlang.git mongodb
% cd bson
% erlc -o ebin -I include src/*.erl
% cd ../mongodb
% erlc -o ebin -I include -I .. src/*.erl
% cd ..


% keys must be atoms
% values can be binary (coerced to BSON text), 
%               atoms (coerced to BSON text),
%               lists (coerced to BSON array)
%               tuples (coerced to BSON object)
%      Data = {  key,<<"val">>,
%                key2,val2,
%                key3,"val3",
%                key4, 999.99,
%                key5, 999,
%                key6, ["hi",9,9.9,<<"bye">>],
%                key7, { a,b }
%             },


-module(mongo_test).
-export([main/0]).

main()->
  application:start(mongodb),
  Host = localhost, % Host = {localhost,27017},
  {ok,Conn} = mongo:connect(Host),


%  Action = fun()-> mongo:find(team,{}) end,
%  {ok,Cursor} = mongo:do(safe,master,Conn,teams,Action),
%  mongo:rest(Cursor).

  Action = 
    fun() ->
      %mongo:delete(some_collection,{}),
      %mongo:insert(some_collection, Data)
      %mongo:command({drop,some_collection})
      %mongo:insert_all(,transfer:main())
    end,
  {ok,Out} = mongo:do(safe,master,Conn,wn,Action),
  
  Out.
