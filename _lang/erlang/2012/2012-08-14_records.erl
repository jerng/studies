%%% getting to know the records syntax, towards improving it
%%%
-module(records).
-compile(export_all).

% analysis: there's no good reason to have N-tuples here
%           when field position queries produce results in
%           terms of (N+1)-tuples
-record(r,{field1,field2=default2,field3=default3}).

%% CREATE
%% returns {r,undefined,b,default3}
assignment()->
  #r{field2=b}.

%% CREATE
%% returns {r,x,b,x}
assignment_wildcard()->
  #r{field2=b,_=x}.

%% CREATE
%% returns {r,undefined,default2,default3}
assignment_empty()->
  #r{}.

%% CREATE
%% returns {r,undefined,{r,undefined,default2,default3},default3} 
assignment_nested()->
  #r{field2=#r{}}.

%% UPDATE (there should be no real "update" OF VARIABLES in Erlang)
%% returns {r,value1,new2,value3} 
modify()->
  R = {r,value1,value2,value3},
  R#r{field2=new2}.

%% READ
%% returns 4
field_positioning()->
  #r.field3.

%% READ
%% returns {r,value1,value2,value3}
read_whole()->
  R = {r,value1,value2,value3},
  R.

%% READ
%% returns value3 
read_partial()->
  R = {r,value1,value2,value3},
  R#r.field3.

%% READ
%% returns value2_3 
read_nested()->
  R = {r,value1_1,{r,value2_1,value2_2,value2_3},value1_3},
  R#r.field2#r.field3.
