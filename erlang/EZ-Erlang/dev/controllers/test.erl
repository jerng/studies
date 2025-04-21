-module(test).
-export([default/3]).
-include("../../lib/ez_c.hrl").

default
  ?BEGIN
  Response 
  = [ { view,test.view} ],
  ?END.
