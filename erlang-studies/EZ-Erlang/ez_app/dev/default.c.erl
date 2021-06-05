-module(default.c).
-export([default/1]).

default(Request)->
  Headers =  [{"Content-Type", "text/html"}],
  ViewModule =    default.default.v, 
  ViewBindings =  [],
  { view,
    Headers,
    ViewModule,   
    ViewBindings }.
