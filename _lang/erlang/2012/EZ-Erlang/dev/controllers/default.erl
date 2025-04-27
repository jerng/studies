%%%-----------------------------------------------------------------------------
%%% When a HTTP client request is caught by Inets, Mod_esi will send back
%%% the result of module:function/3 where:
%%%
%%% scheme://server_name/esi_script_alias/module/function
%%%
%%% So, THIS IS WHERE WE START coding the response routines.
%%%
%%%-----------------------------------------------------------------------------
-module(default).
-export([default/3]).
-include("../../lib/ez_c.hrl").


%%% This is the default:default/3 EZ "controller:action" example.
%%default
%%
%%% This macros is defined in ez/ez_c.hrl.
%%?BEGIN
%%
%%  % At this point, the values from Inets's Mod_esi have been bound to the
%%  % variables Session, Env, and Input (names taken from the Mod_esi manual).
%%  % These have also been wrapped as a proplist, and bound to Request, which
%%  % is also available in Views. EZ Developers then may use the same semantics,
%%  % "proplists:(key,Response)", to access session, env, and input values in 
%%  %  both Controllers and Views.
%%  %
%%  % The following is the minimal Controller:Action logic.
%%  % Refer to the END macro definition, for defaults that can be overwritten.
%%  Response = [],
%%
%%% This macros is defined in ez/ez_c.hrl.
%%?END.

default
?BEGIN
  ChildViewBindings = [ {'A',some_value_in_variable_A} ],
  Response 
  = [ { view,default_template.view},
      { bindings, 
        [ 
          % Method 1:
          % This is the cheaper way to call child Views,
          % in terms of code length, and memory usage.
          { 'ChildViewBindings',ChildViewBindings},

          % Method 2:
          % This lengthens the code a bit, and requires more memory.
          % On the up-side, you can bind child views' variables in the 
          % Controller, and reduce complexity in the parent View.
          { 'ChildViewAsClosure',
            fun() -> default.view:splice(ChildViewBindings) end
          }
        ] 
      } 
    ],
?END.

%% TODO TODO TODO TODO TODO TODO TODO TODO 
%% TODO: figure out Mod_put
