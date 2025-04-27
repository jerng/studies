%%%-----------------------------------------------------------------------------
%%% TODO: remove some defensive case-of-end expressions
%%% TODO: figure out the Erlang way to write an OTP application.
%%% TODO: figure out the Erlang way to stop an application.
%%% TODO: figure out the Erlang way to handle errors. 
%%% TODO: MVC: currently, a Controller module has to call a View module,
%%%       with certain Bindings, in order to render a Response to the client.
%%%         Compilation of the View code (of whatever MIME type), interpolated 
%%%       with Erlang script expressions, into a module, is currently 
%%%       complicated. This compilation of special modules for Views is not 
%%%       necessary - it's just being done under a "everything is a module" 
%%%       heuristic which is intended as a model (not Model) of simplicity.
%%%         An obvious alternative to be explored, is for Controllers to call a
%%%       generic "render the view" function, which interpolates the Erlang
%%%       and non-Erlang View code.
%%%
%%%-----------------------------------------------------------------------------
%%% start/0
%%% |
%%% |-scan/0 TODO: Module is being passed around as a string :( should be atom.
%%% | |-get_config/1
%%% | |-tag_files/4
%%% |
%%% |-compile_and_load_erl/1 
%%% |
%%% |-compile_and_load_view/1
%%% | |-view_bin_to_loaded_module/3
%%% |   |-view_bin_to_erl_bin_forms/2
%%% |   | |-xml_slice_contents/3
%%% |   | |-erl_exprs_to_abstract/1
%%% |   |-erl_bin_form_to_abstract/1
%%% |
%%% |-start_webserver/0
%%% | |-get_config/1
%%% |
%%% v
%%% time's arrow
%%%-----------------------------------------------------------------------------
-module(ezutils).
-export([start/0,stop/0]).

%%------------------------------------------------------------------------------
%% This file is, by default, at eze_app/ez/ezutils.erl
%% ezstart.bat is, by default, at eze_app/ezstart.bat
%% So, "./" would refer to "eze_app/"
%%------------------------------------------------------------------------------
get_config(Key)->
  Config = 
    [ { cwd,
        "./"  },
      { dev_subdir,
        "dev/"  },
      { ez_subdir,
        "ez/" },
      { httpd_config,
        [ { modules,
            [ mod_esi ]},
          { port,
            8000},
          { server_name,
            ""}, 
          { server_root,
            "./log/"},
          { document_root,
            "./webroot/"},
          { erl_script_alias,
            { "",
              [ez]  } } ] } ],
  proplists:get_value(Key,Config).

%%------------------------------------------------------------------------------
%% HAS SIDE EFFECTS!!  
%% TODO: starts the eze_application 
%% Perhaps this needs to get moved to its own Ezinit module. Cleaner.
%%------------------------------------------------------------------------------
start()->
  io:format("ezutils:start/0 was called\n"),
  Scanned = scan(), 
  lists:foreach(
    fun(E)-> compile_and_load_erl(E) end,
    proplists:get_value(erl,Scanned)  ),
  lists:foreach(
    fun(E)-> compile_and_load_view(E) end,
    proplists:get_value(views,Scanned)  ),
  start_webserver().

%%------------------------------------------------------------------------------
%% TODO: stops the eze_application
%% Make this more like the programming guidance.
%%------------------------------------------------------------------------------
stop()-> 
  io:format("ezutils:stop/0 called erlang:halt/0\n"),
  erlang:halt().

%%------------------------------------------------------------------------------
%% HAS SIDE EFFECTS!
%%
%% TODO: scan eze_application directories for files to be compiled
%% TODO: shouldn't module names all be atoms?
%% Returns: lists of tuples: {Module, Dir, File}
%%------------------------------------------------------------------------------
scan()->
  io:format("ezutils:scan/0 was called\n"),
  Cwd =       get_config(cwd),
  DevSubdir = get_config(dev_subdir),
  EzSubdir =  get_config(ez_subdir),

  DevFiles =  element(2, file:list_dir(Cwd++DevSubdir)),
  EzFiles =   element(2, file:list_dir(Cwd++EzSubdir)),
  % TODO: .html should be .view, as there might be other MIME types returned
  Views =       tag_files(DevFiles,DevSubdir,"^.+\.v.html$",".html$"),
  Controllers = tag_files(DevFiles,DevSubdir,"^.+\.c.erl$", ".erl$"),
  Models =      tag_files(DevFiles,DevSubdir,"^.+\.m.erl$", ".erl$"),
  Ez =          tag_files(EzFiles, EzSubdir, "^ez.*\.erl$", ".erl$"),
  [
    {controllers, Controllers},
    {models,      Models},
    {views,       Views},
    {ez,          Ez},
    {erl,         lists:flatten([Controllers, Models, Ez])} ].

%%------------------------------------------------------------------------------
%% 
%%------------------------------------------------------------------------------
tag_files(Filenames,Dir,FileTypeRe,RemoveThisRe)->
  lists:map(
    fun(Filename)->
      {re:replace(Filename,RemoveThisRe,"",[{return,list}]),Dir,Filename} end,
    lists:filter(
      fun(E)-> is_tuple(re:run(E,FileTypeRe)) end,
      Filenames  ) ).

%%------------------------------------------------------------------------------
%% TODO: check and warn, if module is already loaded.
%%------------------------------------------------------------------------------
compile_and_load_erl({_Module,Dir,Filename})->
  io:format("ezutils:compile_and_load_erl/2 was called on: \"~s~s\"\n",
    [Dir,Filename] ),
  case compile:file(Dir++Filename,[binary,report]) of
    error ->                  stop(); 
    {ok, ModuleName, Bin} ->  code:load_binary(ModuleName, Filename, Bin) end.

%%------------------------------------------------------------------------------
%% TODO: check and warn, if module is already loaded.
%%------------------------------------------------------------------------------
compile_and_load_view({Module,Dir,Filename})->
  io:format(  "ezutils:compile_and_load_view/2 was called on: "
    "\"~s~s\"\n", [Dir,Filename]  ),
  case file:read_file(Dir++Filename) of
    {error,Error} -> 
      io:format("ezutils:compile_and_load_view/2 called "
        "file:read_file/1, returned: ~p",[Error]  ),
      stop(); 
    {ok,Bin} -> 
      view_bin_to_loaded_module(Module,Filename,Bin) end.

%------------------------------------------------------------------------------
%%
%%------------------------------------------------------------------------------
view_bin_to_loaded_module(Module,Filename,Bin)->
  {ok,ModuleName,CompiledBin} 
    = compile:forms(
        lists:map(
          fun erl_bin_form_to_abstract/1, 
          view_bin_to_erl_bin_forms(Module, Bin)  )
      ),
  code:load_binary(ModuleName,Filename,CompiledBin).


%------------------------------------------------------------------------------
%%
%%------------------------------------------------------------------------------
view_bin_to_erl_bin_forms(Module,Bin)->
  [ list_to_binary([<<"-module('">>,Module,<<"').">>]), 
    <<"-export([interpolate/1]).">>, 
    list_to_binary(
      [ <<"\ninterpolate(Bindings)->\nSlicedXml = ">>,
        io_lib:print(
          xml_slice_contents(<<"erl">>,Bin,fun erl_exprs_to_abstract/1)  ),
        <<",
        Args = 
          [ fun({Type,Slice})-> 
              case Type of
                not_erl -> Slice;
                erl -> 
                  {value,Value,_NewBindings} 
                    = apply(erl_eval, exprs, [Slice,Bindings]),
                  Value end end, 
            SlicedXml ],
        apply(lists,map,Args).">> ] ) ].

%%------------------------------------------------------------------------------
%% Test bin for <erl> parsing in view
%% <<"jigger<erl>a</erl>something </erl>something<erl>we're back in erl</erl> more something something">>
%%
%% TODO: figure out if binary:split or re:split is faster
%% TODO: reduce nesting, as in xml_erl_to_module/1
%%------------------------------------------------------------------------------
xml_slice_contents(ElementName,Bin,PostProcessor) ->
  OpenTag =     <<"<",ElementName/binary,">">>,
  CloseTag =    <<"</",ElementName/binary,">">>,
  Element =     binary_to_atom(ElementName, utf8),
  NotElement =  binary_to_atom(<<"not_",ElementName/binary>>,utf8),
  % Point of style: is this too Lispy for Erlang? Should it be broken up?
  Fun = 
    fun
      (_ThisFun,[<<>>|T])-> lists:reverse(T);
      (ThisFun, [H|T])-> 
        (case re:split(H,OpenTag,[caseless,{parts,2}]) of
          [H] ->
            ThisFun(ThisFun, [<<>>, {NotElement,H}] ++ T);
          [BeforeOpenTag,AfterOpenTag] ->
            (case re:split(AfterOpenTag,CloseTag,[caseless,{parts,2}]) of
              [AfterOpenTag] ->
                io:format("\nMissing \"~w\" detected in: ~p\n",
                  [CloseTag,AfterOpenTag]  ),
                ThisFun(ThisFun, [<<>>, {NotElement,BeforeOpenTag}] ++ T);
              [InTag,AfterCloseTag] ->
                ThisFun(
                  ThisFun, 
                  [ AfterCloseTag,
                    {Element,     PostProcessor(InTag)},
                    {NotElement,  BeforeOpenTag}
                  ] ++ T ) end) end) end,
  Fun(Fun,[Bin]).

%%------------------------------------------------------------------------------
%% Purpose: ScriptBin -> erl_scan:string/1 -> erl_parse:parse_exprs/1 
%% Limitation: parse_exprs/1 can take at most one Abstract Form;
%%    "-module(hi)." is a form, but "-module(hi). x()->ok." is two forms.
%%
%% Error handling here is non-standard.
%%------------------------------------------------------------------------------
erl_exprs_to_abstract(Bin)->
  case erl_scan:string(binary_to_list(Bin)) of 
    {error,ErrorInfo,ErrorLocation} ->
      io:format( "ezlib:erl_exprs_to_abstract/1 called erl_scan:string/1, "
        "erred at ~p: ~p\n", [ErrorLocation,ErrorInfo] ),
      stop(); 
    {ok,Tokens,_EndLocation} ->
      (case erl_parse:parse_exprs(Tokens) of
        {error,ErrorInfo} -> 
          io:format( "ezlib:erl_exprs_to_abstract/1 called "
            "erl_parse:parse_exprs/1,  and erred: ~p\n", [ErrorInfo] ),
          stop(); 
        {ok,ExprList} ->
          ExprList end) end.
  
%%------------------------------------------------------------------------------
%% Purpose: ScriptBin -> erl_scan:string/1 -> erl_parse:parse_form/1 
%% Limitation: parse_form/1 can take at most one Abstract Form;
%%    "-module(hi)." is a form, but "-module(hi). x()->ok." is two forms.
%%
%% Error handling here is non-standard.
%%------------------------------------------------------------------------------
erl_bin_form_to_abstract(Bin)->
  case erl_scan:string(binary_to_list(Bin)) of 
    {error,ErrorInfo,ErrorLocation} ->
      io:format( "ezlib:erl_bin_form_to_abstract/1 called erl_scan:string/1, "
        "erred at ~p: ~p\n", [ErrorLocation,ErrorInfo] ),
      stop(); 
    {ok,Tokens,_EndLocation} ->
      (case erl_parse:parse_form(Tokens) of
        {error,ErrorInfo} -> 
          io:format( "ezlib:erl_bin_form_to_abstract/1 called "
            "erl_parse:parse_form/1, and erred: ~p\n", [ErrorInfo] ),
          stop(); 
        {ok,ExprList} ->
          ExprList end) end.
  
%%------------------------------------------------------------------------------
%% 
%%------------------------------------------------------------------------------
start_webserver()->
  HttpdConfig = get_config(httpd_config),
  io:format("ezutils:start_webserver/0 was called\n"),
  % TODO: this inets start/stop thing is crufty, fix it.
  begin inets:stop(),
      inets:start() end,
  case proplists:get_value(port, HttpdConfig) of 
    80 -> 
      io:format(
        "ezutils:start_webserver/0: Warning! On some systems, "
        "Port 80 can only be accesed by the root user.\n" ); 
    Port -> 
      io:format(
        "ezutils:start_webserver/0: Attempting to start Inets on Port ~p.\n",
        [Port] ) end,
  inets:start(httpd,HttpdConfig).
