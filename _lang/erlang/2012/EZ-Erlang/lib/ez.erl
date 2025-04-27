%%%-----------------------------------------------------------------------------
%%% TODO: implement -on_load().
%%% TODO: figure out the Erlang way to write an OTP application.
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
%%% | |-compile_and_load_view_bin/3
%%% |   |-view_bin_to_bin_forms/2
%%% |   | |-slice_xml_by_tag_name/3
%%% |   |   |-bin_exprs_to_af_exprs/1
%%% |   |-bin_forms_to_af_forms/1
%%% |
%%% |-start_webserver/0
%%% | |-get_config/1
%%% |
%%% v
%%% time's arrow
%%%
%%% <<"jigger<erl>a</erl>something </erl>something<erl>we're back in erl</erl> more something something">>
%%%-----------------------------------------------------------------------------
-module(ez).
-export([start/0,render/2]).

%%------------------------------------------------------------------------------
%% TODO: validate config at startup?
%%
%% This file is, by default, at eze_app/ez/ezutils.erl
%% ezstart.bat is, by default, at eze_app/ezstart.bat
%% So, "./" would refer to "eze_app/"
%%------------------------------------------------------------------------------
get_config(Key)->
  Config = 
  [ { security_level,     low },
    { debug_mode,         development },
    { build_mode,         production },
    { cwd,                "./"  },
    { framework_subdir,   "lib/" },
    { model_subdir,       "dev/models/"  },
    { view_subdir,        "dev/views/"  },
    { controller_subdir,  "dev/controllers/"  },
    { httpd_config,       [ { modules,[mod_esi,mod_alias,mod_get ] },
        { port,             3000},
        { server_name,      ""}, 
        { server_root,      "./log/"},
        { document_root,    "./webroot/"},
        { directory_index,  [ "index.html" ] },
        { erl_script_alias, { "/ez",[default,test]  } } 
      ] 
    } 
  ],
  proplists:get_value(Key,Config).

%%------------------------------------------------------------------------------
%% HAS SIDE EFFECTS!!  
%%------------------------------------------------------------------------------
start()->
  io:format(
    "\n--------------------------------------------------------------------"
    "----------\n\n" ?MODULE_STRING ":start/0 was called.\n"  ),
  Scanned = scan(), 
  io:format("  Now calling " ?MODULE_STRING ":compile_and_load_erl/2 on:\n"),

  lists:foreach(
    fun compile_and_load_erl/1,
    proplists:get_value(erl,Scanned)
  ),
  io:format("  Now calling " ?MODULE_STRING ":compile_and_load_view/2 on:\n"),
  lists:foreach(
    fun compile_and_load_view/1,
    proplists:get_value(views,Scanned)
  ),

  start_webserver().

%%------------------------------------------------------------------------------
%% HAS SIDE EFFECTS!
%%
%% TODO: shouldn't module names all be atoms? or is this really just a filename?
%% Returns: lists of tuples: {Module, Dir, File}
%%------------------------------------------------------------------------------
scan()->
  io:format(?MODULE_STRING ":scan/0 was called.\n"),
  Cwd =       get_config(cwd),

  FrameworkSubdir =  get_config(framework_subdir),
  ModelSubdir =      get_config(model_subdir),
  ViewSubdir =       get_config(view_subdir),
  ControllerSubdir = get_config(controller_subdir),

  EzFiles =          element(2, file:list_dir(Cwd++FrameworkSubdir)),
  ModelFiles =       element(2, file:list_dir(Cwd++ModelSubdir)),
  ViewFiles =        element(2, file:list_dir(Cwd++ViewSubdir)),
  ControllerFiles =  element(2, file:list_dir(Cwd++ControllerSubdir)),

  Ez 
  = tag_files(EzFiles,FrameworkSubdir,"^ez.*\.erl$",".erl$"),
  Models 
  = tag_files(ModelFiles,ModelSubdir,"^.+\_model.erl$",".erl$"),
  Views 
  = tag_files(ViewFiles,ViewSubdir,"^.+\.view$", "$"),
  Controllers 
  = tag_files(ControllerFiles,ControllerSubdir,"^.+\.erl$",".erl$"),

  [ {ez,          Ez},
    {controllers, Controllers},
    {models,      Models},
    {views,       Views},
    {erl,         lists:flatten([Ez,Controllers,Models])} 
  ].

%%------------------------------------------------------------------------------
%% 
%%------------------------------------------------------------------------------
tag_files(Names,Dir,TakeRe,RemoveRe)->
  Taken = lists:filter(fun(E)-> is_tuple(re:run(E,TakeRe)) end,Names),
  lists:map(
    fun (Name) -> {re:replace(Name,RemoveRe,"",[{return,list}]),Dir,Name} end,
    Taken
  ).

%%------------------------------------------------------------------------------
%% TODO: check and warn, if module is already loaded.
%%------------------------------------------------------------------------------
compile_and_load_erl({_Module,Dir,Filename})->
  io:format("\t\"~s~s\"\n",[Dir,Filename]),
  case compile:file(Dir++Filename,[binary,report]) of
    error               -> erlang:halt(erl_compilation_error); 
    {ok,ModuleName,Bin} -> code:load_binary(ModuleName,Filename,Bin) 
  end.

%%------------------------------------------------------------------------------
%% TODO: check and warn, if module is already loaded.
%%------------------------------------------------------------------------------
compile_and_load_view({Module,Dir,Filename})->
  io:format("\t\"~s~s\"\n",[Dir,Filename]),
  case file:read_file(Dir++Filename) of
    {error,Error} -> erlang:halt({could_not_read_view,Error}); 
    {ok,Bin}      -> compile_and_load_view_bin(Module,Filename,Bin) 
  end.

%------------------------------------------------------------------------------
%% 
%%------------------------------------------------------------------------------
compile_and_load_view_bin(Module,Filename,Bin)->
  {ok,ModuleName,CompiledBin} 
  = compile:forms(
    lists:map(
      fun bin_forms_to_af_forms/1, 
      view_bin_to_bin_forms(Module, Bin)  
    ) 
  ),
  code:load_binary(ModuleName,Filename,CompiledBin).

%------------------------------------------------------------------------------
%%
%%------------------------------------------------------------------------------
view_bin_to_bin_forms(Module,Bin)->
  Form1 = list_to_binary([<<"-module('">>,Module,<<"').">>]), 
  Form2 = <<"-export([splice/1]).">>, 
  Form3 
  = list_to_binary(
      [ <<"
        splice(Bindings)->
        SlicedXml = ">>,
        io_lib:print(
          slice_xml_by_tag_name(<<"erl">>,Bin,fun bin_exprs_to_af_exprs/1)
        ),
        % TODO: NewBindings should pass the subsequent slices
        % TODO: DEBUG: Binaries in Views are turned into Strings in Slices
        % TODO: DEBUG: if module:function() syntax is used,
        % it is transformed as ?MODULEmodule:function/arity;
        % meanwhile, erlang:apply() must be used instead.
        <<",
        Args = 
        [ fun({Type,Slice})-> 
            case Type of
              not_erl -> 
                Slice;
              erl -> 
                
                % debug like this:
                %apply(io,format,[\"~p\",[{slice,Slice}]]),
                %apply(io,format,[\"~p\",[{bindings,Bindings}]]),

                {value,Value,_NewBindings} 
                  = apply(erl_eval, exprs, [Slice,Bindings]),
                Value 
            end 
          end, 
          SlicedXml ],
        apply(lists,map,Args).">> 
      ] 
    ),
  [Form1,Form2,Form3].

%%------------------------------------------------------------------------------
%%
%%------------------------------------------------------------------------------
slice_xml_by_tag_name(ElementName,Bin,PostProcessor) ->
  OpenTag =     <<"<",ElementName/binary,">">>,
  CloseTag =    <<"</",ElementName/binary,">">>,
  Element =     binary_to_atom(ElementName, utf8),
  NotElement =  binary_to_atom(<<"not_",ElementName/binary>>,utf8),
  Fun 
  = fun
    (_ThisFun,[<<>>|T])-> 
      % stop recursion; exit the loop
      lists:reverse(T);
    (ThisFun, [H|T])-> 
      % initial branch; search for open-tag
      case re:split(H,OpenTag,[caseless,{parts,2}]) of
        [H] ->
          % no open-tag found; head to exit
          ThisFun(ThisFun, [ <<>> | [{NotElement,H}|T] ]);
        [BeforeOpenTag,AfterOpenTag] ->
          % found an open-tag; search for close-tag
          case re:split(AfterOpenTag,CloseTag,[caseless,{parts,2}]) of
            [AfterOpenTag] ->
              % no matching close-tag found
              erlang:halt({missing_close_tag,CloseTag,AfterOpenTag});
            [InTag,AfterCloseTag] ->
              % found the matching close-tag; recurse to initial branch
              ThisFun(
                ThisFun, 
                [ AfterCloseTag
                  | [ {Element,PostProcessor(InTag)}
                      | [ {NotElement,BeforeOpenTag}
                          | T 
                        ]
                    ]
                ]
              ) 
          end 
      end 
  end,
  Fun(Fun,[Bin]).

%%------------------------------------------------------------------------------
%%
%%------------------------------------------------------------------------------
bin_exprs_to_af_exprs(Bin)->
  case erl_scan:string(binary_to_list(Bin)) of 
    {error,ErrorInfo,ErrorLocation} ->
      erlang:halt({ErrorLocation,ErrorInfo}); 
    {ok,Tokens,_EndLocation} ->
      case erl_parse:parse_exprs(Tokens) of
        {error,ErrorInfo} -> erlang:halt(ErrorInfo); 
        {ok,ExprList}     -> 
          
          % If you really want to, you can bind a model in the controller
          % and pass it to a view. Otherwise, EZ won't allow it.
          %
          % To detect controllers in views here, we would need a list of
          % controller names, and that's a bit messy (since it would have
          % to be re-generated, or passed down from ?MODULE:scan/0, or 
          % stored by ?MODULE:scan/0 and then referenced by this function
          % in/from the process dictionary). We'll just leave it as is for now. 
          % Calling a controller from the view will just crash the response,
          % or hang it if the view is calling the controller:action that 
          % called it.

          CheckForModels = fun(M) ->
            case re:run(atom_to_list(M),"_model$") of
              nomatch -> ok;
              _       -> erlang:halt({called_from_view,M})
            end
          end,

          lists:foreach(fun(Expr)->
            %io:format("\n~p\n",[Expr]),
            case Expr of

              % handles m:f() calls
              { call,
                _Line,
                {remote,_Line1,{atom,_Line2,M},_F},
                _Args
              } ->
                CheckForModels(M);
              
              % handles apply(m,f,args) calls
              { call,
                _Line,
                {atom,_Line1,apply},
                [{atom,_Line2,M}|_Args]
              } ->
                CheckForModels(M);
              
              % handles F = fun m:f/a assignment
              { match,
                _Line,
                _LHS,
                {'fun',_Line1,{function,{atom,_Line2,M},_F,_A}}
              } ->
                CheckForModels(M);

              _ ->
                ok
            end
          end,ExprList),

          ExprList 
      end 
  end.

%%------------------------------------------------------------------------------
%%
%%------------------------------------------------------------------------------
bin_forms_to_af_forms(Bin)->
  case erl_scan:string(binary_to_list(Bin)) of 
    {error,ErrorInfo,ErrorLocation} ->
      erlang:halt({ErrorLocation,ErrorInfo}); 
    {ok,Tokens,_EndLocation} ->
      case erl_parse:parse_form(Tokens) of
          {error,ErrorInfo} -> erlang:halt(ErrorInfo); 
          {ok,ExprList}     -> ExprList 
      end 
  end.

%%------------------------------------------------------------------------------
%% 
%%------------------------------------------------------------------------------
start_webserver()->
  HttpdConfig = get_config(httpd_config),
  io:format(?MODULE_STRING ":start_webserver/0 was called.\n"),
  % TODO: this inets start/stop thing is crufty, fix it.
  begin inets:stop(), inets:start() end,
  io:format("\tAttempting port ~p.\n",[proplists:get_value(port, HttpdConfig)]),
  inets:start(httpd,HttpdConfig).

%%------------------------------------------------------------------------------
%% 
%%------------------------------------------------------------------------------
render(Session,Response)->
  mod_esi:deliver( 
    Session, 
    [ [ lists:map(  
          fun({Name,Value})-> [Name,": ",Value,"\r\n"] end, 
          proplists:get_value(headers,Response) ), "\r\n"  
      ],
      apply(  
        proplists:get_value(view,Response),
        splice,
        [ proplists:get_value(bindings,Response) ] 
      ) 
    ] 
  ).
