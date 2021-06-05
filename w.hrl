%   Figure out how to make each response
%   start in a new process. spawn/4?
%
web_server(Modules) when is_list(Modules)->
 inets:stop(),
 inets:start(),
 inets:start(httpd, [
  {modules, [mod_esi]},
  {port,8000},
  {server_name,""}, % given example is "localhost"
  {server_root,"trivial_server_root"},
  {document_root,"trivial_document_root"},
  {erl_script_alias, {"", Modules}}
 ])
 .
 
hello(SessionID, _Env, _Input) ->
 mod_esi:deliver(SessionID, [
  "Content-Type: text/html\r\n\r\n", 
  "<html><body>Hello, World!</body></html>"
 ]).
