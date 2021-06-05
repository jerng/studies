#!/bin/bash
erl -sname ez -setcookie ez_default_cookie -env ERL_CRASH_DUMP_SECONDS 1 -eval "case compile:file(\"./lib/ez.erl\",[binary,report]) of error -> io:format(\"\nezstart.bat called erlang:halt/0\n\"), halt(); {ok, ModuleName, Bin} -> code:load_binary(ez, \"ez.erl\", Bin), ez:start() end."
