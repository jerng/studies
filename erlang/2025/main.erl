-module(main).
-export([hello/0]).

hello() -> io:fwrite("hello\n").
