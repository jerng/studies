1.  Make a directory, say `/release_working_directory`. While using the `erl` shell/REPL, you
can also run `cd(".").` to check the current working directory.

2.  Create the following file and directory structure :
```
release_working_directory
|
|-- my_release-r-version-1.rel
|
|-- ebin
|   |-- my_app.app
|
|-- src
    |-- my_app.erl
```
Sources :
```
% my_app.erl
-module(my_app).
-behaviour(application).
-export([start/2, stop/1]).
start(_Type,_Args) -> 
io:fwrite("hello\n"),
{ok,self()}.
stop(_State)->
ok.

% my_release-r-version-1.rel
{   release, 
    {"my_release", "r-version-1"}, 
    {erts, "15.2.6"},
    [   
    {kernel, "10.2.6"}, 
        {stdlib, "6.2.2"}, 
        {my_app, "a-version-1"}
    ]
}.

% my_app.app
{   application, 
    my_app,
    [
        {description, "Hello world application"},
        {vsn, "a-version-1"},
        {modules, [my_app]},
        {applications, [kernel, stdlib]},
        {mod, {my_app, []}},
        {registered, []},
        {env, []}
    ]
}.
```

3. Compile `.beam`, which may be found in `release_working_directory`, and move it to `ebin`.

From the `sh` shell :
```
erlc src/my_app.erl
```

The tree should now look like this :
```
release_working_directory
|
|-- my_release-r-version-1.rel
|
|-- ebin
|   |-- my_app.app
|   |-- my_app.beam
|
|-- src
    |-- my_app.erl
```

4. Generate `.beam`, `.script`, and release package as `tar.gz`.

From the `erl` shell :
```
systools:make_script(   "my_release-r-version-1",[    {path,["./ebin"]}    ]).
systools:make_tar(      "my_release-r-version-1",[    {path,["./ebin"]}    ]).
```

This should put files in `/release_working_directory`, which should now look
like this :
```
release_working_directory
|
|-- my_release-r-version-1.rel
|-- my_release-r-version-1.script
|-- my_release-r-version-1.boot
|-- my_release-r-version-1.tar.gz
|
|-- ebin
|   |-- my_app.app
|   |-- my_app.beam
|
|-- src
    |-- my_app.erl
```

5. Extract the release package.

From the `sh` shell :
```
tar -xf my_release-r-version-1.tar.gz -C /deployment_working_directory
```
