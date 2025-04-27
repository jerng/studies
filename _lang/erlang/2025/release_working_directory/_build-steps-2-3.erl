#!/usr/bin/env escript

main(_)-> 
    systools:make_script("my_release", [
            { path, ["ebin"] }
    ]),
    systools:make_tar("my_release", [
            { path, ["ebin"] }

            % if you want to add the erts binaries to the release, you can
            % comment out the next line, and replace "dir" with the appropriate
            % directory string for your system : 
            %   if      "/path-to/erlang/erts-version/bin"
            %   then    {erts,"/path-to/erlang"} should be sufficient
            %
            % , { erts, "/usr/local/lib/erlang" }
    ]).
