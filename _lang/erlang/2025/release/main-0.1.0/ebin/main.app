% see : https://www.erlang.org/doc/system/applications.html
{
    application,    % keyword
        main,       % this app

        [
        { description,"proof of concept" },
        { vsn,"0.1.0" },
        { mod,
            {   
                main, [ ]
                % callback module, 
                %   main:start(normal,[]) is called to start
                %   main:stop([]) is called to stop
                %
                % arguments
            }
        },
        { modules, [ main ] },
                % modules INTRODUCED by this app

        { registered, [ ] },
                % names of PROCESSES registered by this application

        { applications, [ kernel, stdlib ] }
                % applications to be started BEFORE this app
    ]
}
.
