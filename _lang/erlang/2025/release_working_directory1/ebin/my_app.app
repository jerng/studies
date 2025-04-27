{   application, 
    my_app,
    [
        {description, "Hello world application"},
        {vsn, "a-version-1"},
        {modules, [my_mod]},
        {applications, [kernel, stdlib,sasl]},
        {mod, {my_mod, []}},
        {registered, []},
        {env, []}

        ]}.
