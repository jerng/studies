**Warning** : This minimal example makes the following assumptions,

>   -   some version of `Erlang/OTP` is already installed on your system
>   -   version numbers in `my_release.rel` and `ebin/my_app.app` have been
>       hard-coded, BUT need to match your system
>   -   `build steps` and `install steps` output files in specific
        places : therefore other steps move files from those places

... therefore if `build-install-run.sh` fails, please check the assumptions.

---

# Minimal Example

Running `build-install-run.sh`,

1.  places 
    -   `my_release.rel`
    -   `ebin/my_app.app`
    -   `src/my_mod.erl`
2.  compiles and replaces
    -   `src/my_mod.erl` -> `ebin/my_mod.beam`
3.  compiles 
    -   `my_release.rel` -> `my_release.script` -> `my_release.boot`
    -   ( various ) -> `my_release.tar.gz`
        -   **OPTIONAL INTERVENTION** : you may edit `_build-steps-2-3.erl` to
            uncomment out the `{erts,"dir"}` term, and modify it, before executing
            this script, if you want to include `erts` binaries in the release
4.  extracts
    -   `my_release.tar.gz` -> `deployment_working_directory`
5.  runs
    -   `erl` appropriately upon the contents of `deployment_working_directory`
        -   the given example runs `/usr/bin/env erl`, but if you have alread
            added `erts` binaries to your deployment, and wish to test those,
            then you should execute the `erl` that is in your deployment instead 

... which should print "hello", and exit.

## SO Minimal

1.  This does not set up a proper `OTP application`, which would require separate
    `modules` with `-behaviour(application)`, `-behaviour(supervisor)`,
    `-behaviour(gen_server)`.
2.  `erl` defaults to `erl -mode interactive` : you can perform the necessary
    configuration changes ( if any ) to try 
    -   `erl -mode minimal` or
    -   `erl -mode embedded`
