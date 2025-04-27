#!/bin/bash

# build step 1
#   compiles src/my_mod.erl, to ebin/my_mod.beam
erlc -o ebin src/my_mod.erl

# build steps 2,3
#   compiles my_release.rel, to my_release.script, to my_release.boot
#   compiles (various) to my_release.tar.gz 
escript _build-steps-2-3.erl

# install step 1
mkdir deployment_working_directory

# install step 2
tar -xf my_release.tar.gz -C deployment_working_directory

# run
#   - ERL_LIBS must be the path to : the parent directory of "lib"
#   - "-boot"'s argument must be the path to : "start.boot", without ".boot"
ERL_LIBS=deployment_working_directory/lib/my_app-a-version-1 \
erl -noshell \
-boot deployment_working_directory/releases/r-version-1/start \
-s init stop
