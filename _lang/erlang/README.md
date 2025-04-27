###### Welcome to Erlang/OTP studies
>    Featured in this repository ...
>    -    `erlang/README.md` (this file) provides **installation hints** and a **hello world**
>         example
>    -    `erlang/2025/README.md` attempts to be a **linear guide** through the jungle of
>         Erlang/OTP documents
>    -    `erlang/2025/release_working_directory` is a **minimal working example** of an
>         **application packaging and deployment**


# Installation

from [here](https://github.com/erlang/otp/blob/OTP-27.3.3/HOWTO/INSTALL.md)
```
git clone https://github.com/erlang/otp.git --branch=OTP-27.3.3 --single-branch --depth=1
cd otp

git checkout OTP-27.3.3
sudo apt install libncurses-dev libssl-dev
export ERL_TOP=`pwd` 
export LANG=C
./configure
make -j4
make -j4 release_tests
cd release/tests/test_server

$ERL_TOP/bin/erl -s ts install -s ts smoke_test batch -s init stop
make install
# may have to ln -s /usr/local/bin/erl /usr/bin/erl

vi main.erl

    -module(main).
    -export([hello/0]).

    hello() -> io:fwrite("hello\n").

erlc main.erl
# produced main.beam

erl -noshell -s main hello -s init stop
# no shell, run module function, stop ERTS

```
