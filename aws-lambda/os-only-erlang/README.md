# working

```
# Amazon Linux 2023 is based on (el9 / Red Hat Enterprise Linux 9)
curl -L -o otp.rpm https://github.com/rabbitmq/erlang-rpm/releases/download/v27.3.3/erlang-27.3.3-1.el9.aarch64.rpm

sudo dnf install -y otp.rpm
vi main.erl

zip -r otp.zip /usr/lib64/erlang/
erlc mainb.erl 
# download  otp.zip         -> /var/task/erlang
#   and     mainb.beam      -> /var/task/mainb.beam
#   via scp, then upload to -> lambda

# on lambda : /var/task/bootstrap :
#
#   #!/bin/bash
#   set -euo pipefail
#   /var/task/erlang/bin/erl -noshell -s mainb loop -s init stop
 

```

# not working

From [here](https://github.com/erlang/otp/blob/OTP-27.3.3/HOWTO/INSTALL.md)

```
sudo dnf install git g++ ncurses-devel openssl-devel perl
# gcc will do, but g++ is needed for JIT

git clone https://github.com/erlang/otp.git --branch=OTP-27.3.3 --single-branch --depth=1
cd otp
git checkout OTP-27.3.3

export ERL_TOP=`pwd` 
export LANG=C
./configure
make -j$(nproc)
make -j4 release_tests
cd release/tests/test_server

$ERL_TOP/bin/erl -s ts install -s ts smoke_test batch -s init stop
make install
# may have to ln -s /usr/local/bin/erl /usr/bin/erl

cd $ERL_TOP
vi main.erl

erlc mainb.erl && erl -noshell -s mainb loop -s init stop
# probably, put this in (/var/task/bootstrap) as a #!/bin/bash
```
