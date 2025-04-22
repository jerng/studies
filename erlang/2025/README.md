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
```
