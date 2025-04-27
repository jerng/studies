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
from [here](https://erlangforums.com/t/docker-image-for-erlang-vm-with-minimal-memory-disk-usage/3610/2)
getting the ERTS startup from 30+MB to 10+MB :
```
erl +S 1 +SDio 1 +P 1024 +Q 1024 -mode minimal
erl +Mea min +swct very_lazy

+swct garbage collection wakeup frequency
+S Schedulers:SchedulerOnline, max 1024:1024, default 1:1
+SDio DirtyIOSchedulers, 1-1024, default 10
+P MaxSimultaneouslyExistingProcesses, 1024-134,217,727
+Q MaxSimultaneouslyExistingPorts, 1024-134,217,727
```
