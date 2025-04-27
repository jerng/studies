# Tidier Index/Introduction to Erlang/OTP

Hopefully, this is easier to follow than existing documentation. 
( As of 2025-04 : I found the latter to be rather non-linear. )

**WARNING** : This is not intended to be a comprehensive introduction - and you should read
all other existing documentation thoroughly, before putting any systems into
production.

###### Two minimal examples, of an Erlang Application release

>   - [blog post with code](https://www.n16f.net/blog/building-erlang-applications-the-hard-way/) ( 2023 )
>   - [sample repository with
>   instructions](https://github.com/boardwalk/erltest/tree/master) ( 2017 )

###### Alternative introductions

>   - [The Beam Book](https://blog.stenmans.org/theBeamBook/) ( 2025 )
>   - [Learn You Some Erlang](https://learnyousomeerlang.com/) ( 2013 )

## Ontology / Mereology

### Infrastructure / Hardware

top-down view : from distributed system to function

`distributed system` > `runtime system` > `virtual machine` > `process`

>   A `distributed Erlang system` consists of many `Erlang runtime systems` in
>   communications with each other.
>   [link](https://www.erlang.org/doc/system/distributed.html)
>   
>   An `Erlang runtime system` a.k.a. `node` is an operating system process, which
>   executes :
>   -   an `Erlang compiler`
>   -   an `Erlang virtual machine`
>       -   most commonly the `BEAM VM` ... the [reference
>           implementation](https://blog.stenmans.org/theBeamBook/#_beam_it_is_virtually_unreal)
>           for other Erlang VMs
>   -   N `schedulers` for `Erlang processes`
>   - and other infrastructural concerns, between the operating system layer and the OTP
>     protocol layer
>   
>   An `Erlang process` is the execution state of an `Erlang function`, on an `Erlang
>   virtual machine`.

### Language Specification

[link](https://www.erlang.org/doc/system/reference_manual.html)

### Business Logic / Software

bottom-up view : from function to release

`function` < `module` < `-behaviour(application)` < `release` < `target system`

>   `Erlang functions` are grouped into, and exported by `Erlang
>   modules`. [link](https://www.erlang.org/doc/system/modules.html)
>   
>   `Erlang modules` may belong to `Erlang behaviours`.
>       [link](https://www.erlang.org/doc/system/design_principles.html#behaviours)
>   
>   -   `Erlang modules` are also encapsulations of code for replacement in live
>           `Erlang runtime system`s.
>           [link](https://www.erlang.org/doc/system/code_loading#code-replacement)
>   
>   `Erlang behaviours` include the `OTP application behaviour`.
>   [link](https://www.erlang.org/doc/apps/kernel/application)
>   -   the `OTP application behaviour` describes an `Erlang module`
>       containing `Erlang functions` used to `start/` and `stop/` OTP`
>       applications. [link](https://www.erlang.org/doc/system/applications.html)
>       -   **core** `OTP applications` are : `erts`, `kernel`, `stdlib`, `sasl`
>       -   most **non-core** `OTP applications` : may be hot-swapped/soft-updated in a
>           ERTS without restarting the
>           ERTS. [link](https://www.erlang.org/doc/system/upgrade.html) /
>           [cookbook](https://www.erlang.org/doc/system/appup_cookbook.html)
>   
>   `OTP applications` are grouped under `OTP
>       releases`. [link](https://www.erlang.org/doc/system/release_structure.html)
>   
>   `OTP releases` are supported by the `sasl` application.
>   [link](https://www.erlang.org/doc/system/release_handling.html)
>   -   the `systools` module : supports offline building of releases
>       -   the `release_handler` module : supports online unpacking and installing
>           of releases
>   -   the `relup` file format : defines overall release upgrades.
>       [link](https://www.erlang.org/doc/apps/sasl/relup.html)
>       -   the `appup` file format : defines component application upgrades.
>           [link](https://www.erlang.org/doc/apps/sasl/appup.html) /
>           [cookbook](https://www.erlang.org/doc/system/appup_cookbook.html)
>   -   `OTP releases` are operations upon `OTP target systems`
>       -   creating initial `OTP target system`s :
>           [link](https://www.erlang.org/doc/system/create_target.html#creating-a-target-system)
>       -   creating subsequent `OTP target systems`s :
>           [link](https://www.erlang.org/doc/system/create_target.html#creating-the-next-version)

---
---

# Old notes / to be edited or deleted : 

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
