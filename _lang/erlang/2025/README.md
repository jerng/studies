# Tidier Introduction/Index to Erlang/OTP

**WARNING** : This is not intended to be a comprehensive introduction - and you should read
all other existing documentation thoroughly, before putting any systems into
production.

Hopefully, this is easier to follow than existing documentation, as of 2025-04. 

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
>   -   multiple `Erlang runtime systems` can exist on the same `host` because
>   ...
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

bottom-up view : from function to target system

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
>       containing `Erlang functions` used to `start/` and `stop/` OTP
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

