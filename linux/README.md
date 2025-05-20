# Filesystem Hierarchy

Various

```
/       :   the 'root directory'
|
+- bin  :   binaries, essential (single-user mode), bootstrapping and
|           repair ( e.g. 'cat', 'ls', 'cp' )
|
+- boot :   boot loading (e.g. 'kernels', 'initrd' / initial ramdisk,
|           'initramfs' / initial RAM file systemi, more modern,
Debian preferred )
```

- **POSIX.1-2001** introduced [`pax` / portable exchange
  archive](https://en.wikipedia.org/wiki/Pax_(command)) to avoid
  incompatibilities between the [`cpio` / copy in and
  out](https://en.wikipedia.org/wiki/Cpio) and [`tar` / tape
  archiver](https://en.wikipedia.org/wiki/Tar_(computing)) ( history :
  `tar` < `tp` < `tap` ) utilities. `pax` supports the file formats of
  `cpio`, `pax`, and  [`UStar` / Unix Standard Tar
  format](https://en.wikipedia.org/wiki/Tar_(computing)#UStar_format)
  from **POSIX.1-1988**.

# Boot Sequence

Ubuntu

>   Summarised from :
>   - [link](https://wiki.ubuntu.com/Booting)
>   -
>   [link](https://medium.com/@fouadpro2002/system-v-upstart-and-systemd-689574a94e73)

1.  `BIOS` / basic input output system or `UEFI` / unified extensible
    firmware interface, performs a `POST` / power-on self-test to check
    hardware integrity
    -   firmware is executed from `ROM` / read-only memory, on the
        motherboard.
    -   hardware is initialised
    >   code is obtained for the next step ( `boot loader` )

2.  `Boot loader` : there are multiple alternatives A.  stored on an
early sector of a hard drive
        -   [`MBR` / master boot
            record](https://en.wikipedia.org/wiki/Master_boot_record)
            for IBM-compatible PCs, typically on sector 0
        -   [`GPT` / GUID Partition
            Table](https://en.wikipedia.org/wiki/GUID_Partition_Table)
            is a modern alternative, typically on sector 1, with a
            protective MBR shimmed upon sector 0 E.g. `GRUB`, `LILO`,
            `yaboot`, etc.
        >   [`GRUB` / grand unified
        >   bootloader](https://en.wikipedia.org/wiki/GNU_GRUB) is bulky
        >   with many
        >   features; storage is split up :
        >   1.  stage 1 is in the `MBR`
        >       -   location of `/boot/grub/menu.lst`
        >       -   files for stages 1.5 and 2
        >   2.  stage 1.5 is on the first cylinder of the disk
        >   3.  stage 2 is on the disk ( CHECK )
    B.  stored on other storage, e.g. CDR, USB, etc.
    C.  stored on a ROM, on the networking card / hardware : e.g. via
        `PXE` / pre-execution environment
    >   code is obtained for the next step ( `kernel` and initial ram
    >   disk filesystem )

3.  Kernel
    -   a small filesystem is loaded into RAM : `initrd` / initial
        ramdisk, or `initramfs` / initial RAM filesystem
    -   initialisation script is launched in that filesystem : this is
        the core code of the operating system, enabling hardware
        etc.
    -   the root storage partition, is found ( possibly on a different
        physical host ), and typically mounted with `mount`

4.  `init` ( system tasks & essential services ) 
    >   Configured at `/etc/init`
    -   various scripts of the form `/etc/*rc` are run
        -   `*rc` standads for runcom / run commands, since Bell Labs
            System V Unix
    1.  Plymouth : graphical boot animation & logger
    2.  mountall : mounts all filesystems defined on `/etc/fstab`
    3.  network : 
    4.  display manager : `GDM`, `KDM`, `XDM`, ...
    -   Through iterations, [System V / SysV / System
        Five](https://en.wikipedia.org/wiki/Init#SYSV) was
        developed by AT&T in 1983.
        [Runlevels](https://en.wikipedia.org/wiki/Runlevel) were defined
        as 
        0.  halt : everything is shut down, processes stop
        1.  single-user mode : troubleshooting & recovery
        2.  multi-user mode, without networking : ditto
        3.  text mode (multi-user mode, with networking) : default state
            for servers on a network
        4.  unused
        5.  graphical mode : default state for PCs
        6.  reboot
        >   -   Consequently, `initdefault` should not be set to 0 or 6
        >   -   `telinit` is the command to change run levels
    -   [UpStart](https://en.wikipedia.org/wiki/Upstart_(software\))
        replaced traditional `init.d`-style scripts under `System-V`,
        providing better management of race-conditions, via an
        event-monitoring system, developed by Canonical and used from
        2006 to 2014.
    -   [systemd](https://en.wikipedia.org/wiki/Systemd) has become a
        dominant `init` system since 2015, and gained marketshare via
        reliable parallelism, and centralised management of processes,
        daemons, and services, and mount points. Developed by Red Hat
        since 2010.
        -   Further reading on [ancilliary
            components](https://en.wikipedia.org/wiki/Systemd#Ancillary_components)
            is encouraged : `journald`, `libudev`, `localed`, `logind`,
            `hostnamed`, `homed`, `networkd`, `resolved`,
            `systemd-boot`, `systemd-bsod`, `systemd-nspawn`,
            `timedated`, `timesyncd`, `tmpfiles`, `udevd`, etc.
        -   Configuration files a.k.a. `unit files` use a declarative
            language inspired by [.ini
            files](https://en.wikipedia.org/wiki/INI_file) : `.service`,
            `.socket`, `.device`, `.mount`, `.automount`, `.swap`,
            `.target`, `.path`, `.timer`, `.snapshot`, `.slice`,
            `.scope`, etc.

# Kernel

#### Networking

-   [`TUN`/`TAP`](https://en.wikipedia.org/wiki/TUN/TAP) refers to
    **virtual** devices hosted by the kernel, which allow client
    programs to engage with the kernel via familiar **interfaces**.
    -   `TUN`nel : simulates a [ network / layer 3
        ](https://en.wikipedia.org/wiki/OSI_model#Layer_3:_Network_layer)
        device, carrying [IP packets](https://en.wikipedia.org/wiki/Internet_Protocol)
    -   network `TAP` : simulates a [ data link / layer
        2](https://en.wikipedia.org/wiki/OSI_model#Layer_2:_Data_link_layer)
        device, carrying [ethernet
        frames](https://en.wikipedia.org/wiki/Ethernet)


# Shells

Unixes

-   originally, [`sh` / Bourne
    Shell](https://en.wikipedia.org/wiki/Bourne_shell), 1976,
    alternatively,
    -   gradually replaced in BSD by [`ash` / A shell, Almquist
        shell](https://en.wikipedia.org/wiki/Almquist_shell), 1989
        -   ported to Debian as [`dash` / Debian Almquist
            shell](https://en.wikipedia.org/wiki/Almquist_shell#Dash),
            1997, which is linked to from `sh` on most current systems
-   [`csh` / C shell](https://en.wikipedia.org/wiki/C_shell), by Bill
    Joy, 1978
    -   [ `tcsh` / TENEX C shell](https://en.wikipedia.org/wiki/Tcsh),
        1975, a more modern implementation, and what is actually linked
        by `csh` in many modern systems
-   [`ksh` / KornShell](https://en.wikipedia.org/wiki/KornShell), 1983
-   [`bash` / Bourne Again
    SHell](https://en.wikipedia.org/wiki/Bash_(Unix_shell\)), 1989, most
    dominant in 2025
-   [`zsh` / zhongshao shell / Z
    shell](https://en.wikipedia.org/wiki/Z_shell), 1990, inspired by
    `tcsh` and `ksh` : macOS switched to this, from `bash` in macOS
    Catalina, 2019
