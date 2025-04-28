###### Install GNU Fortran
On Ubuntu :
```
sudo apt install gfortran gcc-multilib
```

###### Install Fortan Package Manager from Sources
[https://github.com/fortran-lang/fpm]
-       Do NOT use `snap install` : outdated, buggy, and opaque : as of 2025-04

###### Consider
-       [fortran-curl](https://github.com/interkosmos/fortran-curl)
        -       **WARNING** : check your `libcurl` version compatibility
        -       [http-clien](https://github.com/fortran-lang/http-client) is
                based on `fortran-curl`, and provides a higher-level API which
                may be less performant because you can't reuse `curl` handlers.
