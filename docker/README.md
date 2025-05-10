#`adocker` is a `docker` assistant

`adocker` is a `dash`/`sh` script : other-`sh` users, beware.

### A successful example 
```console
adocker find-dependent-images alpine

adocker : [ alpine ] : searching for TAGS of this [ parent IMAGE ]

adocker : found ...  

          alpine2:latest
          alpine:latest
          
adocker : searching for dependent IMAGES of [ parent IMAGE ]
adocker : found ... [ parent IMAGE's ] final LAYER (sha256) ...

          08000c18d16dadf9553d747a58cf44023423a9ab010aab96cf263d2216b8b350

adocker : ... in the history of the following IMAGES ...
          
          alpine2:latest
          alpine/curl:latest
          alpine/git:latest
          alpine:latest

```
