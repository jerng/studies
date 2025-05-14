#!/bin/sh
#
# This script attempts to find local docker images dependent on imageA,
# by checking all local image histories to see if any contain the final
# layer of imageA.
#
# TODO : actually comprehend the Go Template syntax
#
# Many thanks for their advice on my improvement, to 
# : 2025-05-14 : https://www.reddit.com/user/BetterScripts/
#
# Init.
# : 2025-05
#
noprefix="          "
prefix="adocker : "
try="Try : 'adocker find-dependent-images [TAG or SHA]'"
tags=""
indented_tags=""
needle=""

printf "\n${prefix}written for (sh), not (bash)\n"

if [ "$1" != "find-dependent-images" ]
then 
    printf "%sdid not understand \$1 (this shell script's argument 1)\n\n" "$prefix"
    printf "$try\n"
else

    if [ "a" = "a$2" ] 
    then 
        printf "%sfind-dependent-images : did not understand \$2\n" "$prefix"
        printf "%s( this shell script's argument 2 )\n\n%s" "$prefix" "$try"
        exit 1 
    fi

    printf "%s[ $2 ] : searching for TAGS of this [ parent IMAGE ]\n\n" "$prefix"

    if ! tags=$(docker image inspect "$2" -f \
        '{{range .RepoTags}}{{.}}{{"\n"}}{{end}}')
    then 
        printf "%sfound none  \n\n" "$prefix"
        exit 1
    else
        indented_tags=$(printf "%s" "$tags" | sed "s/^/$noprefix/g")
        printf "%sfound ...  \n\n%s\n\n" "$prefix" "$indented_tags"
    fi

    printf "%ssearching for [ dependent IMAGES ] of [ parent IMAGE ]\n\n" "$prefix"
    needle=$( docker image inspect "$2" -f \
        '{{range .RootFS.Layers}}{{.}}{{"\n"}}{{end}}'\
        | grep -v '^$' \
        | tail -n 1 \
        | sed 's/sha256://g' \
    )
    printf "%sfound ... [ parent IMAGE's ] final LAYER (sha256) ...\n\n" "$prefix"
    printf "%s%s\n\n" "$noprefix" "$needle"
    printf "%s... in the history of the following IMAGES ...\n\n" "$prefix"

    docker images -q | \
        while read -r line
        do 
        if $(docker image inspect "$line" \
            -f '{{range .RootFS.Layers}}{{.}}{{"\n"}}{{end}}' \
            | grep -q "$needle" \
        )
        then
            printf "%s\n" $(docker image inspect "$line"\
                -f '{{range .RepoTags}}{{.}}{{"\n"}}{{end}}' )
        fi
        done \
        | sort -u \
        | sed "s/^/$noprefix/g"
fi
