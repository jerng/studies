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
# : 2025-05-14 : updated and passed shellcheck.net
# : 2025-05-19 : added 'find-all' to 'find' : does not yet pass shellcheck

# define
quiet=false
noprefix="          "
  prefix="adocker : "
try="Try : 'adocker (find|find-dependent-images) [TAG or SHA]'"

# init only
command_inspect_parent_image_tags=""
tags=""
indented_tags=""
needle=""

[ "$3" = "-q" ] && quiet=true

"$quiet" || printf "\n%swritten for (sh), not (bash)\n" "$prefix"

if 
    [ "$1" = "find-all" ]  
then

    # ACT 2

    printf "\n"

    docker images -q | uniq | \
        while read -r line; \
        do
            printf "%s [ parent IMAGE ] : has dependents :\n" "$line"
            adocker find "$line" -q
            printf "\n"
        done

    exit
elif 
    { [ "$1" != "find" ] && [ "$1" != "find-dependent-images" ]; } 
then 
    printf "%sdid not understand \$1 (this shell script's argument 1)\n\n" "$prefix"
    printf "%s\n" "$try"
else

    if [ "a" = "a$2" ] 
    then 
        printf "%sfind-dependent-images : did not understand \$2\n" "$prefix"
        printf "%s( this shell script's argument 2 )\n\n%s" "$prefix" "$try"
        exit 1 
    fi

    # ACT 1, SCENE 1

    command_inspect_parent_image_tags='docker image inspect '"$2"' -f '\''{{range.RepoTags}}{{.}}{{"\n"}}{{end}}'\'

    if "$quiet"
    then
        if ! tags="$($command_inspect_parent_image_tags 2> /dev/null)"
        then 
            exit 1
        fi
    else
        printf "%s[ $2 ] : searching for TAGS of this [ parent IMAGE ]\n\n" "$prefix"
        if ! tags="$($command_inspect_parent_image_tags)"
        then 
            printf "%sfound none  \n\n" "$prefix"
            exit 1
        else
            indented_tags="$(printf "%s" "$tags" | sed -e "s/^/$noprefix/g" -e "s/'//g")"
            printf "%sfound ...  \n\n%s\n" "$prefix" "$indented_tags"
        fi
    fi

    # ACT 1, SCENE 2

    "$quiet" || printf "%ssearching for [ TAGS of dependent IMAGES ] of [ parent IMAGE ]\n\n" "$prefix"
    needle="$( docker image inspect "$2" -f \
        '{{range .RootFS.Layers}}{{.}}{{"\n"}}{{end}}'\
        | grep -v '^$' \
        | tail -n 1 \
        | sed 's/sha256://g' \
    )"
    "$quiet" || printf "%sfound ... [ parent IMAGE's ] final LAYER (sha256) ...\n\n" "$prefix"
    "$quiet" || printf "%s%s\n\n" "$noprefix" "$needle"
    "$quiet" || printf "%s... in the history of the following IMAGES ...\n\n" "$prefix"

    # ACT 1, SCENE 3

    docker images -q | \
        while read -r line
        do 
        if docker image inspect "$line" \
            -f '{{range .RootFS.Layers}}{{.}}{{"\n"}}{{end}}' \
            | grep -q "$needle"
        then
            printf "%s\n" "$(docker image inspect "$line" \
                -f '{{range .RepoTags}}{{.}}{{"\n"}}{{end}}' )"
        fi
        done \
        | sort -u \
        | { if "$quiet"; then cat ; else sed "s/^/$noprefix/g"; fi; }
fi
