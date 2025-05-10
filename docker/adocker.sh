#!/bin/sh

printf "\n"

if [ "$1" = "find-dependent-images" ];
then 

    if [ "a$2" = "a" ]; 
    then 
        printf "adocker : find-dependent-images : did not understand \$2 ( this shell script's argument 2)\n\n"
        printf "Try : adocker find-dependent-images [TAG or SHA]\n"
        exit 
    fi

    printf "adocker : [ $2 ] : searching for TAGS of this [ parent IMAGE ]\n\n"

    if ! sudo docker image inspect $2 -f \
        '{{range .RepoTags}}{{.}}{{"\n"}}{{end}}' > /dev/null; \
            # the clauses : > /dev/null $ 2>&1
            # are explained here :
            # https://unix.stackexchange.com/questions/119648/redirecting-to-dev-null
    then 
        printf "\nadocker : found none  \n\n" 
        exit;
    else
        printf "adocker : found ...  \n\n" 
    fi

    sudo docker image inspect $2 -f \
        '{{range .RepoTags}}{{.}}{{"\n"}}{{end}}' \
        | sed 's/^/          /g'
        # redundantly called a second time, but haven't figured out how
        # to save both the exit status of a subshell, and the subshell's
        # output

    printf "adocker : searching for dependent IMAGES of [ parent IMAGE ]\n"
    needle=$( sudo docker image inspect $2 -f \
        '{{range .RootFS.Layers}}{{.}}{{"\n"}}{{end}}' \
        | tac \
        | grep -E -m 1 . \
        | sed 's/sha256://g' \
    )
        printf "adocker : found ... [ parent IMAGE's ] final LAYER (sha256) ...\n\n"
        printf "          $needle\n\n"
        printf "adocker : ... in the history of the following IMAGES ...\n"

        sudo docker images -q \
            | xargs -d "\n" sh -c '
                    export RESULTS=""
                    for arg
                    do 
                        haystack=$(sudo docker image inspect $arg -f '\''{{range .RootFS.Layers}}{{.}}{{"\n"}}{{end}}'\'')

                        if ( echo "$haystack" | grep -q '$needle' );
                        then
                            export RESULTS=$RESULTS$(sudo docker image inspect $arg -f '\''{{range .RepoTags}}{{.}}{{"XDELIMITER"}}{{end}}'\'')
                        fi
                    done
                    echo $RESULTS | sed "s/XDELIMITER/\n/g" | sed "s/^/          /g" | sort | uniq 
                    '
else
    printf "adocker : did not understand \$1 (this shell script's argument 1)\n\n"
    printf "Try : adocker find-dependent-images [TAG or SHA]\n"
fi
