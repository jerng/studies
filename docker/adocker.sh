#!/bin/sh
#
# This script attempts to find local docker images dependent on imageA,
# by checking all local image histories to see if any contain the final
# layer of imageA.

noprefix="          "
  prefix="adocker : "
     try="Try : 'adocker find-dependent-images [TAG or SHA]'"

     printf "\n$prefix""written for (sh), not (bash)\n"

if [ "$1" != "find-dependent-images" ];
then 
    printf "$prefix""did not understand \$1 (this shell script's argument 1)\n\n"
    printf "$try\n"
else

    if [ "a$2" = "a" ]; 
    then 
        printf "%s\n" \
            "$prefix""find-dependent-images : did not understand \$2 " \
            "$noprefix( this shell script's argument 2 )" \
            "" \
            "$try" 
        exit 
    fi

    printf "$prefix""[ $2 ] : searching for TAGS of this [ parent IMAGE ]\n\n"

    if ! inspection=$(sudo docker image inspect $2 -f \
        '{{range .RepoTags}}{{.}}{{"\n"}}{{end}}');
    then 
        printf "$prefix""found none  \n\n" 
        exit;
    else
        printf "$prefix""found ...  \n\n$(echo $inspection \
            | sed "s/^/$noprefix/g" )\n\n" 
    fi

    printf "$prefix""searching for dependent IMAGES of [ parent IMAGE ]\n"
    needle=$( sudo docker image inspect $2 -f \
        '{{range .RootFS.Layers}}{{.}}{{"\n"}}{{end}}' \
        | tac \
        | grep -E -m 1 . \
        | sed 's/sha256://g' \
    )
        printf "$prefix""found ... [ parent IMAGE's ] final LAYER (sha256) ...\n\n"
        printf "$noprefix$needle\n\n"
        printf "$prefix""... in the history of the following IMAGES ...\n"

        sudo docker images -q \
            | xargs -d "\n" sh -c '

        export results=""
        for arg
        do 
            haystack=$(sudo docker image inspect $arg \
                -f '\''{{range .RootFS.Layers}}{{.}}{{"\n"}}{{end}}'\'')

            if ( echo "$haystack" | grep -q '$needle' );
            then
                export results=$results$(\
                    sudo docker image inspect $arg\
                    -f '\''{{range .RepoTags}}{{.}}{{"XDELIMITER"}}{{end}}'\'')
            fi
        done
        echo $results \
            | sed "s/XDELIMITER/\n/g" \
            | sed "s/^/'"$noprefix"'/g" \
            | sort \
            | uniq '
fi
