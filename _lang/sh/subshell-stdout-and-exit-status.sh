#!/bin/sh

printf "\n*   Using : #!/bin/sh\n"

printf "\n"
a='if (echo inner thoughts; exit 1); 
then echo evaluated to TRUE; 
else echo evaluated to FALSE; 
    fi'
printf "1. Attempt the following script :\n*   \"$a\"\n*\n"
sh -c "$a"

printf "\n"
b='if output=$(echo inner thoughts; exit 1); 
then 
    printf "\$? : $?\n"
    echo evaluated to TRUE; 
else 
    printf "\$? : $?\n"
    echo evaluated to FALSE; 
fi
printf "output : $output\n"
'
printf "2. Attempt the following script :\n*   \"$b\"\n*\n"
sh -c "$b"

printf "\n"
c='if output=$(echo inner thoughts; exit 0); 
then 
    printf "\$? : $?\n"
    echo evaluated to TRUE; 
else 
    printf "\$? : $?\n"
    echo evaluated to FALSE; 
fi
printf "output : $output\n"
'
printf "3. Attempt the following script :\n*   \"$c\"\n*\n"
sh -c "$c"

