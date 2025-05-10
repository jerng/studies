#!/bin/sh

printf "\n*   Using : #!/bin/sh\n"

printf "\n"
a='if (exit 0); then echo evaluated to TRUE; else echo evaluated to FALSE; fi'
printf "1A. Attempt the following script :\n*   \"$a\"\n*\n"
sh -c "$a"

printf "\n"
b='if (exit 1); then echo evaluated to TRUE; else echo evaluated to FALSE; fi'
printf "1B. Attempt the following script :\n*   \"$b\"\n*\n"
sh -c "$b"

printf "\n"
e='if (ls); then echo evaluated to TRUE; else echo evaluated to FALSE; fi'
printf "2A. Attempt the following script :\n*   \"$e\"\n*\n"
sh -c "$e"

printf "\n"
d='if (ls -invalid-option); then echo evaluated to TRUE; else echo evaluated to FALSE; fi'
printf "2B. Attempt the following script :\n*   \"$d\"\n*\n"
sh -c "$d"

printf "\n"
c='if [ ( exit 0 ) ]; then echo evaluated to TRUE; else echo evaluated to FALSE; fi'
printf "3. Attempt the following script :\n*   \"$c\"\n*\n"
sh -c "$c"
printf "*\n*   Because, \"[\" is special syntax.\n\nAlso see : \"info test\""
