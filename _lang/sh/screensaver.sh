#!/bin/sh
# loops through different highlights of a text

array="mq scheme apache wasm golang haskell rust ocaml \
alpine ubuntu debian SUS \
\\.[[:digit:]]MB \\.[[:digit:]]GB"
while true; do
    echo "$array" | tr ' ' '\n' | while read string; do
        lsdockerthings.sh | grep -v -e tmpfs | \
            grep --color=always -e ^ -e "$string.*"
        sleep 2
    done
done
