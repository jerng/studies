bash -xvc 'y="2> /dev/null";echo test0; $(echo test1; echo test2 "$y")'
