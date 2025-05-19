# problems ( different )
bash -xvc 'y="2> /dev/null";echo test0; z=$(echo test1; echo test2 "$y")'
bash -xvc 'y="2> /dev/null";echo test0; z=$(echo test1; echo test2 $y)'

# solution
bash -xvc 'y="2> /dev/null";echo test0; z=$(echo test1; echo test2 '"$y"')'
