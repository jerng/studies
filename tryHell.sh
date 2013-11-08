#!/bin/bash
clear
echo "************************************************************************"
echo "Working directory is ."
echo ""
echo "Trying to compile ./makeHell.hs with GHC"
echo "If there are missing dependencies, check the ./hell-version.cabal and rerun 'cabal-dev install' in this directory."
ghc -package-conf ./cabal-dev/packages-7.4.2.conf/ ./makeHell.hs
echo ""
echo "Trying to execute ./makeHell"
./makeHell
rm ./makeHell
rm ./Hell/*.hi
rm ./Hell/*.o
rm *.hi
rm *.o
echo "Cleaned up working directory. (You may alter this.)"

cd app
echo ""
echo "************************************************************************"
echo "Switched working directory to ./app"
echo ""
echo "Removing any existing ./Server"
rm ./Server
echo "Trying to compile ./Server.hs with GHC"
echo "If there are missing dependencies, check the ./hell-version.cabal and rerun 'cabal-dev install' in this directory."
ghc -package-conf ../cabal-dev/packages-7.4.2.conf/ ./Server.hs
rm ./Hell/*.hi
rm ./Hell/*.o
rm *.hi
rm *.o
echo ""
echo "Cleaned up working directory. (You may alter this.)"
echo ""
echo "Trying to execute ./Server (Hit Ctrl+c to kill the Server...)"
./Server
