# Attempt to replace compilation from source, of GHC, with a binary package.




# STACK

FROM debian:7.8
MAINTAINER yangjerng@gmail.com
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && \
    apt-get install -y \
    curl git vim screen bzip2 \
      # tools
    libgmp3-dev libncursesw5 gcc make \
      # GHC compilation dependency
    zlib1g-dev
      # cabal compilation dependency

RUN curl -LO http://www.haskell.org/ghc/dist/7.4.1/ghc-7.4.1-x86_64-unknown-linux.tar.bz2 && \
    tar xvfj ghc-7.4.1-x86_64-unknown-linux.tar.bz2

RUN cd ghc-7.4.1 && \
    ln -s /usr/lib/x86_64-linux-gnu/libgmp.so.10 /usr/lib/libgmp.so.3 && \
    ./configure && \
    make install && \
    rm -fr /ghc-7.4.1-x86_64-unknown-linux.tar.bz2 /ghc-7.4.1

RUN curl -LO  http://hackage.haskell.org/packages/archive/Cabal/1.16.0.3/Cabal-1.16.0.3.tar.gz && \
    tar xaf Cabal-1.16.0.3.tar.gz 

RUN cd Cabal-1.16.0.3 && \
    ghc --make Setup && \
    ./Setup configure --user && \
    ./Setup build && \
    ./Setup install && \
    rm -fr /Cabal-1.16.0.3.tar.gz /Cabal-1.16.0.3

RUN git clone https://github.com/haskell/cabal.git && \
    cd cabal && \
    git checkout tags/cabal-install-v1.16.0.2 && \
    cd cabal-install && \
    chmod 700 bootstrap.sh && \
    sed -i 's/--fail/--fail -L/g' bootstrap.sh && \
    ./bootstrap.sh && \
    rm -fr /cabal

RUN export PATH=$PATH:/root/.cabal/bin && \
    cabal update && \
    cabal install cabal-dev

RUN echo "deb http://repo.mongodb.org/apt/debian wheezy/mongodb-org/3.0 main" | tee /etc/apt/sources.list.d/mongodb-org-3.0.list && \
    apt-get update && \
    apt-get install -y --force-yes mongodb-org  && \
    mkdir -p /data/db 



# APPLICATION

RUN git clone https://github.com/jerng/Hell.git && \
    cd Hell && \
    export PATH=$PATH:/root/.cabal/bin && \
    cabal-dev install -j2

RUN sed -i 's/4.2/4.1/g' Hell/tryHell.sh

RUN mongod --fork --syslog && \
    sleep 3 && \
    echo '[{"testkey":"testvalue"}]'  > seed.json && \
    mongoimport --db testdb --collection testcollection --type json --file seed.json --jsonArray






# CONVENIENCE

RUN echo 'export PATH=$PATH:/root/.cabal/bin' >> ~/.bashrc && \
    echo 'mongod --fork --syslog' >> ~/.bashrc && \
    echo 'cd Hell && ./tryHell.sh' >> ~/.bashrc
                                                       
