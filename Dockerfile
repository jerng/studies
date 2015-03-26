# unoptimised

FROM alanz/debian-base-64

MAINTAINER yangjerng@gmail.com

ENV DEBIAN_FRONTEND noninteractive

####### GHC 7.4.1 ######################

RUN wget http://www.haskell.org/ghc/dist/7.4.1/ghc-7.4.1-x86_64-unknown-linux.tar.bz2
RUN tar xvfj ghc-7.4.1-x86_64-unknown-linux.tar.bz2
RUN cd ghc-7.4.1 && ./configure
RUN cd ghc-7.4.1 && make install

# Tools 
RUN apt-get update && apt-get install -y curl git vim screen

RUN wget  http://hackage.haskell.org/packages/archive/Cabal/1.16.0.3/Cabal-1.16.0.3.tar.gz
RUN tar xaf Cabal-1.16.0.3.tar.gz
RUN cd Cabal-1.16.0.3 && \
    ghc --make Setup && \
    ./Setup configure --user && \
    ./Setup build && \
    ./Setup install

RUN git clone https://github.com/haskell/cabal.git && \
    cd cabal && \
    git checkout tags/cabal-install-v1.16.0.2 && \
    cd cabal-install && \
    chmod 700 bootstrap.sh && \
    sed -i 's/--fail/--fail -L/g' bootstrap.sh && \
    ./bootstrap.sh
RUN export PATH=$PATH:/root/.cabal/bin && \
    cabal update && \
    cabal install cabal-dev

# Clean up
RUN rm -fr ghc-7.4.1-x86_64-unknown-linux.tar.bz2 ghc-7.4.1 \
           Cabal-1.16.0.3.tar.gz Cabal-1.16.0.3 \
           cabal

# App
RUN git clone https://github.com/jerng/Hell.git && \
    cd Hell && \
    export PATH=$PATH:/root/.cabal/bin && \
    cabal-dev install -j2
RUN sed -i 's/4.2/4.1/g' Hell/tryHell.sh
RUN apt-get install mongodb-server -y && \
    mkdir -p /data/db 

# Convenience
RUN mongod --fork --syslog && \ 
    sleep 3 && \
    echo '[{"testkey":"testvalue"}]'  > seed.json && \
    mongoimport --db testdb --collection testcollection --type json --file seed.json --jsonArray

RUN echo 'export PATH=$PATH:/root/.cabal/bin' >> ~/.bashrc && \
    echo 'mongod --fork --syslog' >> ~/.bashrc && \
    echo 'cd Hell && ./tryHell.sh' >> ~/.bashrc

