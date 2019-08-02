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

RUN apt-get install -y haskell-platform

RUN curl -LO https://downloads.haskell.org/~ghc/8.0.1/ghc-8.0.1-x86_64-deb8-linux.tar.xz && \
    tar xf ghc-8.0.1-x86_64-deb8-linux.tar.xz 

RUN cd ghc-8.0.1 && \
#    ln -s /usr/lib/x86_64-linux-gnu/libgmp.so.10 /usr/lib/libgmp.so.3 && \
    ./configure && \
    make install && \
    rm -fr /ghc-8.0.1-x86_64-deb8-linux.tar.xz /ghc-8.0.1

RUN cabal update

# required to add Mongo repository; a space-heavy hack
RUN apt-get install -y gnupg lsb-release 

RUN echo "deb http://repo.mongodb.org/apt/debian "$(lsb_release -sc)"/mongodb-org/4.0 main" | tee /etc/apt/sources.list.d/mongodb.list && \
    apt-get update && \
    apt-get install -y --force-yes mongodb-org  && \
    mkdir -p /data/db 



# APPLICATION

RUN git clone https://github.com/jerng/Hell.git && \
    cd Hell && \
    cabal install -j4 -v

RUN sed -i 's/4.2/4.1/g' Hell/tryHell.sh

RUN mongod --fork --syslog && \
    sleep 3 && \
    echo '[{"testkey":"testvalue"}]'  > seed.json && \
    mongoimport --db testdb --collection testcollection --type json --file seed.json --jsonArray






# CONVENIENCE

RUN echo 'export PATH=$PATH:/root/.cabal/bin' >> ~/.bashrc && \
    echo 'mongod --fork --syslog' >> ~/.bashrc && \
    echo 'cd Hell && ./tryHell.sh' >> ~/.bashrc
                                                       
