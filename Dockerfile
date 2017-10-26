FROM ubuntu:14.04
RUN apt-get -y update && apt-get -y install software-properties-common
RUN add-apt-repository ppa:avsm/ppa
RUN apt-get -y update && apt-get -y install opam build-essential m4
RUN opam init
RUN opam switch 4.03.0
RUN eval `opam config env`
RUN opam install core re2 menhir
RUN opam config env >> ~/.profile
RUN opam config env >> ~/.bashrc
RUN wget https://bitheap.org/cram/cram-0.6.tar.gz
RUN tar zxvf cram-0.6.tar.gz
WORKDIR cram-0.6
RUN make install
COPY ./compiler /compiler
COPY ./build.sh /compiler
WORKDIR /compiler
RUN /compiler/build.sh
RUN ln -s /compiler/_build/spooky.native /usr/local/bin/compile
ENTRYPOINT [ "compile", "-j" ]
