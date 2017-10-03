FROM ubuntu:14.04
RUN apt-get -y update && apt-get -y install software-properties-common
RUN add-apt-repository ppa:avsm/ppa
RUN apt-get -y update && apt-get -y install opam build-essential m4
RUN opam init
RUN opam switch 4.03.0
RUN eval `opam config env`
RUN opam install core re2 menhir
