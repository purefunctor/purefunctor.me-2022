FROM ubuntu:focal

RUN apt-get -y update \
 && apt-get -y install libnuma-dev

COPY dist-newstyle/bin/purefunctor-me /usr/bin/purefunctor-me
