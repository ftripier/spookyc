#!/usr/bin/env bash

if ! docker images | grep -q spookybox
then
  docker build . --tag spookybox
fi
docker run --rm -v `pwd`:/source spookybox "/source/${1}"
