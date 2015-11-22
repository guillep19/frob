#!/usr/bin/env bash
# script:  watch
# author:  Mike Smullin <mike@smullindesign.com>
# license: GPLv3
# description:
#   watches the given path for changes
#   and executes a given command when changes occur
# usage:
#   watch <path> <cmd...>
#
 
build() {
  echo -en " building...\n\n"
  make clean
  timeout 10 make
  echo -en "\n-->"
}
trap build SIGQUIT
trap exit SIGINT

#python -m SimpleHTTPServer &

echo -e  "--> Press Ctrl+C to force build, Ctrl+\\ to exit."
while true; do
  build
  mv Informe.pdf IInforme.pdf
  sleep 10
done
