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
  timeout 10 make
  echo -en "\n-->"
}
trap build SIGINT
trap exit SIGQUIT

python -m SimpleHTTPServer &

echo -e  "--> Press Ctrl+C to force build, Ctrl+\\ to exit."
while true; do
  build
  sleep 5
done
