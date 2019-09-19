#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
cyclone 175.sld
cyclone "$1.scm"
