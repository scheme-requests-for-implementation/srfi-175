#!/bin/sh
# You need the r7rs egg: `chicken-install r7rs`.
set -eu
dir="$(dirname "$0")"
exec csc -R r7rs -prologue "$dir/ascii.sld" "$dir/$1.scm"
