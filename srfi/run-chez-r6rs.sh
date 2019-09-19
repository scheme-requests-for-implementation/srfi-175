#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec chez --libdirs "$dir" --program "$dir/$1.sps"
