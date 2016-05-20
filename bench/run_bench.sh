#!/bin/bash

set -e -o pipefail

ROOT="$(hg root)"
ppx="$ROOT/.ppx/JANE/ppx.exe"

time $ppx -dump-ast -inline-test-lib blah -o /dev/null input.ml
