#!/bin/bash
EXECUTABLE="$1"
shift
./_build/default/analysis/${EXECUTABLE}.exe "$@"
