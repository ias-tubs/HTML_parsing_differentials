#!/bin/bash
seeds=()
for i in {1..10}; do
  RANDOM_NUMBER="$(shuf --random-source='/dev/urandom' -n 1 -i 1-32767)"
  seeds+=( "-s${RANDOM_NUMBER}" )
done
./_build/default/generate/local.exe ${seeds[@]} "$@"
