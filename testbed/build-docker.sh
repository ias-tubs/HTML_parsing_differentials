#!/bin/bash

declare -a languages=("javascript" "php" "dotnet" "ruby" "java")
CWD="$(pwd)"

for lang in "${languages[@]}"
do
  LANG_DIR="$CWD/$lang"
  echo "Building docker container for '$lang' in $LANG_DIR"
  cd "$LANG_DIR"
  bash ./build-docker.sh
  cd -
done

