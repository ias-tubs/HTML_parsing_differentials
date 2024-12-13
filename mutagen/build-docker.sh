#!/bin/bash

docker build --no-cache -t mxssy_gen -f docker/Dockerfile.generator .
docker build --no-cache -t mxssy_analysis -f docker/Dockerfile.analysis .
