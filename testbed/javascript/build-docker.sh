#!/bin/bash

docker build --no-cache -t mxssy_sanitize -f Dockerfile.sanitize .
docker build --no-cache -t mxssy_dompurify_sanitize -f Dockerfile.dompurify_sanitize .
docker build --no-cache -t mxssy_run -f Dockerfile .
docker build --no-cache -t mxss_db_runner -f Dockerfile.db .

