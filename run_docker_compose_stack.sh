#!/usr/bin/env bash

docker-compose -f docker-compose.yml down  && \
docker build -t dstream-server -f server.Dockerfile . && \
docker build -t dstream-int-test -f int-test.Dockerfile . && \
docker-compose -f docker-compose.yml up --build