#!/bin/bash
APP_NAME=$1
APP_PORT=$2

MEMORY="2g"
SWAP_MEMORY="1g"
CPUS="4"

if [ $(docker inspect -f '{{.State.Running}}' $APP_NAME) = "true" ]; then
  docker kill "${APP_NAME}"
fi

docker run -d --cpus="${CPUS}" --memory="${MEMORY}" --memory-swap="${SWAP_MEMORY}" --name "${APP_NAME}" -p "${APP_PORT}:3838" "${APP_NAME}"
