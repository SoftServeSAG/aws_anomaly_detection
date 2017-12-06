#!/bin/bash
APP_NAME=$1
APP_PORT=$2

if [ $(docker inspect -f '{{.State.Running}}' $APP_NAMR) = "true" ]; then
  docker kill "${APP_NAME}"
fi

docker run --rm -d --memory="2g" --memory-swap="1g" --name "${APP_NAME}" -p "${APP_PORT}:3838" "${APP_NAME}"
