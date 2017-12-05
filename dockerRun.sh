#!/bin/bash
APP_NAME=$1
APP_PORT=$2

docker run --rm -d --memory="2g" --memory-swap="1g" -p "${APP_PORT}:3838" "${APP_NAME}"
