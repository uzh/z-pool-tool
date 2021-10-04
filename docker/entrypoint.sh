#!/bin/sh

mkdir -p /app/logs
/app/run.exe migrate

exec "$@"
