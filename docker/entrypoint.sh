#!/bin/sh

mkdir -p /app/logs
/app/run.exe migrate.root
/app/run.exe migrate.tenant

exec "$@"
