#!/bin/bash
set -euo pipefail

if [ "${CONTAINER_ROLE:-app}" = "app" ]; then
  echo "[entrypoint] Running database migrations..."

  echo "[entrypoint] migrate.root"
  /app/run.exe migrate.root

  echo "[entrypoint] migrate.tenant_migrations_pending"
  /app/run.exe migrate.tenant_migrations_pending || true

  echo "[entrypoint] migrate.tenant"
  /app/run.exe migrate.tenant
fi

echo "[entrypoint] Starting application: $*"
exec /app/run.exe "$@"
