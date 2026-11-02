#!/usr/bin/env bash

# Development loop: dune rebuilds on source changes (watch mode) and the server
# is restarted whenever run.exe is relinked. Assets are built once on start
# (`yarn build`) and rebuilt whenever something in resources/ changes.
#
# Other dune commands (`make format`, `make test`, ...) are forwarded to this
# watch instance. `dune exec --watch` (or an alias running the server) would
# restart the server on its own, but forwarded builds hang while it runs and
# alias output only shows once the action ends (dune 3.24).
#
# Watch mode needs inotify, which 9p mounts (Windows repositories under /mnt/c)
# don't provide. There the sources are polled instead, which can be forced with
# DEV_POLL=1.

set -euo pipefail

cd "$(dirname "$0")/.."

EXE=_build/default/pool/run/run.exe
# NOTE: Passing exclusions replaces dune's defaults, whose `/.git` doesn't match
# the top-level `.git` directory (written to by git tooling on every change)
WATCH_EXCLUSIONS='^\.git;/\.git;^_opam;/_opam;^\.#.*;/\.#.*;~$;^#[^#]*#$;/#[^#]*#$;^4913$;/4913$;^node_modules;^logs;^public;^\.parcel-cache'

needs_polling() {
  [[ ${DEV_POLL:-0} == 1 ]] && return 0
  [[ $(uname) == Linux ]] || return 1
  case $(stat -f -c %T .) in
    9p | v9fs) return 0 ;;
  esac
  return 1
}

# NOTE: Holds the build lock only while building, so other dune commands still work
poll_build() {
  local changed="$stamp.src"
  while true; do
    touch "$changed"
    dune build || true
    until [[ -n $(find . \( -name _build -o -name node_modules -o -name .git -o -name logs -o -name public -o -name .parcel-cache \) -prune \
      -o -newer "$changed" -type f -print -quit) ]]; do
      sleep 2
    done
  done
}

# NOTE: Polls instead of using `yarn watch`, whose Parcel watcher has the same
# inotify limitations; resources/ is small enough to scan every 2s
build_assets() {
  local changed="$stamp.assets"
  while true; do
    touch "$changed"
    yarn build || echo "⚠ yarn build failed, waiting for changes in resources/" >&2
    until [[ -n $(find resources -newer "$changed" -print -quit) ]]; do
      sleep 2
    done
  done
}

dune_pid=""
assets_pid=""
server_pid=""
stamp=$(mktemp)
trap 'kill $server_pid $assets_pid $dune_pid 2>/dev/null || true; rm -f "$stamp" "$stamp.src" "$stamp.assets"' EXIT
trap 'exit 130' INT TERM

if needs_polling; then
  echo "⚠ No inotify support on this filesystem, polling for changes every 2s"
  poll_build &
else
  dune build --watch --watch-exclusions="$WATCH_EXCLUSIONS" &
fi
dune_pid=$!

build_assets &
assets_pid=$!

while sleep 1; do
  kill -0 "$dune_pid" 2>/dev/null || {
    echo "dune exited (is another 'make dev' running?), stopping" >&2
    exit 1
  }
  [[ -f $EXE ]] || continue
  [[ -z $server_pid || $EXE -nt $stamp ]] || continue
  # wait for the linker to finish writing the executable
  while touch "$stamp" && sleep 1 && [[ $EXE -nt $stamp ]]; do :; done
  if [[ -n $server_pid ]]; then
    kill "$server_pid" 2>/dev/null || true
    wait "$server_pid" 2>/dev/null || true
  fi
  SIHL_ENV=development "$EXE" server &
  server_pid=$!
done
