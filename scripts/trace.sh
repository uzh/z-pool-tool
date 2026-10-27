#!/usr/bin/env bash

# Runs the app (`dev`), the worker under load (`worker`) or the test suite
# (`test`) with memtrace and optionally a sanitizer, and writes a markdown report
# next to the raw traces and logs:
#
#   make dev-trace [SANITIZER=tsan|asan|lsan] [DURATION=60]
#   make worker-trace [SANITIZER=...] [DURATION=600] [JOBS=500] [WAVE=60]
#   make test-trace [SANITIZER=tsan|asan|lsan]
#
# Output: logs/trace/<mode>-<sanitizer>-<timestamp>/report.md
#
# Sanitizers need the compiler-only switches created by .devcontainer/postCreate.sh;
# the project dependencies are installed into them on first use (~15 min each).
# Every switch builds into a build directory of its own (_build_<sanitizer>),
# so the default _build and a running `make dev` are left alone.
#
# `dev` starts the server and the worker (no watch mode) and stops them on
# Ctrl-C or after DURATION seconds. NOTE: Sihl's .env loader overrides the
# environment, so a MEMTRACE in .env wins; its files are linked into the run.
#
# `worker` starts only the worker and enqueues JOBS test emails every WAVE
# seconds until WAVE seconds before the end, so the last wave drains and the
# live heap shows whether it returns to its baseline. The emails really go out
# (EMAIL_BYPASS_INTERCEPT=true) to the SMTP server of the root database, in the
# devcontainer that's Mailpit. RSS, open fds and Mailpit's accepted count are
# sampled every 5s, the guard LRU cache sizes are taken from the worker log.
#
# Settings: MEMTRACE_RATE (default 1e-4, memtrace's own default 1e-6 records
# too few samples for short runs).

set -euo pipefail

cd "$(dirname "$0")/.."

mode=${1:-}
case $mode in
  dev | worker | test) ;;
  *)
    echo "usage: $0 dev|worker|test" >&2
    exit 2
    ;;
esac

sanitizer=${SANITIZER:-none}
case $sanitizer in
  none | "") option="" sanitizer=none ;;
  tsan) option=tsan ;;
  asan | address-sanitizer) option=address-sanitizer sanitizer=asan ;;
  lsan | leak-sanitizer) option=leak-sanitizer sanitizer=lsan ;;
  *)
    echo "unknown SANITIZER=$sanitizer, use tsan, asan or lsan" >&2
    exit 2
    ;;
esac

base_switch=$(opam switch show)
switch=$base_switch${option:++$option}
build_dir=_build_${option:-trace}
out=logs/trace/$mode-$sanitizer-$(date +%Y%m%d-%H%M%S)
out_abs=$PWD/$out
mkdir -p "$out"

# the sanitized toolchain (ocamlc, dune, ...) leaks at exit, which fails builds
toolchain_env=(ASAN_OPTIONS=detect_leaks=0 LSAN_OPTIONS=detect_leaks=0)

if ! opam switch list --short | grep -qxF "$switch"; then
  echo "opam switch $switch is missing, see .devcontainer/README.md#debug-switches" >&2
  exit 1
fi
if [[ -z $(opam list --switch="$switch" --installed --short sihl 2>/dev/null) ]]; then
  echo "Installing the project dependencies into $switch"
  # NOTE: ocamlyacc 5.5.2 reads past a buffer (yacc/closure.c:109 set_first_derives),
  # which ASan aborts on when building octavius (via ppx_js_style). It's a build
  # tool only, so the ASan switch uses the uninstrumented one of the base switch.
  if [[ $option == address-sanitizer ]]; then
    yacc=$(opam var --switch="$switch" bin)/ocamlyacc
    [[ -f $yacc.asan ]] || mv "$yacc" "$yacc.asan"
    cp "$(opam var --switch="$base_switch" bin)/ocamlyacc" "$yacc"
  fi
  env "${toolchain_env[@]}" opam install --switch="$switch" --working-dir --deps-only --with-test --with-dev-setup -y .
fi

dune() {
  env "${toolchain_env[@]}" opam exec --switch="$switch" -- dune "$@" --root . --build-dir="$build_dir"
}

echo "Building with $switch into $build_dir"
# NOTE: The first build after a dependency change in the ASan switch can fail
# with "Unbound module Pool_database__", a second one succeeds
targets=(pool/run/run.exe scripts/trace_report/trace_report.exe @pool/test/all)
dune build "${targets[@]}" || dune build "${targets[@]}"
run_exe=$build_dir/default/pool/run/run.exe

# NOTE: No OCAMLRUNPARAM=c (free the runtime at exit): dune inherits it and
# deadlocks at exit after running the tests (dune 3.24)
run_env=(
  MEMTRACE="$out_abs/memtrace-%p.ctf"
  MEMTRACE_RATE="${MEMTRACE_RATE:-1e-4}"
)
# NOTE: ASan also reads LSAN_OPTIONS, whose exitcode=0 would make a process
# aborted by ASan exit with 0 (and the tests pass). Data races and leaks are
# reported but don't fail the run, memory errors do. Leaks are the LSan run's job.
case $sanitizer in
  tsan) run_env+=(TSAN_OPTIONS="log_path=$out_abs/tsan exitcode=0") ;;
  asan) run_env+=(ASAN_OPTIONS="log_path=$out_abs/asan detect_leaks=0") ;;
  lsan) run_env+=(LSAN_OPTIONS="log_path=$out_abs/lsan exitcode=0") ;;
esac

status=0
case $mode in
  dev | worker)
    if [[ $mode == dev ]] && (echo >/dev/tcp/127.0.0.1/3000) 2>/dev/null; then
      echo "Port 3000 is in use, stop 'make dev' first" >&2
      exit 1
    fi
    [[ $mode == worker || -d public ]] || yarn build
    commands=(server worker)
    duration=${DURATION:-}
    if [[ $mode == worker ]]; then
      commands=(worker)
      duration=${DURATION:-600}
      # NOTE: The DevInbox (non-production default) keeps every email in memory
      run_env+=(EMAIL_BYPASS_INTERCEPT=true)
    fi
    pids=()
    sampler=""
    stop() {
      kill -TERM "${pids[@]}" $sampler 2>/dev/null || true
      wait "${pids[@]}" 2>/dev/null || true
    }
    trap 'stop' INT TERM
    for command in "${commands[@]}"; do
      env "${run_env[@]}" SIHL_ENV=development "$run_exe" "$command" \
        >"$out/$command.out" 2>&1 &
      pids+=($!)
    done
    # MEMTRACE from .env takes precedence (Sihl loads it over the environment),
    # link the files it points to into the run directory
    sleep 5
    for command in "${commands[@]}"; do
      file=$(sed -n 's/^memtrace: writing //p' "$out/$command.out" | head -n 1)
      if [[ -z $file ]]; then
        echo "⚠ $command isn't traced, see $out/$command.out" >&2
      elif [[ $file != "$out_abs"/* ]]; then
        echo "MEMTRACE from .env: $command traces to $file"
        ln -sf "$file" "$out/memtrace-$command.ctf"
      fi
    done
    echo "${commands[*]} running (output in $out), stop with Ctrl-C${duration:+ or after ${duration}s}"
    if [[ $mode == worker ]]; then
      worker_pid=${pids[0]}
      mailpit=${MAILPIT_URL:-http://mailtrap:8025}
      (
        echo -e "seconds\trss_kb\tfds\tmailpit_accepted"
        start=$SECONDS
        while kill -0 "$worker_pid" 2>/dev/null; do
          rss=$(awk '/^VmRSS/ {print $2}' "/proc/$worker_pid/status" 2>/dev/null || echo -)
          fds=$(find "/proc/$worker_pid/fd" -mindepth 1 2>/dev/null | wc -l)
          accepted=$(curl -s -m 2 "$mailpit/api/v1/info" |
            sed -n 's/.*"SMTPAccepted":\([0-9]*\).*/\1/p')
          echo -e "$((SECONDS - start))\t$rss\t$fds\t${accepted:--}"
          sleep 5
        done
      ) >"$out/resources.tsv" &
      sampler=$!
      jobs=${JOBS:-500}
      wave=${WAVE:-60}
      start=$SECONDS
      while ((SECONDS - start + wave <= duration)); do
        echo "Enqueueing $jobs emails"
        # the load generator isn't under test
        env "${toolchain_env[@]}" MEMTRACE= SIHL_ENV=development QUEUE_FORCE_ASYNC=true \
          "$build_dir/default/pool/run/run.exe" test.mail worker-trace@example.com worker-trace@example.com "$jobs" \
          >>"$out/enqueue.out" 2>&1 || echo "⚠ enqueueing failed, see $out/enqueue.out" >&2
        sleep $((wave - (SECONDS - start) % wave)) &
        wait $! || true
      done
      sleep $((duration - (SECONDS - start) > 0 ? duration - (SECONDS - start) : 0)) &
      wait $! || true
    elif [[ -n $duration ]]; then
      sleep "$duration" &
      wait $! || true
    else
      wait "${pids[@]}" || true
    fi
    stop
    trap - INT TERM
    ;;
  test)
    env "${run_env[@]}" SIHL_ENV=test QUEUE_FORCE_ASYNC=true \
      "$run_exe" seed.tenant.test >"$out/seed.out" 2>&1 || status=$?
    if [[ $status == 0 ]]; then
      env "${run_env[@]}" SIHL_ENV=test QUEUE_FORCE_ASYNC=true \
        opam exec --switch="$switch" -- dune build --root . --build-dir="$build_dir" --force @runtest \
        >"$out/test.out" 2>&1 || status=$?
    fi
    ;;
esac

{
  echo "# Trace report: $mode, sanitizer $sanitizer"
  echo
  echo "- switch: $switch, commit $(git rev-parse --short HEAD)$(git diff --quiet || echo ' (dirty)'), $(date -Iseconds)"
  if [[ $mode == test ]]; then
    echo "- tests: $([[ $status == 0 ]] && echo passed || echo "FAILED (exit $status), see test.out/seed.out")"
  fi
  if [[ -f $out/resources.tsv ]]; then
    echo
    echo "## Worker resources (every 5s, Mailpit: cumulative accepted emails)"
    echo
    echo "| time | RSS | open fds | Mailpit accepted |"
    echo "|---:|---:|---:|---:|"
    rows=$(($(wc -l <"$out/resources.tsv") - 1))
    step=$(((rows + 19) / 20))
    tail -n +2 "$out/resources.tsv" | awk -F '\t' -v step="${step:-1}" -v rows="$rows" \
      'NR % step == 1 || step == 1 || NR == rows { printf "| %ss | %.1f MiB | %s | %s |\n", $1, $2 / 1024, $3, $4 }'
  fi
  if [[ -f $out/worker.out ]] && grep -q "Updated size of guard cache" "$out/worker.out"; then
    echo
    echo "## Guard LRU caches (capacity 2048 each, last logged size)"
    echo
    sed -n 's/.*Updated size of guard cache \([a-z_]*\): \([0-9]*\).*/\1 \2/p' "$out/worker.out" |
      awk '{ size[$1] = $2 } END { for (c in size) printf "- %s: %s\n", c, size[c] }'
  fi
  env "${toolchain_env[@]}" "$build_dir/default/scripts/trace_report/trace_report.exe" "$out"
} >"$out/report.md"

echo "Report: $out/report.md"
exit "$status"
