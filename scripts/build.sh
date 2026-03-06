#!/bin/sh

# this script is used to build the project
# it is setup to run in a RHEL 8 or Debian-based Docker image

# immediately when a command fails and print each command
set -ex

printTitle() {
  echo -e "\e[1;33m~~~ $1: ~~~\e[0m"
}

# Skip system dependency installation when running inside the CI build image,
# as all required packages are already baked in.
if [ "${SKIP_SYS_DEPS:-0}" != "1" ]; then
  printTitle "Installing system dependencies"
  # Detect package manager and install dependencies
  if command -v dnf >/dev/null 2>&1; then
    dnf install -y --allowerasing --skip-broken --nobest \
      m4 \
      gcc \
      gcc-c++ \
      gmp-devel \
      wget \
      curl \
      make \
      cmake \
      git \
      openssl-devel \
      mariadb-connector-c-devel
  elif command -v yum >/dev/null 2>&1; then
    yum install -y \
      m4 \
      gcc \
      gcc-c++ \
      libev-devel \
      gmp-devel \
      pkgconfig \
      wget \
      curl \
      make \
      cmake \
      git \
      openssl-devel \
      mariadb-devel
  elif command -v apt-get >/dev/null 2>&1; then
    apt-get update -q
    apt-get install -yqq --no-install-recommends \
      m4 \
      gcc \
      libev-dev \
      libgmp-dev \
      pkg-config \
      wget \
      curl \
      build-essential \
      make \
      cmake \
      git \
      libssl-dev \
      libmariadb-dev
    apt-get autoremove -y
    apt-get clean all
  else
    echo "Error: No supported package manager found (dnf, yum, or apt-get)"
    exit 1
  fi
else
  printTitle "Skipping system dependencies (already installed in image)"
fi

printTitle "Mark source dir as save"
git config --global --add safe.directory $(pwd)

printTitle "Setup project"
./scripts/setup.sh

printTitle "Install project dependencies"
opam update
opam install --deps-only --with-test --update-invariant --locked --no-depexts -y .

printTitle "Build project"
opam exec -- dune build --root .

printTitle "Check formatting"
make format

printTitle "Setup test"
opam exec -- dune exec --root . pool/run/run.exe migrate.root
opam exec -- dune exec --root . pool/run/run.exe seed.root.clean
opam exec -- dune exec --root . pool/run/run.exe migrate.tenant
opam exec -- dune exec --root . pool/run/run.exe seed.tenant.clean

printTitle "Execute tests"
opam exec -- make test

printTitle "Build script finished"
