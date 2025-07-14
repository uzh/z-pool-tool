#!/bin/sh

# this script is used to build the project
# it is setup to run in a ocaml/opam:debian-ocaml based Docker image

# immediately when a command fails and print each command
set -ex

printTitle() {
  echo -e "\e[1;33m~~~ $1: ~~~\e[0m"
}

printTitle "Installing system dependencies"
sudo apt-get update -q
sudo apt-get install -yqq --no-install-recommends \
  m4 \
  gcc \
  libev-dev \
  libgmp-dev \
  pkg-config \
  wget \
  curl \
  build-essential \
  make \
  perl \
  cmake \
  git

# cleanup installations
sudo apt-get autoremove -y
sudo apt-get clean all

printTitle "Installing OpenSSL and MariaDB"
sudo ./scripts/install.sh

printTitle "Loading environment variables"
# Source the environment setup created by install.sh
if [ -f /tmp/env_setup.sh ]; then
  . /tmp/env_setup.sh
  export PATH="$PATH"
  export LD_LIBRARY_PATH="$LD_LIBRARY_PATH"
  export PKG_CONFIG_PATH="$PKG_CONFIG_PATH"
  export OPENSSL_DIR="$OPENSSL_DIR"
  export OPENSSL_INCLUDE_DIR="$OPENSSL_INCLUDE_DIR"
  export OPENSSL_LIB_DIR="$OPENSSL_LIB_DIR"
  export MARIADB_CONFIG="$MARIADB_CONFIG"
fi

printTitle "Mark source dir as save"
git config --global --add safe.directory $(pwd)

printTitle "Setup project"
./scripts/setup.sh

printTitle "Install project dependencies"
opam install --deps-only --with-test --update-invariant --locked --no-depexts -y .

printTitle "Build project"
opam exec -- dune build --root .

printTitle "Check formatting"
make format

printTitle "Setup test"
opam config exec -- dune exec --root . pool/run/run.exe migrate.root
opam config exec -- dune exec --root . pool/run/run.exe seed.root.clean
opam config exec -- dune exec --root . pool/run/run.exe migrate.tenant
opam config exec -- dune exec --root . pool/run/run.exe seed.tenant.clean

printTitle "Execute tests"
opam config exec -- make test

printTitle "Build script finished"
