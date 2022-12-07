#!/bin/sh

# immediately when a command fails and print each command
set -ex

printTitle () {
  echo -e "\e[1;33m~~~ $1: ~~~\e[0m"
}

printTitle "Installing system dependencies"
sudo apk add openssl-libs-static mariadb-static sqlite-static zlib-static

printTitle "Mark source dir as save"
git config --global --add safe.directory $(pwd)

printTitle "Setup project"
./scripts/setup.sh

printTitle "Install project dependencies"
opam install --deps-only --with-test -y .
eval $(opam env)

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
