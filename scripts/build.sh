#!/bin/sh

printTitle () {
  echo -e "\e[1;43m  $1:  \e[0m"
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
