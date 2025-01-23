#!/bin/sh

# immediately when a command fails and print each command
set -ex

sudo chown -R opam: _build
sudo chown -R opam: node_modules

opam init -a --shell=zsh

/workspace/scripts/setup.sh

opam install . --working-dir --with-test --with-doc --deps-only

# install opam packages used for vscode ocaml platform package
# e.g. when developing with emax, add also: utop merlin ocamlformat
make deps

# install yarn packages
yarn
