#!/bin/sh

# immediately when a command fails and print each command
set -ex

opam init -a --shell=zsh

# update default ocaml remote - make sure that opam finds latest package versions
# (e.g. otherwise alcotest latest version is 1.1.0 instead of 1.2.1)
opam remote remove --all default
opam repository add default --all-switches --set-default https://opam.ocaml.org

# Check for git line ending issues and provide information
if [ -n "$(git status --porcelain)" ]; then
  echo "⚠️  Git detected file changes"
  echo "    (likely line ending issue, see .devcontainer/README.md#error-with-line-endings)"
else
  echo "✓ No git file changes detected"
fi

opam install ocaml-lsp-server
opam install --working-dir --with-test --with-doc --deps-only --update-invariant -y .

# install yarn packages
yarn
