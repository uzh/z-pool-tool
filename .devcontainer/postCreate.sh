#!/bin/sh

# immediately when a command fails and print each command
set -ex

sudo chown -R opam: _build
sudo chown -R opam: node_modules

opam init -a --shell=zsh

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
