#!/bin/sh

# immediately when a command fails and print each command
set -ex

sudo chown -R opam: _build
sudo chown -R opam: node_modules

git config --global --add safe.directory /workspace

# Configure git for cross-platform compatibility (OS independent)
git config core.autocrlf false
git config core.eol lf
git config core.filemode false

# Check for git line ending issues and provide manual commands
if [ ! -z "$(git status --porcelain)" ]; then
  echo "⚠️  Git detected file changes (likely line ending issues)"
  echo "To fix line ending issues manually, run:"
  echo "  git add --renormalize ."
  echo ""
  echo "If you want to reset all changes (WARNING: will lose uncommitted changes):"
  echo "  git rm --cached -r ."
  echo "  git reset --hard"
  echo ""
fi

opam init -a --shell=zsh

/workspace/scripts/setup.sh

opam install --working-dir --with-test --with-doc --deps-only --update-invariant -y .

# install opam packages used for vscode ocaml platform package
# e.g. when developing with emax, add also: utop merlin ocamlformat
make deps

# install yarn packages
yarn
