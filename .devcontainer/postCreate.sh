#!/bin/sh

# immediately when a command fails and print each command
set -ex

sudo chown -R opam: _build
sudo chown -R opam: node_modules

opam init -a --shell=zsh

# Check for git line ending issues and provide troubleshooting information
if [ ! -z "$(git status --porcelain)" ]; then
  echo "⚠️  Git detected file changes (likely line ending issues)"
  echo ""
  echo "TROUBLESHOOTING: Line ending issues"
  echo "==================================="
  echo ""
  echo "If you're experiencing line ending issues, you can configure git manually:"
  echo "  git config core.autocrlf false    # Disable automatic line ending conversion"
  echo "  git config core.eol lf            # Use LF (Unix) line endings"
  echo "  git config core.filemode false    # Ignore file mode changes"
  echo ""
  echo "To fix existing line ending issues:"
  echo "  git add --renormalize .           # Renormalize all files"
  echo ""
  echo "If you want to reset all changes (WARNING: will lose uncommitted changes):"
  echo "  git rm --cached -r ."
  echo "  git reset --hard"
  echo ""
else
  echo "✓ No git file changes detected"
fi

/workspace/scripts/setup.sh

opam install ocaml-lsp-server
opam install --working-dir --with-test --with-doc --deps-only --update-invariant -y .

# install yarn packages
yarn
