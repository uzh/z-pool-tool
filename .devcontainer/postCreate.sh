#!/bin/sh

# immediately when a command fails and print each command
set -ex

# customize zsh (Oh My Zsh and ~/.zshrc are installed by the common-utils feature)
if [ -d ~/.oh-my-zsh ]; then
  sed -i "/^plugins=/c\plugins=(git dotenv)" ~/.zshrc
fi

# configure git identity from .devcontainer/.env (committer and tools read the config)
if [ -n "${GIT_AUTHOR_NAME:-}" ]; then
  git config --global user.name "$GIT_AUTHOR_NAME"
fi
if [ -n "${GIT_AUTHOR_EMAIL:-}" ]; then
  git config --global user.email "$GIT_AUTHOR_EMAIL"
fi

# use HTTPS + PAT for SSH-style GitHub remotes (no ssh-agent in the container)
if [ -n "${GIT_REMOTE_PAT:-}" ]; then
  git config --global --replace-all url."https://github.com/".insteadOf "git@github.com:"
  git config --global --add url."https://github.com/".insteadOf "ssh://git@github.com/"
  # read the PAT from the environment at call time, so it's never written to disk
  git config --global --replace-all credential.https://github.com.helper ""
  git config --global --add credential.https://github.com.helper \
    '!f() { test "$1" = get && echo "username=${GIT_REMOTE_USERNAME}" && echo "password=${GIT_REMOTE_PAT}"; }; f'
fi

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
