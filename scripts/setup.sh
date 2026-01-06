#!/bin/sh

# immediately when a command fails and print each command
set -ex

repo_oxi=https://github.com/oxidizing
repo_uzh=https://github.com/uzh
repo_sihl=$repo_uzh/sihl.git#6c3c4040413294155cda3a363edf0ff3c7e638b8
repo_opium=https://github.com/rgrinberg/opium.git#da1b22e2ee31889fea68a943b4a6358a633df0c6

# pin Sihl repos to ensure edge is available
opam pin add -yn sihl $repo_sihl
opam pin add -yn sihl-email $repo_sihl
opam pin add -yn sihl-storage $repo_sihl
opam pin add -yn conformist $repo_oxi/conformist.git#aa7b95d1f39215cdaab8cf96d765d63e41d5f8a6
opam pin add -yn letters https://github.com/mabiede/letters.git#bug/connection-close-wait
opam pin add -yn opium $repo_opium
opam pin add -yn rock $repo_opium

# pin custom libraries
opam pin add -yn guardian $repo_uzh/guardian.git#287bab9baf9b346efe47999e8b7f456bfefe8c13
opam pin add -yn canary $repo_uzh/canary.git#0.0.3

eval $(opam env)
