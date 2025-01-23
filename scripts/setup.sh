#!/bin/sh

# immediately when a command fails and print each command
set -ex

# get newest opam packages
opam remote remove --all default
opam remote add default https://opam.ocaml.org

repo_oxi=https://github.com/oxidizing
repo_uzh=https://github.com/uzh
repo_sihl=https://github.com/mabiede/sihl.git#maintenance-2501

# pin Sihl repos to ensure edge is available
opam pin add -yn sihl $repo_sihl
opam pin add -yn sihl-email $repo_sihl
opam pin add -yn sihl-storage $repo_sihl
opam pin add -yn conformist $repo_oxi/conformist.git#aa7b95d1f39215cdaab8cf96d765d63e41d5f8a6
opam pin add -yn letters $repo_oxi/letters.git#0.4.0

# TODO: remove pins when the packages are released
opam pin add -yn opium https://github.com/mabiede/opium.git#upgrade-packages
opam pin add -yn rock https://github.com/mabiede/opium.git#upgrade-packages

# pin custom libraries
opam pin add -yn guardian $repo_uzh/guardian.git#287bab9baf9b346efe47999e8b7f456bfefe8c13
opam pin add -yn canary $repo_uzh/canary.git#0.0.3

opam install . --working-dir --with-test --with-doc --deps-only

eval $(opam env)
