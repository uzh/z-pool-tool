#!/bin/sh

# immediately when a command fails and print each command
set -ex

# get newest opam packages
opam remote remove --all default
opam remote add default https://opam.ocaml.org

repo_oxi=https://github.com/oxidizing
repo_uzh=https://github.com/uzh
repo_sihl=$repo_oxi/sihl.git#d1250a2ff807dd1731f6ccd6250061215cdf73bf

# pin Sihl repos to ensure edge is available
opam pin add -yn sihl $repo_sihl
opam pin add -yn sihl-cache $repo_sihl
opam pin add -yn sihl-email $repo_sihl
opam pin add -yn sihl-queue $repo_sihl
opam pin add -yn sihl-storage $repo_sihl
opam pin add -yn sihl-token $repo_sihl
opam pin add -yn sihl-user $repo_sihl
opam pin add -yn conformist $repo_oxi/conformist.git#aa7b95d1f39215cdaab8cf96d765d63e41d5f8a6
opam pin add -yn letters $repo_oxi/letters.git#0.3.3

# pin custom libraries
opam pin add -yn guardian $repo_uzh/guardian.git#ed36bc1251fd336f8b2fc848fc3122432ad23873
opam pin add -yn canary $repo_uzh/canary.git#62f93e9194e56ee70d8e95935405708a4322db88

opam pin add -yn pool .
OPAMSOLVERTIMEOUT=180 opam depext -y pool

eval $(opam env)
