#!/bin/sh

# immediately when a command fails and print each command
set -ex

# get newest opam packages
opam remote remove --all default
opam remote add default https://opam.ocaml.org

repo_oxi=https://github.com/oxidizing
repo_uzh=https://github.com/uzh
repo_sihl=$repo_oxi/sihl.git#1070575d3699319430aa589749b364a580ba40cf

# pin Sihl repos to ensure edge is available
opam pin add -yn sihl $repo_sihl
opam pin add -yn sihl-cache $repo_sihl
opam pin add -yn sihl-email $repo_sihl
opam pin add -yn sihl-queue $repo_sihl
opam pin add -yn sihl-storage $repo_sihl
opam pin add -yn sihl-token $repo_sihl
opam pin add -yn sihl-user $repo_sihl
opam pin add -yn conformist $repo_oxi/conformist.git
opam pin add -yn letters $repo_oxi/letters.git

# pin custom libraries
opam pin add -yn canary $repo_uzh/canary.git
opam pin add -yn guardian $repo_uzh/guardian.git

opam pin add -yn pool .
OPAMSOLVERTIMEOUT=180 opam depext -y pool

eval $(opam env)
