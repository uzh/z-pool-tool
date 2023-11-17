#!/bin/sh

# immediately when a command fails and print each command
set -ex

# get newest opam packages
opam remote remove --all default
opam remote add default https://opam.ocaml.org

repo_oxi=https://github.com/oxidizing
repo_uzh=https://github.com/uzh
repo_sihl=$repo_oxi/sihl.git#e0b304a4ebd3cb2ac0d21e0f6d151b9db301f9eb

# pin Sihl repos to ensure edge is available
opam pin add -yn sihl https://github.com/leostera/sihl.git#ef4c89fd3d72fdf51caf30082cc55b7e7430b8f2
opam pin add -yn sihl-cache $repo_sihl
opam pin add -yn sihl-email $repo_sihl
opam pin add -yn sihl-queue $repo_sihl
opam pin add -yn sihl-storage $repo_sihl
opam pin add -yn sihl-token $repo_sihl
opam pin add -yn sihl-user $repo_sihl
opam pin add -yn conformist $repo_oxi/conformist.git#aa7b95d1f39215cdaab8cf96d765d63e41d5f8a6
opam pin add -yn letters $repo_oxi/letters.git#0.3.3

# pin custom libraries
opam pin add -yn canary $repo_uzh/canary.git#02cf40e029268560e160ca032850426e387aa598
opam pin add -yn guardian $repo_uzh/guardian.git#f144f2c12459e13365c8d6adc481556223dd87f9

opam pin add -yn pool .
OPAMSOLVERTIMEOUT=180 opam depext -y pool

eval $(opam env)
