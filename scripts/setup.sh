#!/bin/sh

# get newest opam packages
opam remote remove --all default
opam remote add default https://opam.ocaml.org

# ensure all system dependencies are installed
repo=https://github.com/oxidizing/sihl.git
opam pin add -yn sihl $repo
opam pin add -yn sihl-cache $repo
opam pin add -yn sihl-email $repo
opam pin add -yn sihl-queue $repo
opam pin add -yn sihl-storage $repo
opam pin add -yn sihl-token $repo
opam pin add -yn sihl-user $repo

# pin custom libraries hosted on the UZH gitlab
opam pin add -yn canary https://github.com/uzh/canary.git
opam pin add -yn conformist https://github.com/oxidizing/conformist.git
opam pin add -ywn guardian https://github.com/uzh/guardian.git
opam pin add -yn letters https://github.com/oxidizing/letters.git

opam pin add -yn pool .
OPAMSOLVERTIMEOUT=180 opam depext -y pool
