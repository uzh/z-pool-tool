#!/bin/sh

project_dir=/app

# Install system dependencies
sudo apk add openssl-libs-static mariadb-static sqlite-static zlib-static

# Mark source dir as save
git config --global --add safe.directory $project_dir

# Setup project
$project_dir/scripts/setup.sh

# Install project dependencies
opam install --deps-only --with-test -y .
eval $(opam env)

# Install project dependencies
opam exec -- dune build --root .
