# ocaml/opam post create script

sudo chown -R opam: _build
sudo chown -R opam: node_modules

opam init -a --shell=zsh

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
opam pin add -yn ocaml_authorize https://$GITLAB_USER:$GITLAB_TOKEN@gitlab.uzh.ch/econ/it/framework/ocaml/permissions.git

opam pin add -yn pool .
opam depext -y pool

# install opam packages used for vscode ocaml platform package
# e.g. when developing with emax, add also: utop merlin ocamlformat
make deps
