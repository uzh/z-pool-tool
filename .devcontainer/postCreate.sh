# ocaml/opam post create script

sudo chown -R opam: _build
sudo chown -R opam: node_modules

opam init -a --shell=zsh

# get newest opam packages
opam remote remove --all default
opam remote add default https://opam.ocaml.org

# ensure all system dependencies are installed
opam pin add -yn sihl git://github.com/oxidizing/sihl.git
opam pin add -yn sihl-email git://github.com/oxidizing/sihl.git
opam pin add -yn sihl-queue git://github.com/oxidizing/sihl.git
opam pin add -yn sihl-storage git://github.com/oxidizing/sihl.git
opam pin add -yn sihl-token git://github.com/oxidizing/sihl.git
opam pin add -yn sihl-user git://github.com/oxidizing/sihl.git

# pin custom libraries hosted on the UZH gitlab
opam pin add -yn canary https://github.com/chrismamo1/canary.git

opam pin add -yn pool .
opam depext -y pool

# install opam packages used for vscode ocaml platform package
# e.g. when developing with emax, add also: utop merlin ocamlformat
make deps
