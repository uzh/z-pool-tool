# ocaml/opam post create script

sudo chown -R opam: _build
sudo chown -R opam: node_modules

opam init -a --shell=zsh

# get newest opam packages
opam remote remove --all default
opam remote add default https://opam.ocaml.org

# TODO: remove pins when Sihl version is released
opam pin add --kind=git sihl git+https://github.com/oxidizing/sihl.git
opam pin add --kind=git sihl-token git+https://github.com/oxidizing/sihl.git
opam pin add --kind=git sihl-user git+https://github.com/oxidizing/sihl.git

# ensure all system dependencies are installed
opam pin add -yn pool .
opam depext -y pool

# install opam packages used for vscode ocaml platform package
# e.g. when developing with emax, add also: utop merlin ocamlformat
make deps
