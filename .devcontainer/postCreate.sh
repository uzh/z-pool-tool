# ocaml/opam post create script

sudo chown -R opam: _build
sudo chown -R opam: node_modules

opam init -a --shell=zsh

# ensure all system dependencies are installed
opam pin add -yn pool .
opam depext -y pool

# install opam packages used for vscode ocaml platform package
# e.g. when developing with emax, add also: utop merlin ocamlformat
make deps
