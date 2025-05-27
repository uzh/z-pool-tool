.DEFAULT_GOAL := all

ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
ARGS := $(subst :,\:,$(ARGS))
$(eval $(ARGS):;@:)

.PHONY: all
all:
	yarn install
	opam exec -- dune build --root . @install

.PHONY: deps
deps: ## Install development dependencies
	opam install -y dune-release ocamlformat ocaml-lsp-server
	opam install --deps-only --with-test --with-doc --working-dir --locked -y .

.PHONY: lock
lock: ## Generate a lock file
	opam lock .

.PHONY: build
build:
	yarn build --log-level warn
	opam exec -- dune build --root .

.PHONY: build-watch
build-watch:
	yarn build --log-level warn
	opam exec -- dune build --root . --watch

.PHONY: install
install: all ## Install the packages on the system
	opam exec -- dune install --root .

.PHONY: update
update:
	yarn upgrade
	opam update

.PHONY: sihl
sihl: all ## Run the produced executable
	SIHL_ENV=development opam exec -- dune exec --root . pool/run/run.exe $(ARGS)

.PHONY: test
test: ## Run the all tests
	SIHL_ENV=test QUEUE_FORCE_ASYNC=true opam exec -- dune exec --root . pool/run/run.exe seed.tenant.test
	SIHL_ENV=test QUEUE_FORCE_ASYNC=true opam exec -- dune build --root . @runtest

.PHONY: test-migrate
test-migrate: ## Run the all tests
	SIHL_ENV=test opam exec -- dune exec --root . pool/run/run.exe migrate.root
	SIHL_ENV=test opam exec -- dune exec --root . pool/run/run.exe seed.root.clean
	SIHL_ENV=test opam exec -- dune exec --root . pool/run/run.exe migrate.tenant

.PHONY: test-clean
test-clean: | test-migrate test

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean --root .

.PHONY: doc
doc: ## Generate odoc documentation
	opam exec -- dune build --root . @doc

.PHONY: format
format: ## Format the codebase with ocamlformat
	opam exec -- dune build --root . --auto-promote @fmt

.PHONY: dev
.SILENT:
.ONESHELL:
dev:: ## Run the Sihl app, watch files and restart on change
	sigint_handler()
	{
	[[ $$(jobs -pr) = "" ]] || kill $$(jobs -pr)
	exit
	}
	trap sigint_handler SIGINT
	yarn build --log-level warn
	while true; do
	dune build
	if [ $$? -eq 0 ]
	then
		SIHL_ENV=development ./_build/default/pool/run/run.exe server &
		PID=$$!
	fi
	echo
	inotifywait -e modify -e move -e create -e delete -e attrib -r `pwd` --exclude "(_build|public|.parcel-cache|logs|Makefile|.vscode|.devcontainer|.git|.DS_Store|node_modules|resources)" -qq
	[[ $$(jobs -pr) = "" ]] || kill $$(jobs -pr)
	echo
	done

.PHONY: utop
utop: ## Run a REPL and link with the project's libraries
	opam exec -- dune utop --root . lib -- -implicit-bindings

.PHONY: db
db: ## Starts the database using docker-compose
	docker-compose -f docker/docker-compose.dev.yml up -d

.PHONY: db_down
db_down: ## Removes the database using docker-compose
	docker-compose -f docker/docker-compose.dev.yml down
