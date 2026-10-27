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
	opam install --deps-only --with-test --with-doc --with-dev-setup --working-dir --locked -y .

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
	yarn up --recursive '*'
	opam update

.PHONY: sihl
sihl: all ## Run the produced executable
	SIHL_ENV=development opam exec -- dune exec --root . pool/run/run.exe $(ARGS)

.PHONY: test
test: ## Run the all tests
	SIHL_ENV=test QUEUE_FORCE_ASYNC=true opam exec -- dune exec --root . pool/run/run.exe seed.tenant.test
	SIHL_ENV=test QUEUE_FORCE_ASYNC=true opam exec -- dune build --root . @runtest

.PHONY: test-trace
test-trace: ## Run the tests with memtrace (and SANITIZER=tsan|asan|lsan), report in logs/trace
	SANITIZER=$(SANITIZER) ./scripts/trace.sh test

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
	./scripts/dev.sh

.PHONY: dev-trace
dev-trace: ## Run server and worker with memtrace (and SANITIZER=tsan|asan|lsan), report in logs/trace
	SANITIZER=$(SANITIZER) DURATION=$(DURATION) ./scripts/trace.sh dev

.PHONY: worker-trace
worker-trace: ## Run the worker under email load with memtrace (and SANITIZER=...), report in logs/trace
	SANITIZER=$(SANITIZER) DURATION=$(DURATION) JOBS=$(JOBS) WAVE=$(WAVE) ./scripts/trace.sh worker

.PHONY: utop
utop: ## Run a REPL and link with the project's libraries
	opam exec -- dune utop --root . lib -- -implicit-bindings

.PHONY: db
db: ## Starts the database using docker-compose
	docker-compose -f docker/docker-compose.dev.yml up -d

.PHONY: db_down
db_down: ## Removes the database using docker-compose
	docker-compose -f docker/docker-compose.dev.yml down

.PHONY: version version-patch version-minor version-major
version:
	@echo "Usage: make version-{patch|minor|major}"

version-patch:
	./scripts/bump-version.sh --patch

version-minor:
	./scripts/bump-version.sh --minor

version-major:
	./scripts/bump-version.sh --major
