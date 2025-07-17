SHELL = bash

.PHONY: dev
.SILENT:
.ONESHELL:
dev:: ## Run the Sihl app, watch files and restart on change
	sigint_handler()
	{
	kill -9 $$(lsof -ti tcp:3000) 2>/dev/null || true
	exit
	}
	trap sigint_handler SIGINT
	yarn build --log-level warn
	dune build
	watchexec \
		--restart \
		--ignore "_build/**" \
		--ignore "public/**" \
		--ignore "logs/**" \
		--ignore "Makefile" \
		--ignore ".vscode/**" \
		--ignore ".devcontainer/**" \
		--ignore ".git/**" \
		--ignore ".DS_Store" \
		--ignore "node_modules/**" \
		--ignore "resources/**" \
		--ignore "*.ctf" \
		--shell=bash \
		--exts ml,mli,re,rei,js,ts,tsx,jsx,json,yaml,yml,toml \
		-- 'dune build && SIHL_ENV=development ./_build/default/pool/run/run.exe server'

include ./Makefile
