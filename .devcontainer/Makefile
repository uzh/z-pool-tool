include ./Makefile

SHELL = bash

.PHONY: dev
.SILENT:
.ONESHELL:
dev:: ## Run the Sihl app, watch files and restart on change
	sigint_handler()
	{
	kill -9 $$(lsof -ti tcp:3000)
	exit
	}
	trap sigint_handler SIGINT
	yarn build --log-level warn
	while true; do
	dune build
	if [ $$? -eq 0 ]
	then
		SIHL_ENV=development ./_build/default/pool/run/run.exe server &
	fi
	echo
	inotifywait -e modify -e move -e create -e delete -e attrib -r `pwd` --exclude "(_build|public|logs|Makefile|.vscode|.devcontainer|.git|.DS_Store|node_modules|resources|*.ctf)" -qq
	kill -9 $$(lsof -ti tcp:3000)
	echo
	done
