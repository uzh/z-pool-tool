SHELL = zsh

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
	while true; do
	dune build
	if [ $$? -eq 0 ]
	then
		SIHL_ENV=development ./_build/default/pool/run/run.exe server &
	fi
	echo
	inotifywait -e modify -e move -e create -e delete -e attrib -r `pwd` --exclude "(_build|public|logs|Makefile|\.vscode|\.devcontainer|\.git|\.DS_Store|node_modules|resources|.*\.ctf)" -qq
	PID=$$(lsof -ti tcp:3000)
	if [ -n "$$PID" ]; then
		kill -0 $$PID 2>/dev/null && kill -9 $$PID || true
	fi
	echo
	done

# Forward all other targets to parent directory's Makefile
%:
	@cd /workspace && $(MAKE) $@
