SHELL = zsh
EXCLUDE_PATTERN = (_build|public|logs|Makefile|\.vscode|\.devcontainer|\.git|\.DS_Store|node_modules|resources|.*\.ctf)
PORT = 3000

.PHONY: dev
.SILENT:
.ONESHELL:
dev:: ## Run the Sihl app, watch files and restart on change
	kill_process_on_port() {
		local port=$$1
		PID=$$(lsof -ti tcp:$$port)
		if [ -n "$$PID" ]; then
			kill -0 $$PID 2>/dev/null && kill -9 $$PID || true
		fi
	}
	sigint_handler()
	{
		kill_process_on_port $(PORT)
		exit
	}
	trap sigint_handler SIGINT
	yarn build --log-level warn
	while true; do
	if dune build; then
		SIHL_ENV=development ./_build/default/pool/run/run.exe server &
	fi
	echo
	inotifywait -e modify -e move -e create -e delete -e attrib -r $(pwd) --exclude "$(EXCLUDE_PATTERN)" -qq
	kill_process_on_port $(PORT)
	echo
	done

# Forward all other targets to parent directory's Makefile
%:
	@cd /workspace && $(MAKE) $@
