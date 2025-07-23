SHELL = zsh
EXCLUDE_PATTERN = (_build|public|logs|Makefile|\.vscode|\.devcontainer|\.git|\.DS_Store|node_modules|resources|.*\.ctf)
PORT = 3000

ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
ARGS := $(subst :,\:,$(ARGS))
$(eval $(ARGS):;@:)

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
	if /workspace/.devcontainer/test-inotify.sh; then
		USE_INOTIFY=true
	else
		echo "⚠ inotify not supported (likely 9p/Windows filesystem)"
		USE_INOTIFY=false
		touch /tmp/last_build_time
	fi
	while true; do
		if (cd /workspace && dune build); then
			SIHL_ENV=development /workspace/_build/default/pool/run/run.exe server &
		fi
		if [ "$$USE_INOTIFY" = "true" ]; then
			echo "Watching for file changes (inotify)..."
			inotifywait -e modify -e move -e create -e delete -e attrib -r /workspace --exclude "$(EXCLUDE_PATTERN)" -qq
		else
			echo "Watching for file changes (polling every 2s)..."
			while true; do
				sleep 2
				RESULT=$$(find /workspace \( -name "*.ml" -o -name "*.mli" -o -name "dune*" -o -name "*.js" -o -name "*.scss" \) -printf "%T@ %p\n" 2>/dev/null | sort -n | tail -1)
				LAST_CHANGE=$$(echo "$$RESULT" | awk '{print int($$1)}')
				LAST_FILE=$$(echo "$$RESULT" | awk '{print $$2}')
				LAST_BUILD=$$(stat -c %Y /tmp/last_build_time 2>/dev/null || echo 0)
				if [ "$$LAST_CHANGE" -gt "$$LAST_BUILD" ]; then
					echo "File change detected: $$LAST_FILE"
					touch /tmp/last_build_time
					break
				fi
			done
			echo "File change detected: $$LAST_CHANGE"
		fi
		kill_process_on_port $(PORT)
		echo
	done

# explicitly forward sihl commands (issue on Docker for Windows)
.PHONY: sihl
sihl:
	@cd /workspace && $(MAKE) sihl $(ARGS)

# Used for checking inotify on Docker for Windows mounted volumes
.PHONY: test-filesystem
test-filesystem:
	@echo "Testing inotify support on current filesystem..."
	@if /workspace/.devcontainer/test-inotify.sh; then \
		echo "✓ inotify IS supported"; \
	else \
		echo "⚠ inotify NOT supported (likely 9p/Windows filesystem)"; \
	fi

# Forward all other targets to parent directory's Makefile
%:
	@cd /workspace && $(MAKE) $@
