# Development with DevContainer (VS Code)

[Back to main README](../README.md)

When using a DevContainer, the `opam` packages and database files are mounted as volumes into the DevContainer.

- Docker volume `db-root`: development mariadb data (root)
- Docker volume `db-tenant`: development mariadb data (tenants)
- Docker volume `opam`: opam packages
- Docker volume `build`: cached build directory to speed up development

## Requirements

This project is setup to run in a DevContainer. Ensure requirements to run in a DevContainer:

1. [Git](https://gitlab.uzh.ch/econ/it/engineering-public/-/wikis/git) installed
1. Source Tree for GitLab setup (see [Install user interface](https://gitlab.uzh.ch/econ/it/engineering-public/-/wikis/git#install-user-interface) and [Configure GitLab for Source Tree](https://gitlab.uzh.ch/econ/it/engineering-public/-/wikis/git#configure-gitlab-for-source-tree))
1. [Docker](/Technologies/Docker) installed
1. [Visual Studio Code](https://code.visualstudio.com/) (VS Code) installed
1. VS Code Extension [Remote Container](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) installed

Neither `~/.gitconfig` nor `~/.ssh` are mounted by the compose setup. VS Code Dev Containers copies your Git config on
its own; please ensure `~/.gitconfig` doesn't contain absolute paths (you may use the `~` profile prefix, i.e.
`excludesfile = ~/.gitignore_global`). **Please note:** You probably have to update Sourcetree settings. In its
settings "General" tab uncheck "Allow Sourcetree to modify your global Mercurial and Git configuration files".

The forwarded SSH agent is cleared (`remoteEnv.SSH_AUTH_SOCK` in `devcontainer.json`), so neither you nor an agent
running in the container can use your SSH keys. Remote access goes over HTTPS with the PAT from `.devcontainer/.env`
(`postCreate.sh` rewrites `git@github.com:` remotes to HTTPS), so scope that PAT to what the container may do.
Clearing the variable is not a hard boundary: VS Code still creates the agent socket in `/tmp/vscode-ssh-auth-*.sock`,
and a process that looks for it can use it while your host agent has keys loaded.

## Start

> [!important]
> GitHub Actions only build the `linux/amd64` image, use the build command on step 2 for `linux/arm64`

1. If needed, edit `.devcontainer/docker-compose.override.yml` to suit your needs.
1. Copy `.devcontainer/.env.example` to `.devcontainer/.env` (git ignored) and fill in your values (git identity, signing key, PAT). Only `.env` is loaded into the container; the container won't start without it.
1. If not logged in to GitHub's container registry, the base image can be built with the following command: `docker build -f ./docker/Dockerfile.build -t ghcr.io/uzh/z-pool-tool/ocaml:9-5.5-2.6 .`
1. Mark devcontainer workspace as safe: `git config --global --add safe.directory /workspace`
1. Click on the icon similar to "><" in the bottom left corner and select `Remote-Containers: Reopen in Container`.
    - If any changes were made to files in `.devcontainer` folder the Container should be rebuilt (`Remote-Containers: Rebuild Container`)

> **NOTE**: When the setup is fully installed, select `View` -> `Command Palette...` and run the command `OCaml: Restart Language Server`

## Image and Features

The image is based on `ghcr.io/uzh/z-pool-tool/ocaml:9-5.5-2.6` (built from `docker/Dockerfile.build`, RHEL 9 UBI).
Tooling on top of it is installed with [devcontainer features](https://containers.dev/features) (see `devcontainer.json`):

- `common-utils`: common CLI utilities (via `dnf`), zsh, Oh My Zsh and passwordless `sudo` for the `opam` user
- `node`: Node.js and yarn (via corepack)

`make dev` (`scripts/dev.sh`) uses dune's watch mode for rebuilds, so no extra file watcher is installed
(dune 3 has a built-in inotify backend; the dune docs still mention `inotifywait`/`fswatch`, which is outdated).
Watch mode needs inotify, which 9p mounts (a Windows repository under `/mnt/c`) don't provide. There `make dev` polls the
sources every 2s instead; set `DEV_POLL=1` in `.devcontainer/.env` to force it if rebuilds don't trigger. Cloning the
repository inside the WSL file system is faster and keeps watch mode.

## Running Dev Container

- Ports from outside (e.g. localhost)
  - webserver
    - root `3016`
    - tenant `3017`
  - database
    - root`3316`
    - tenant `3317`

## Running Tests with Dev Container

1. Create a `.env.test` file
1. Add the following variables to it
    - `DATABASE_URL=mariadb://root@database-root:3306/test`
    - `MYSQL_DATABASE=test_econ`
1. Prepare test database (migration/seeds) and run the tests
    - `make test-clean`

## Database View

There is an `Adminer` container added to the development package. To be able to use it, follow these few steps:

1. Uncomment its line in the `.devcontainer/devcontainer.json` under `runServices`
1. Use `Remote-Containers: Rebuild Container` that it will also create and startup the `Adminer` container
1. Open your web browser and open `localhost:8080`

## Troubleshooting

### "Go-to-definition" opens Capitalized File

This behaviour is related to the ocaml-lsp-server (as a first impression), when using a ritght click on a function name and selecting "Go to Definition" it jumps to the capitalized filename instead of the correct and existing (in our case lowercased) filename.

#### Solution

This issue isn't related to ocaml-lsp-server it's depending on the underliing format of your data volume and it's ability to handle case sensitivity (link for [MacOS](https://support.apple.com/en-gb/guide/disk-utility/dsku19ed921c/mac) and [Windows WSL](https://learn.microsoft.com/en-us/windows/wsl/case-sensitivity)).

##### MacOS

- Create a "Case-Sensitive" volume
- Clone the project on the new volume
- Delete all currently existing Container/Images/Volumes related to the pool tool
- Open the newly cloned project in VSCode
- Use "Reopen in Container"

### Error on devcontainer Configuration during startup

It's most likely a git related issue.

#### Solution

- Open a new Terminal within the container
- check output of `git status`
- run `git config --global --add safe.directory /workspace`
- run `./.devcontainer/postCreate.sh`

[test](#error-with-line-endings)

### Error with line endings

**NOTE:** Most likely with Docker on Windows

Git detected file changes (likely line ending issues).
If you're experiencing line ending issues, you can configure git manually:

```zsh
git config core.autocrlf false    # Disable automatic line ending conversion"
git config core.eol lf            # Use LF (Unix) line endings"
git config core.filemode false    # Ignore file mode changes"
```

To fix existing line ending issues (renormalize all files)

```zsh
git add --renormalize .
```

If you want to reset all changes (**WARNING: will lose uncommitted changes**)

```zsh
git rm --cached -r .
git reset --hard
```
