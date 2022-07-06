# Development with DevContainer (VS Code)

[Back to main README](../README.md)

When using a DevContainer, the `opam` packages and database files are mounted as volumes into the DevContainer.

- `.devcontainer/data/db-root/`: development mariadb data (root)
- `.devcontainer/data/db-tenant/`: development mariadb data (tenants)
- Docker volume `opam`: opam packages
- Docker volume `build`: cached build directory to speed up development

## Requirements

This project is setup to run in a DevContainer. Ensure requirements to run in a DevContainer:

1. [Git](https://gitlab.uzh.ch/econ/it/engineering-public/-/wikis/git) installed
1. Source Tree for GitLab setup (see [Install user interface](https://gitlab.uzh.ch/econ/it/engineering-public/-/wikis/git#install-user-interface) and [Configure GitLab for Source Tree](https://gitlab.uzh.ch/econ/it/engineering-public/-/wikis/git#configure-gitlab-for-source-tree))
1. [Docker](/Technologies/Docker) installed
1. [Visual Studio Code](https://code.visualstudio.com/) (VS Code) installed
1. VS Code Extension [Remote Container](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) installed
1. Set environment variable `ECON_SMTP_APP_PASSWORD` to our SMTP account password (see 1Password) on your local machine.

Your SSH folder and Git config gets mapped to the container. You should be able to use SSH and Git inside the
container. Please ensure `~/.gitconfig` doesn't contain absolute paths (you may use the `~` profile prefix, i.e.
`excludesfile = ~/.gitignore_global`). **Please note:** You probably have to update Sourcetree settings. In its
settings "General" tab uncheck "Allow Sourcetree to modify your global Mercurial and Git configuration files".

## Start

Click on the icon similar to "><" in the bottom left corner and select `Remote-Containers: Reopen in Container`.
If any changes were made to files in `.devcontainer` folder the Container should be rebuilt (`Remote-Containers: Rebuild Container`)

> **NOTE**: When the setup is fully installed, select `View` -> `Command Palette...` and run the command `OCaml: Restart Language Server`

## Running Dev Container

- The Makefile used in the DevContainer is located in the `.devcontainer` folder
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
1. Run migrations and seeds for the test databases
    ```
    opam config exec -- dune exec --root . pool/run/run.exe migrate.root
    opam config exec -- dune exec --root . pool/run/run.exe seed.root.clean
    opam config exec -- dune exec --root . pool/run/run.exe migrate.tenant
    opam config exec -- dune exec --root . pool/run/run.exe seed.tenant.clean
    ```
1. Run `make test`

## Database View

There is an `Adminer` container added to the development package. To be able to use it, follow these few steps:

1. Uncomment its line in the `.devcontainer/devcontainer.json` under `runServices`
1. Use `Remote-Containers: Rebuild Container` that it will also create and startup the `Adminer` container
1. Open your web browser and open `localhost:8080`
