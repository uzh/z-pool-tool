# Pool Tool

## Links

Access using following URL:

- production: `https://pool.econ.uzh.ch/`
- staging: `https://pool.econ.uzh.ch/staging/`

For the staging login credentials see 1Password.

## Architecture

Follow the [Architecture](./doc/ARCHITECTURE.md) documentation.

## Development

A guide how to setup the project with devcontainers can be found [here](./.devcontainer/README.md).

### Build project

When using a DevContainer, the `opam` packages and database files are mounted as volumes into the DevContainer.

- `.devcontainer/data/db-root/`: development mariadb data (root)
- `.devcontainer/data/db-tenant/`: development mariadb data (tenants)
- Docker volume `opam`: opam packages
- Docker volume `build`: cached build directory to speed up development

Run `make build` to build the pool-tool project. This builds `.ml` files using `dune`.

### Run / Start project

1. `make install` - to install all the dependencies. Under the hood, `dune` is used for package management.
1. `make sihl migrate.root` - runs all migrations for root database
1. `make sihl seed.root` - runs all seeds with development data for root database (use `make seed.root.clean` for a clean restart)
1. `make sihl migrate.tenant` - runs all migrations for tenant databases
1. `make sihl seed.tenant` - runs all seeds with development data for tenant database (use `make seed.tenant.clean` for a clean restart)
1. `make dev` - runs build in watch mode and the web application on port `3016` (default). You typically have that process running in one shell. Changing code will recompile and restart the server automatically. Changing JS code that is not embedded in `.ml` files (libraries) requires restarting the server.

Additionals

- `make test` to run all tests. This requires a running MariaDB instance for integration tests.
- `make sihl` runs the executable and shows the list of commands
- `make sihl <ARGS>` runs the executable with the provided arguments

### Environment files

For completeness, these environment files are handled with priority from SIHL.

- `.env`: Stores the environment for your local development
- `.env.test`: Stores the environment used when running tests (e.g. with `make test`)

### Start without seed

1.  The project executable can be run via command line `make sihl` and shows the information of the executable e.g.:

    ```bash
    2021-11-16T14:41:27-00:00 [INFO] [sihl.core.app]: Setting up...
    2021-11-16T14:41:27-00:00 [INFO] [sihl.core.configuration]: SIHL_ENV: development

    Sihl

    Run one of the following commands with the argument "help" for more information.

    config
    gen.html
    gen.model
    gen.view
    migrate.root
    migrate.tenant
    participant.signup
    random
    routes
    seed.root
    seed.root.clean
    seed.tenant
    seed.tenant.clean
    server
    tenant.create
    user.admin
    ```

1.  As a next step, the database can be migrated with

  ```bash
  make migrate.root
  ```

1.  And in this short preview, an example to add a new administrator

  ```bash
  make sihl user.admin engineering@econ.uzh.ch admin_secret
  ```

[Database View when using a DevContainer](./.devcontainer/README.md#database-view)

```
docker-compose -f .devcontainer/docker-compose.yml up -d database-root database-tenant adminer
```

to start a MariaDB and Adminer instance in a docker container and

```
docker-compose -f .devcontainer/docker-compose.yml down
```

to remove the MariaDB and Adminer container.

### Release to production

In order to deploy to production:

1. edit `dune-project` and update version `(version 0.0.0)`
1. build the project `dune build` or edit `payout.opam` and update version `version: "0.0.0"`
1. commit
1. tag commit and push
