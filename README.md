# Pool Tool

## Architecture

Follow the [Architecture](./doc/ARCHITECTURE.md) documentation.

## Development

A guide how to setup the project with devcontainers can be found [here](./.devcontainer/README.md).

The project executable can be run via command line `make sihl` and shows the information of the executable.

1. Create a `.env` file and set the environment variable `ECON_PACKAGE_REGISTRY_READ_TOKEN`to the value stored in 1Password or in the GitLab CI/CD variables.
1. Migrate root database with `make sihl migrate.root`
1. Seed root database `make sihl seed.root`
1. Migrate tenant database with `make sihl migrate.tenant`
1. Seed tenant database `make sihl seed.tenant`
1. Run development server `make dev`
1. See `pool/database/seed/seed_user.ml` for default users or `make sihl user.admin it@econ.uzh.ch admin_secret` to generate one

### Commands

- `make install` - to install all the dependencies. Under the hood, `dune` is used for package management.
- `make dev` - runs build in watch mode and the web application on port `3016` (default). You typically have that process running in one shell. Changing code will recompile and restart the server automatically. Changing JS code that is not embedded in `.ml` files (libraries) requires restarting the server.
- `make test` to run all tests. This requires a running MariaDB instance for integration tests.
- `make sihl` runs the executable and shows the list of commands
- `make sihl <ARGS>` runs the executable with the provided arguments

#### Application commands

The following list shows only the most frequently used commands, for a complete list use `make sihl` as above.

- `make sihl migrate.root` - runs all migrations for root database
- `make sihl seed.root` - runs all seeds with development data for root database (use `make seed.root.clean` for a clean restart)
- `make sihl migrate.tenant` - runs all migrations for tenant databases
- `make sihl seed.tenant` - runs all seeds with development data for tenant database (use `make seed.tenant.clean` for a clean restart)
- `make sihl admin.create` -  create a new administrator on a specific tenant
- `make sihl admin.grant_role` -  grant a role to an existing administrator

Example commands to add an administrator and provide `OperatorAll` role.

```bash
run.exe admin.create tenant1 it@econ.uzh.ch password Firstname Lastname Admin
run.exe admin.grant_role tenant1 it@econ.uzh.ch OperatorAll
```

### Environment files

For completeness, these environment files are handled with priority from SIHL.

- `.env`: Stores the environment for your local development
- `.env.test`: Stores the environment used when running tests (e.g. with `make test`)

## 🚀 Release new version

1. Update `CHANGELOG.md` and document changes made. Ensure the version to be releases has a header matching the version, follow previous releases.
1. Commit your changes.
1. Release version using `yarn version`.
1. Push changes and Git tag.

### Build project

Run `make build` to build the pool-tool project. This builds `.ml` files using `dune`.

### Local test with production environment

When testing the production environment, you need to update the csrf cookie key (default `__HOST-csrf`).
This can be done with adding the optional argument `cookie_key` to the csrf middleware.

```ocaml
(* NOTE: the middleware might have additional arguments defined *)
Sihl.Web.Middleware.csrf ~cookie_key:"csrf" ()
```

> **`__Host-` prefix**: Cookies with names starting with `__Host-` must be set with the `secure` flag, must be from a secure page (HTTPS), must not have a domain specified (and therefore aren't sent to subdomains) and the path must be `/`.

_Source: <https://developer.mozilla.org/de/docs/Web/HTTP/Headers/Set-Cookie>_

Alternatively, these options simulate a similar production environment (without `CHECK_CSRF`):

```bash
QUEUE_FORCE_ASYNC=true EMAIL_BYPASS_INTERCEPT=true SMTP_SENDER=noreply@uast.uzh.ch SMTP_HOST=uzhxchange.uzh.ch SMTP_USERNAME=noreply@uast.uzh.ch SMTP_PORT=5587 SMTP_START_TLS=true SMTP_PASSWORD=<password> make dev
```

Use this until new SMTP method is supported is resolved:

```bash
SMTP_HOST=smtp.uzh.ch SMTP_PORT=25 SMTP_SENDER=info@uast.uzh.ch SMTP_START_TLS=true EMAIL_BYPASS_INTERCEPT=true make dev
```
