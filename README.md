# Pool Tool

## Development

A guide to setup the project with devcontainers can be found
[here](./.devcontainer/README.md). If you prefer to [run the project
locally](#running-locally-with-Esy), you can do that with [esy][esy].

The project executable can be run via command line `make sihl` and shows the information of the executable.

1. Migrate root database with `make sihl migrate.root`
1. Seed root database `make sihl seed.root`
1. Migrate tenant database with `make sihl migrate.tenant`
1. Seed tenant database `make sihl seed.tenant`
1. Run development server `make dev`
1. See `pool/database/seed/seed_user.ml` for default users or `make sihl admin.create ...` (see [Application Commands](#application-commands)) to generate one

### Running locally with Esy

Esy is a reproducible package manager for OCaml with heavy emphasis on
usability. You can get it installed with `npm install --global esy`, and then
run `esy` inside the project.

Once `esy` finishes running, you can prefix any command with `esy ...` to
execute it within the project's sandbox, or you can start a new development
shell within it by running `esy shell`.

[esy]: https://esy.sh

### Commands

- `make install` - to install all the dependencies. Under the hood, `dune` is used for package management.
- `make dev` - runs build in watch mode and the web application on port `3016` (default). You typically have that process running in one shell. Changing code will recompile and restart the server automatically. Changing JS code that is not embedded in `.ml` files (libraries) requires restarting the server.
- `make test-clean` - clean prepare and run all tests. This requires a running MariaDB instance for integration tests.
- `make sihl` - runs the executable and shows the list of commands (when at least one argument is missing or `-h` is used, the help is printed)
- `make sihl <ARGS>` - runs the executable with the provided arguments

#### Application commands

The following list shows only the most frequently used commands, for a complete list use `make sihl` as above.

- `make sihl migrate.root` - runs all migrations for root database
- `make sihl seed.root` - runs all seeds with development data for root database (use `make seed.root.clean` for a clean restart)
- `make sihl migrate.tenant` - runs all migrations for tenant databases
- `make sihl seed.tenant` - runs all seeds with development data for tenant database (use `make seed.tenant.clean` for a clean restart)
- `make sihl admin.create` - create a new administrator on a specific tenant
- `make sihl admin.grant_role` - grant a role to an existing administrator
- `make sihl admin.list_roles` - show a list of all possible role patterns
- `make sihl seed.default message_templates` - inserts the default message templates to all databases (resets default templates)
- `make sihl server` - starts the tools webserver
- `make sihl worker` - starts the tools worker for background jobs (e.g. send mails, create invitations)

Example commands to add an administrator for a specific role.

```bash
# list available roles (shows also patterns)
run.exe admin.list_roles

# Create an Operator
run.exe admin.create tenant1 operator@mail.com password Firstname Lastname Operator

# Create an Experimenter with UUID
run.exe admin.create tenant1 experimenter@mail.com password Firstname Lastname Experimenter 00000000-0000-0000-0000-000000000000

# Grant additional role to some administrator
run.exe admin.grant_role tenant1 experimenter@mail.com RecruiterAll
```

### Environment files

For completeness, these environment files are handled with priority from SIHL.

- `.env`: Stores the environment for your local development
- `.env.test`: Stores the environment used when running tests (e.g. with `make test`)

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
QUEUE_FORCE_ASYNC=true EMAIL_BYPASS_INTERCEPT=true SMTP_SENDER=noreply@uast.uzh.ch make dev
```

## 🧑‍💻 Maintenance

Update/upgrade all package versions for ocaml and yarn packages.

1. `make update` -> updates yarn and opam
1. `make build` -> check if no errors occur (else: debug)
1. `make test-clean` -> clean prepare for tests and run then (on failiures: debug)
1. `make lock`
1. commit all changes
1. release a new version depending on updates

## 🚀 Release new version

1. Check [Maintenance Cycle](#🧑‍💻-maintenance)
1. Update `CHANGELOG.md` and document changes made. Ensure the version to be releases has a header matching the version, follow previous releases.
1. Commit your changes.
1. Release version using `yarn version`
1. Push changes and Git tag.
