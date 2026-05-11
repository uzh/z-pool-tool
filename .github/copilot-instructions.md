# Pool Tool – Copilot Instructions

OCaml multi-tenant participant management platform built on the [Sihl](https://github.com/oxidizing/sihl) framework.

## Build & Test

```bash
make build          # Compile (dune + yarn CSS/JS bundle)
make dev            # Watch mode + live-reload server on :3016 / :3017
make format         # Auto-format with ocamlformat (dune @fmt --auto-promote)
make test           # Seed test tenant + run alcotest suite
make test-migrate   # Clean DB: migrate root + seed root + migrate tenant (no test run)
make test-clean     # Full clean: migrate → seed → test (requires MariaDB)
make sihl <cmd>     # Run executable; e.g. make sihl migrate.root
```

**Database bootstrap order:**
1. `make sihl migrate.root`
2. `make sihl seed.root.clean`
3. `make sihl migrate.tenant`
4. `make sihl seed.tenant.clean`

Default dev users are in [pool/seed/seed_user.ml](../pool/seed/seed_user.ml).

## Architecture

```
pool/app/<domain>/        # 40+ domains (contact, experiment, session, …)
  entity.ml               # Core types with [@@deriving eq, show, yojson]
  event.ml                # Domain events (discriminated union + handle_event)
  entity_guard.ml         # Guardian authorization targets/access
  repo/                   # Raw SQL via Caqti – no ORM (via Repo Entities)

pool/cqrs_command/        # CQRS commands: decode → handle → events + effects
pool/pool_event/          # Unified Pool_event.t, dispatch to domain handlers
pool/web/handler/         # HTTP handlers (Opium)
pool/web/view/            # TyXML HTML generation (no template files)
pool/routes/              # Route definitions
pool/test/                # Alcotest-lwt tests, one file per domain
```

**Two databases:**
- *Root* – tenant registry, root admins, global config (`Pool_database.Root`)
- *Tenant* – per-tenant isolated data; resolved from subdomain at runtime

## Key Conventions

### Entity field sub-modules
Every domain field is a module extending a base type:
```ocaml
module Title = struct
  include Pool_model.Base.String  (* or Base.Id, Base.Boolean, Base.Integer, … *)
  let field = Pool_message.Field.Title
  let schema () = schema field ()  (* produces a Pool_conformist decoder *)
end
```
Entities derive `[@@deriving eq, show, yojson]`; event variant types additionally `[@@deriving variants]`. Mark sensitive values (passwords, tokens) `[@opaque]` in event payload types so `show` doesn't leak them.

### Error handling
Use `(_, Pool_message.Error.t) result` throughout. Prefer `CCResult` operators:
- `>|=` map ok, `>>|` flatmap, `>|+` map ok (containers variant)
- Log + pass through with `Pool_common_utils.with_log_error`

### CQRS command pattern
```ocaml
module MyCommand : Common.CommandSig = struct
  type t = { ... }
  let schema = Pool_conformist.make [ ... ] (fun ... -> { ... })
  let decode data =
    Pool_conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error  (* required: gives field-level errors *)
  let handle ?tags (cmd : t) = Ok [ Domain.event_ctor data |> Pool_event.domain ]
  let effects role id = Guard.ValidationSet.one_of_tuple (Permission.Update, role, Some id)
end
```

### Event pattern
Each domain defines `type event [@@deriving eq, show, variants]` and `let handle_event : Pool_database.Label.t -> event -> unit Lwt.t`. Events are dispatched centrally via `Pool_event.handle_event`. Wrap domain events before dispatch: `Domain.MyEvent |> Pool_event.domain_name`.

### Authorization (Guardian)
Access checks live in `entity_guard.ml` as `Access` and `Guard` sub-modules. Routes validate permissions in middleware by calling `Guard.Persistence.validate`.

### Database queries (Caqti)
Write typed queries using `Caqti_request.Infix`:
```ocaml
let find_request =
  {sql| SELECT ... WHERE uuid = UNHEX(REPLACE($1, '-', '')) |sql}
  |> Id.t ->! t
let find pool id = Database.find_opt pool find_request id
```
UUIDs are stored as `BINARY(16)` and require `UNHEX(REPLACE(...))` on insert/query.

### Multi-tenancy
`Pool_context.t` is threaded through every handler carrying `database_label`, authenticated user, language, csrf token. Always pass context, never hardcode a database label.

### HTML generation
Use TyXML (`ppx_tyxml`) — no template files. Components live in `pool/web/view/component/`, full pages in `pool/web/view/page/`. Add HTMX attributes via helpers in `pool/web/view/htmx/`.

### Migrations
Add timestamped `.ml` files under `pool/app/pool_database/migrations/`. Filename format: `migration_yyyymmddHHMM.ml` (e.g. `migration_202605110000.ml`). The internal `empty` timestamp string must match the filename timestamp exactly. Register them in [root.ml](../pool/app/pool_database/root.ml) or [tenant.ml](../pool/app/pool_database/tenant.ml).

## AI Agent Workflow

### Test-Driven Development (TDD)
Always follow the red/green/refactor cycle:

1. **Red** — write a failing test in `pool/test/<domain>.ml` that captures the intended behaviour. Run `make test` and confirm it fails.
2. **Green** — write the minimal production code to make the test pass. Run `make test` and confirm it is green.
3. **Refactor** — clean up duplication or style issues, then run `make test` again to stay green.
4. **Format** — run `make format` before committing.

Never write production code before a failing test exists.

### Approved terminal commands (no confirmation needed)
The following commands may be run freely:
- `make build` / `make test` / `make test-migrate` / `make test-clean` / `make format` / `make sihl <cmd>`
- `dune build` / `dune test` / `dune fmt` / `dune clean`
- `opam install` / `opam list` / `opam show`
- `yarn`
- Read-only git commands: `git status`, `git log`, `git diff`, `git show`, `git branch`
- Safe branch operations: `git add`, `git commit`, `git checkout`, `git switch`, `git stash`

Commands that require confirmation: `git push`, `git reset`, `git rebase`, `git merge`, destructive `make` targets, any `rm -rf`.

## Common Pitfalls

- **UUID encoding**: Always use `UNHEX(REPLACE(?, '-', ''))` for UUID lookups; forgetting this returns empty results silently.
- **Database label**: Never use a hardcoded label — use the one from `Pool_context.t`.
- **Event sourcing**: State changes must go through events; direct repo writes bypass the audit log and side-effect handlers.
- **CSRF in production**: Cookie name must drop the `__Host-` prefix in non-HTTPS environments (see README §Local test with production environment).
- **`make test` requires MariaDB**: Tests hit a real database. Use `.env.test` for connection strings.
- **Sensitive fields in events**: Always annotate password/token fields in event payload types with `[@opaque]`; otherwise `show` (used in logging) will leak them in plaintext.
- **Conformist error wrapping**: The `decode` function in every command must apply `Pool_message.to_conformist_error`; omitting it turns field-level validation errors into opaque generic strings.
- **Test fixtures**: Use `Test_utils.Model.*` factories (not direct constructors) to build test entities; `check_result` compares `(Pool_event.t list, Pool_message.Error.t) result` values. Data is organized in `Data.Raw / Data.String / Data.Validated` sub-modules within each test.
