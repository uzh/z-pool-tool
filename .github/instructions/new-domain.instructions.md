---
description: "Use when creating a new domain, adding a domain module, or scaffolding entity/event/command/repo/test for a new feature area in pool/app/."
---

# Creating a New Domain in Pool Tool

Follow this checklist in order. Each step has a template — adapt names to your domain.

## 1. Domain Directory Structure

Create `pool/app/<domain>/` with these files:

```
pool/app/<domain>/
  dune             # Library definition (must include `(include_subdirs unqualified)`)
  entity.ml        # Types + field sub-modules
  event.ml         # Events + handle_event
  entity_guard.ml  # Authorization
  <domain>.ml      # Top-level wrapper: includes Entity, Event, Repo; re-exports functions
  <domain>.mli     # Public interface
  repo/
    repo_entity.ml # Caqti type encoders/decoders (one sub-module per entity field)
    repo.ml        # SQL queries, using types from repo_entity.ml
```

## 2. `dune` file

```ocaml
(library
 (name <domain>)
 (libraries pool_common pool_message)
 (preprocess
  (pps lwt_ppx ppx_deriving.eq ppx_deriving.show ppx_yojson_conv)))

(include_subdirs unqualified)
```

Add libraries as needed (`guard`, `pool_user`, `pool_tenant`, `ptime`, `utils`, etc.) — only include what the domain actually imports. Add `ppx_variants_conv` to the `pps` list only if the event type uses `[@@deriving variants]`. `(include_subdirs unqualified)` is required so that files in `repo/` are compiled as part of the same library.

## 3. `entity.ml`

```ocaml
module Id = struct
  include Pool_common.Id
end

module Title = struct
  include Pool_model.Base.String
  let field = Pool_message.Field.Title
  let schema () = schema field ()
end

(* Add more field sub-modules following the same pattern *)

type t =
  { id : Id.t
  ; title : Title.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show, yojson]

let create ~title =
  { id = Id.create ()
  ; title
  ; created_at = Pool_common.CreatedAt.create_now ()
  ; updated_at = Pool_common.UpdatedAt.create_now ()
  }
```

Use `Pool_model.Base.String / Base.Id / Base.Boolean / Base.Integer` as the foundation for field sub-modules. Each sub-module must expose `field` and `schema ()`.

## 4. `event.ml`

```ocaml
type create =
  { title : Entity.Title.t
  (* Add more fields; mark sensitive ones [@opaque] *)
  }
[@@deriving eq, show]

type event =
  | Created of create
  | Updated of Entity.t
  | Deleted of Entity.Id.t
[@@deriving eq, show, variants]

let handle_event ?user_uuid pool : event -> unit Lwt.t = function
  | Created { title } ->
    let entity = Entity.create ~title in
    Repo.insert pool entity
  | Updated entity ->
    Repo.update pool entity
  | Deleted id ->
    Repo.delete pool id
```

Add `[@@deriving variants]` to the event type when `Pool_event.ml` uses the variant constructor functions for dispatch (most new domains should include it). Omit it only for domains that don't register their events centrally.

**Always** annotate password/token fields with `[@opaque]` so `show` doesn't log them.

The `handle_event` signature varies by domain need:
- `let handle_event pool : event -> unit Lwt.t` — minimal (no audit trail)
- `let handle_event ?user_uuid pool : event -> unit Lwt.t` — when the domain needs audit user tracking
- `let handle_event ~tags pool : event -> unit Lwt.t` — when structured log tags are required

Default to `?user_uuid` if the domain touches user-visible state; add `~tags` only if the domain explicitly logs with tag sets.

## 5. `entity_guard.ml`

```ocaml
module Target = struct
  let to_authorizable ?ctx t =
    Guard.Persistence.Target.decorate ?ctx
      (fun e -> Guard.Target.create `<Domain> (Guard.Uuid.target_of Entity.Id.value e.Entity.id))
      t
end

module Access = struct
  open Guard
  open ValidationSet

  let domain action uuid =
    one_of_tuple (action, `<Domain>, Some (Uuid.target_of Entity.Id.value uuid))

  let index = one_of_tuple (Permission.Read, `<Domain>, None)
  let create = one_of_tuple (Permission.Create, `<Domain>, None)
  let read uuid = domain Permission.Read uuid
  let update uuid = domain Permission.Update uuid
  let delete uuid = domain Permission.Delete uuid
end
```

## 6. `repo/repo_entity.ml`

Caqti type encoders/decoders. One sub-module per entity field, plus a top-level `t` encoder:

```ocaml
open CCFun.Infix
open Entity

let make_caqti_type = Pool_common.Repo.make_caqti_type

module Id = struct
  include Id

  let t = make_caqti_type Caqti_type.string (of_string %> CCResult.return) value
end

module Title = struct
  include Title

  let t = make_caqti_type Caqti_type.string create value
end

let t =
  let open Database.Caqti_encoders in
  let decode (id, (title, (created_at, updated_at))) =
    Ok { id; title; created_at; updated_at }
  in
  let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
  let open Schema in
  custom ~encode ~decode Caqti_type.[ Id.t; Title.t; Pool_common.CreatedAt.t; Pool_common.UpdatedAt.t ]

(* Write model: used for INSERT/UPDATE — encode only, decode raises *)
module Write = struct
  let t =
    let open Database.Caqti_encoders in
    let decode _ = Pool_common.Utils.failwith Pool_message.Error.WriteOnlyModel in
    let encode { id; title; created_at; updated_at } =
      Ok Data.[ id; title; created_at; updated_at ]
    in
    let open Schema in
    custom ~encode ~decode Caqti_type.[ Id.t; Title.t; Pool_common.CreatedAt.t; Pool_common.UpdatedAt.t ]
end
```

## 7. `repo/repo.ml`

SQL queries, referencing types from `Repo_entity`:

```ocaml
open Repo_entity

let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_<domain>s.uuid"
  ; "pool_<domain>s.title"
  ; "pool_<domain>s.created_at"
  ; "pool_<domain>s.updated_at"
  ]

let find_request_sql where_fragment =
  Format.asprintf
    {sql| SELECT %s FROM pool_<domain>s %s |sql}
    (CCString.concat ", " sql_select_columns)
    where_fragment

let insert_request =
  {sql|
    INSERT INTO pool_<domain>s (
      uuid, title, created_at, updated_at
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')), $2, $3, $4
    )
  |sql}
  |> t ->. Caqti_type.unit

let find_request =
  find_request_sql "WHERE pool_<domain>s.uuid = UNHEX(REPLACE($1, '-', ''))"
  |> Entity.Id.t ->! t

let insert pool entity = Database.exec pool insert_request entity
let find pool id = Database.find pool find_request id
let find_opt pool id = Database.find_opt pool find_request id
```

UUID columns are stored as `BINARY(16)` — **always** use `UNHEX(REPLACE(...))` on insert/query.

## 8. `<domain>.ml` (top-level wrapper module)

`include` the sub-modules and re-export any repo functions that callers need:

```ocaml
include Entity
include Event
module Guard = Entity_guard   (* preferred: namespaced as <Domain>.Guard.Access *)

let find = Repo.find
let find_opt = Repo.find_opt

module Repo = struct
  include Repo_entity

  let sql_select_columns = Repo.sql_select_columns
  let find_request_sql = Repo.find_request_sql
end
```

Two patterns for guard exposure exist:
- `module Guard = Entity_guard` — namespaced (`<Domain>.Guard.Access.create`); preferred for new domains
- `include Entity_guard` — flat (`<Domain>.Access.create`); used by some older domains

Expose only the repo internals (`sql_select_columns`, `find_request_sql`, etc.) that other domains join against. Keep the top-level surface minimal.

## 8a. `<domain>.mli` (interface file)

Declare the public interface, mirroring what `<domain>.ml` exposes. Use `include module type of <Module>` for sub-modules whose full signature should be re-exported, and explicit `sig … end` for anything that needs restricting.

## 9. Register in `Pool_event`

Add a variant to `pool/pool_event/pool_event.ml` **and** `pool_event.mli`:

```ocaml
(* In pool_event.ml — type t *)
| <Domain> of <Domain>.Event.event

(* In pool_event.ml — handle internal dispatch function *)
| <Domain> e -> <Domain>.Event.handle_event ?user_uuid pool e
  (* pass ?tags / ?user_uuid matching what handle_event expects *)

(* In pool_event.ml — public constructor function *)
let <domain> e = <Domain> e
```

Mirror the same additions in `pool_event.mli`:
```ocaml
(* type t variant *)
| <Domain> of <Domain>.Event.event

(* constructor *)
val <domain> : <Domain>.Event.event -> t
```

## 10. CQRS Command (`pool/cqrs_command/<domain>_command.ml`)

```ocaml
let src = Logs.Src.create "<domain>.cqrs"

module Create : sig
  include Common.CommandSig

  type t = { title : <Domain>.Title.t }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode
    :  (string * string list) list
    -> (t, Pool_message.Error.t) result
end = struct
  type t = { title : <Domain>.Title.t }

  let schema =
    Pool_conformist.make
      Pool_conformist.Field.[ <Domain>.Title.schema () ]
      (fun title -> { title })

  let handle ?(tags = Logs.Tag.empty) ({ title } : t) =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    Ok [ <Domain>.Event.Created { title } |> Pool_event.<domain> ]

  let decode data =
    Pool_conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error

  let effects = <Domain>.Guard.Access.create
end
```

`Common.CommandSig` only requires `handle : ?tags:Logs.Tag.set -> t -> result` and `effects : Guard.ValidationSet.t`. Commands needing additional arguments (existing entity, tenant list, etc.) extend the signature with an explicit `sig ... end` wrapper overriding `handle`'s type.

## 11. Migration

Create `pool/app/pool_database/migrations/migration_<YYYYMMDDHHMM>.ml` (12-digit timestamp, no `_create_<domain>` suffix — e.g. `migration_202605110000.ml`):

```ocaml
let create_table =
  Database.Migration.Step.create
    ~label:"create pool_<domain>s"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_<domain>s (
        id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
        uuid BINARY(16) NOT NULL,
        title VARCHAR(255) NOT NULL,
        created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        UNIQUE KEY (uuid)
      )
    |sql}

let migration () =
  Database.Migration.(
    empty "<YYYYMMDDHHMM>"
    |> add_step create_table)
```

The string passed to `empty` must exactly match the timestamp in the filename (e.g. `"202605110000"`).

Register in `pool/app/pool_database/tenant.ml` (or `root.ml` for root-scope data) using the module name derived from the filename:

```ocaml
let migrations = [ ...; Migration_<YYYYMMDDHHMM>.migration () ]
```

## 12. Test (`pool/test/<domain>_test.ml`)

```ocaml
let get_or_failwith = Test_utils.get_or_failwith

module Data = struct
  let id = <Domain>.Id.create ()
  let title = <Domain>.Title.of_string "Test Title" |> get_or_failwith
  let title_str = <Domain>.Title.value title

  (* URL-encoded form data for command decoding *)
  let create =
    [ Pool_message.Field.(show Title), [ title_str ] ]
end

let create_<domain>_test _ () =
  let cmd = <Domain>_command.Create.decode Data.create in
  let expected =
    Ok [ <Domain>.Event.Created { title = Data.title } |> Pool_event.<domain> ]
  in
  Test_utils.check_result expected
    (cmd |> CCResult.flat_map <Domain>_command.Create.handle);
  Lwt.return_unit

let suite =
  [ Alcotest_lwt.test_case "Create <domain>" `Quick create_<domain>_test ]
```

`Test_utils.check_result` signature: `?msg:string -> expected:result -> actual:result -> unit`—arguments are positional, `expected` first then `actual`.

`Test_utils.get_or_failwith` unwraps a result, raising on `Error`.

`Test_utils.Model.*` provides factories for common entities (`create_admin`, `create_contact`, `create_experiment`, etc.).

### Test registration

Add the suite to the appropriate test runner:
- **Pure unit tests** (no DB): register in `pool/test/command.ml` — add `(<Domain>_test.suite, "<Domain> commands")`
- **Integration tests** (DB-backed): register in `pool/test/integration.ml`
