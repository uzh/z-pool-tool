---
description: "Use when creating a new domain, adding a domain module, or scaffolding entity/event/command/repo/test for a new feature area in pool/app/."
---

# Creating a New Domain in Pool Tool

Follow this checklist in order. Each step has a template — adapt names to your domain.

## 1. Domain Directory Structure

Create `pool/app/<domain>/` with these files:

```
pool/app/<domain>/
  dune          # Library definition
  entity.ml     # Types + field sub-modules
  event.ml      # Events + handle_event
  entity_guard.ml  # Authorization
  repo.ml       # SQL queries
```

## 2. `dune` file

```ocaml
(library
 (name <domain>)
 (libraries pool_common pool_user pool_message)
 (preprocess
  (pps lwt_ppx ppx_deriving.eq ppx_deriving.show ppx_yojson_conv ppx_variants_conv)))
```

Add additional libraries as needed (`ptime`, `settings`, etc.).

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

let handle_event ?tags pool : event -> unit Lwt.t = function
  | Created { title } ->
    let entity = Entity.create ~title in
    Repo.insert pool entity
  | Updated entity ->
    Repo.update pool entity
  | Deleted id ->
    Repo.delete pool id
```

**Always** add `[@@deriving variants]` to the event type — it generates constructor functions used for dispatch.
**Always** annotate password/token fields with `[@opaque]` so `show` doesn't log them.

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

## 6. `repo.ml`

```ocaml
module RepoEntity = struct
  open Entity

  let t =
    let encode { id; title; created_at; updated_at } =
      Ok (id, (title, (created_at, updated_at)))
    in
    let decode (id, (title, (created_at, updated_at))) =
      Ok { id; title; created_at; updated_at }
    in
    Caqti_type.(custom ~encode ~decode (tup2 Id.t (tup2 Title.t (tup2 Pool_common.CreatedAt.t Pool_common.UpdatedAt.t))))
end

let insert_request =
  {sql|
    INSERT INTO pool_<domain>s (
      uuid, title, created_at, updated_at
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')), $2, $3, $4
    )
  |sql}
  |> RepoEntity.t ->. Caqti_type.unit

let find_request =
  {sql|
    SELECT
      LOWER(CONCAT(
        SUBSTR(HEX(uuid), 1, 8), '-',
        SUBSTR(HEX(uuid), 9, 4), '-',
        SUBSTR(HEX(uuid), 13, 4), '-',
        SUBSTR(HEX(uuid), 17, 4), '-',
        SUBSTR(HEX(uuid), 21)
      )), title, created_at, updated_at
    FROM pool_<domain>s
    WHERE uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Entity.Id.t ->! RepoEntity.t

let insert pool t = Database.exec pool insert_request t
let find pool id = Database.find pool find_request id
let find_opt pool id = Database.find_opt pool find_request id
```

UUID columns: store as `BINARY(16)`, **always** use `UNHEX(REPLACE(...))` on insert/query.

## 7. Register in `Pool_event`

Add a variant to `pool/pool_event/pool_event.ml`:

```ocaml
(* In the type *)
| <Domain> of <Domain>.Event.event

(* In handle_event *)
| <Domain> e -> <Domain>.Event.handle_event ?tags pool e

(* Constructor function *)
let <domain> e = <Domain> e
```

## 8. CQRS Command (`pool/cqrs_command/<domain>_command.ml`)

```ocaml
module Create : Common.CommandSig = struct
  type t = { title : <Domain>.Title.t }

  let schema =
    Pool_conformist.make
      Pool_conformist.Field.[ <Domain>.Title.schema () ]
      (fun title -> { title })

  let decode data =
    Pool_conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error

  let handle ?tags ({ title } : t) =
    Ok [ <Domain>.Event.Created { title } |> Pool_event.<domain> ]

  let effects = <Domain>.Guard.Access.create
end
```

## 9. Migration

Create `pool/app/pool_database/migrations/<YYYYMMDDHHMMSS>_create_<domain>.ml`:

```ocaml
let create_table =
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
  ( "<YYYYMMDDHHMMSS> create <domain>"
  , [ ("create pool_<domain>s", create_table) ] )
```

Register in `pool/app/pool_database/tenant.ml` (or `root.ml` for root-scope data):

```ocaml
let migrations = [ ...; Migration_<YYYYMMDDHHMMSS>_create_<domain>.migration () ]
```

## 10. Test (`pool/test/<domain>_test.ml`)

```ocaml
module Data = struct
  module Raw = struct
    let title = "Test Title"
  end

  module String = struct
    let title = Raw.title
  end

  module Validated = struct
    let title = <Domain>.Title.of_string String.title |> Test_utils.get_or_failwith
  end
end

let create_<domain>_test () =
  let cmd = <Domain>_command.Create.decode
    [ (Pool_message.Field.show Pool_message.Field.Title, [ Data.String.title ]) ]
  in
  let expected = Ok [ <Domain>.Event.Created { title = Data.Validated.title }
                      |> Pool_event.<domain> ]
  in
  Test_utils.check_result expected (cmd |> CCResult.flat_map (<Domain>_command.Create.handle))

let suite =
  [ Alcotest_lwt.test_case "Create <domain>" `Quick (fun _ () ->
      create_<domain>_test ();
      Lwt.return_unit)
  ]
```
