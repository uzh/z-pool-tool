open Ppx_yojson_conv_lib.Yojson_conv

exception Exception of string

module Id = struct
  include Pool_common.Id
end

module Change = struct
  (*** Currently, the @to_yojson/@of_joyson is working for record types but not
    for variants:
    https://github.com/ocaml-ppx/ppx_deriving_yojson?tab=readme-ov-file#to_yojson--of_yojson

    If this is fixed, this module is superfluous *)
  type t = Yojson.Safe.t * Yojson.Safe.t [@@deriving eq, show]

  let t_of_yojson (json : Yojson.Safe.t) : t =
    match json with
    | `Tuple [ a; b ] -> a, b
    | _ -> raise (Yojson.Json_error "Invalid Change.t yojson")
  ;;

  let yojson_of_t (a, b) = `Tuple [ a; b ]
end

module Changes = struct
  type t =
    | Assoc of (string * t) list
    | Change of Change.t
  [@@deriving eq, show, yojson]

  let of_string str = str |> Yojson.Safe.from_string |> t_of_yojson
  let to_string t = t |> yojson_of_t |> Yojson.Safe.to_string
end

type t =
  { id : Id.t
  ; changes : Changes.t
  ; model : Pool_message.Field.t
  ; entity_uuid : Pool_common.Id.t
  ; user_uuid : Pool_common.Id.t
  ; user_email : Pool_user.EmailAddress.t
  ; created_at : Pool_common.CreatedAt.t
  }

module Write = struct
  type t =
    { id : Id.t
    ; changes : Changes.t
    ; model : Pool_message.Field.t
    ; entity_uuid : Pool_common.Id.t
    ; user_uuid : Pool_common.Id.t
    ; created_at : Pool_common.CreatedAt.t
    }
end

module type RecordSig = sig
  type t

  val model : Pool_message.Field.t
  val yojson_of_t : t -> Yojson.Safe.t
end

module type TSig = sig
  type record

  val model : Pool_message.Field.t
  val default_query : Query.t
  val filterable_by : Query.Filter.human option
  val searchable_by : Query.Column.t list
  val sortable_by : Query.Column.t list

  val create
    :  Database.Label.t
    -> ?id:Id.t
    -> entity_uuid:Pool_common.Id.t
    -> user_uuid:Pool_common.Id.t
    -> before:record
    -> after:record
    -> unit
    -> unit Lwt.t

  val all_by_entity
    :  ?query:Query.t
    -> Database.Label.t
    -> Pool_common.Id.t
    -> (t list * Query.t) Lwt.t
end
