open Ppx_yojson_conv_lib.Yojson_conv

exception Exception of string

(* module Json = struct include Yojson.Safe

   let yojson_of_t change = `Tuple let t_of_yojson = CCFun.const let to_yojson =
   CCFun.const end *)
(* module Yojson = struct include Yojson.Safe

   let yojson_of_t = CCFun.const let t_of_yojson = CCFun.const end *)

module Id = struct
  include Pool_common.Id
end

module Change = struct
  type t = Yojson.Safe.t * Yojson.Safe.t [@@deriving eq, show]

  let t_of_yojson (json : Yojson.Safe.t) : t =
    match json with
    | `Tuple [ a; b ] -> a, b
    | _ -> raise (Exception "TODO")
  ;;

  let yojson_of_t (a, b) = `Tuple [ a; b ]
end

module Changes = struct
  (* TODO: There has to be a better way *)

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
  ; created_at : Pool_common.CreatedAt.t
  }

module type RecordSig = sig
  type t

  val model : Pool_message.Field.t
  val yojson_of_t : t -> Yojson.Safe.t
end

module type TSig = sig
  type record

  val model : Pool_message.Field.t

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
