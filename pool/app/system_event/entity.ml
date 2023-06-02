module Id = struct
  include Pool_common.Id

  let to_common m = m
end

module Key = struct
  include Pool_common.Model.String

  let field = Pool_common.Message.Field.Key
  let schema () = schema field ()
end

module Argument = struct
  include Pool_common.Model.String

  module Ptime = struct
    include Ptime

    let t_of_yojson = Utils.Ptime.ptime_of_yojson
    let yojson_of_t = Utils.Ptime.yojson_of_ptime
  end

  let field = Pool_common.Message.Field.Argument
  let schema () = schema field ()

  let tenant_opt m =
    try m |> Pool_database.Label.create |> CCResult.to_opt with
    | _ -> None
  ;;

  type user_login_ban = Pool_database.Label.t * string * int * Ptime.t option
  [@@deriving eq, show { with_path = false }, yojson]

  let user_login_ban_opt m =
    try
      m
      |> Yojson.Safe.from_string
      |> user_login_ban_of_yojson
      |> CCOption.return
    with
    | _ -> None
  ;;
end

type t =
  { id : Id.t
  ; key : Key.t
  ; argument : Argument.t option
  ; created_at : Pool_common.CreatedAt.t [@equal fun _ _ -> true]
  ; updated_at : Pool_common.UpdatedAt.t [@equal fun _ _ -> true]
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) ?argument key =
  let open CCResult in
  Ok
    { id
    ; key
    ; argument
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
;;

module EventLog = struct
  module Hostname = struct
    (* TODO: rename hostname to service identifier, as the worker also needs so
       run these events *)
    include Pool_common.Model.String

    let field = Pool_common.Message.Field.Host
    let schema () = schema field ()
    let get = Unix.gethostname
  end

  let print = Utils.ppx_printer

  module Status = struct
    module Core = struct
      let field = Pool_common.Message.Field.Status

      type t =
        | Failed [@name "failed"] [@printer print "failed"]
        | Successful [@name "successful"] [@printer print "successful"]
      [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
    end

    include Pool_common.Model.SelectorType (Core)
    include Core

    let to_human language =
      let go = Pool_common.Utils.field_to_string language in
      let open Pool_common.Message in
      function
      | Failed -> Field.Failed |> go
      | Successful -> Field.Successful |> go
    ;;
  end

  module Message = struct
    include Pool_common.Model.String

    let field = Pool_common.Message.Field.Message
    let schema () = schema field ()
  end

  type t =
    { event_id : Id.t
    ; hostname : Hostname.t (* TODO: rename hostname to service identifier *)
    ; status : Status.t
    ; message : Message.t option
    ; created_at : Pool_common.CreatedAt.t [@equal fun _ _ -> true]
    ; updated_at : Pool_common.UpdatedAt.t [@equal fun _ _ -> true]
    }
  [@@deriving eq, show]

  let create ?message event_id hostname status =
    let open CCResult in
    Ok
      { event_id
      ; hostname
      ; status
      ; message
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      }
  ;;
end
