type identifier =
  [ `Server
  | `Worker
  ]

module Id = struct
  include Pool_common.Id

  let to_common m = m
end

module Job = struct
  type t =
    | GuardianCacheCleared [@name "guardiancachecleared"]
    [@printer Utils.ppx_printer "guardiancachecleared"]
    | I18nPageUpdated [@name "i18npageupdated"]
    [@printer Utils.ppx_printer "i18npageupdated"]
    | PageScriptsUpdated [@name "pagescriptsupdated"]
    [@printer Utils.ppx_printer "pagescriptsupdated"]
    | SmtpAccountUpdated [@name "smtpaccountupdated"]
    [@printer Utils.ppx_printer "smtpaccountupdated"]
    | TenantDatabaseReset of Database.Label.t [@name "tenantdatabasereset"]
    [@printer Utils.ppx_printer "tenantdatabasereset"]
    | TenantCacheCleared [@name "tenantcachecleared"]
    [@printer Utils.ppx_printer "tenantcachecleared"]
    | TenantDatabaseCacheCleared [@name "tenantdatabasecachecleared"]
    [@printer Utils.ppx_printer "tenantdatabasecachecleared"]
  [@@deriving eq, show, yojson]

  let read m = m |> Yojson.Safe.from_string |> t_of_yojson

  let of_string str =
    try Ok (read str) with
    | _ -> Error Pool_message.(Error.Invalid Field.SystemEvent)
  ;;

  let to_string m = m |> yojson_of_t |> Yojson.Safe.to_string
end

type t =
  { id : Id.t
  ; job : Job.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) job =
  { id
  ; job
  ; created_at = Pool_common.CreatedAt.create_now ()
  ; updated_at = Pool_common.UpdatedAt.create_now ()
  }
;;

module EventLog = struct
  module ServiceIdentifier = struct
    include Pool_model.Base.String

    let field = Pool_message.Field.Host
    let schema () = schema field ()

    let get identifier =
      let hostname = Unix.gethostname () in
      match identifier with
      | `Server -> hostname
      | `Worker -> Format.asprintf "%s-worker" hostname
    ;;
  end

  let print = Utils.ppx_printer

  module Status = struct
    module Core = struct
      let field = Pool_message.Field.Status

      type t =
        | Failed [@name "failed"] [@printer print "failed"]
        | Successful [@name "successful"] [@printer print "successful"]
      [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
    end

    include Pool_model.Base.SelectorType (Core)
    include Core

    let to_human language =
      let go = Pool_common.Utils.field_to_string language in
      let open Pool_message in
      function
      | Failed -> Field.Failed |> go
      | Successful -> Field.Successful |> go
    ;;

    let read = Utils.Json.read_variant t_of_yojson

    let of_string str =
      try Ok (read str) with
      | _ -> Error Pool_message.(Error.Invalid Field.Status)
    ;;
  end

  module Message = struct
    include Pool_model.Base.String

    let field = Pool_message.Field.Message
    let schema () = schema field ()
  end

  type t =
    { event_id : Id.t
    ; service_identifier : ServiceIdentifier.t
    ; status : Status.t
    ; message : Message.t option
    ; created_at : Pool_common.CreatedAt.t
    ; updated_at : Pool_common.UpdatedAt.t
    }
  [@@deriving eq, show]

  let create ?message event_id service_identifier status =
    { event_id
    ; service_identifier
    ; status
    ; message
    ; created_at = Pool_common.CreatedAt.create_now ()
    ; updated_at = Pool_common.UpdatedAt.create_now ()
    }
  ;;
end
