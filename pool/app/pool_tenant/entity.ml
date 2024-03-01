open Sexplib.Conv
module Common = Pool_common
module Database = Pool_database
module Id = Common.Id
module CreatedAt = Common.CreatedAt
module UpdatedAt = Common.UpdatedAt
module File = Common.File
module LogoMapping = Entity_logo_mapping

module Title = struct
  include Pool_common.Model.String

  let field = Pool_message.Field.Title
  let schema () = schema field ()
end

module Description = struct
  include Pool_common.Model.String

  let field = Pool_message.Field.Description
  let schema () = schema field ()
end

module Url = struct
  include Pool_common.Model.String

  let field = Pool_message.Field.Url
  let schema () = schema field ()
end

module GtxApiKey = struct
  include Common.Model.String

  let field = Pool_message.Field.GtxApiKey
  let schema () = schema field ()
end

module Styles = struct
  type t = File.t [@@deriving eq, show, sexp_of]

  let value m = m
  let id m = m.File.id
  let mime_type m = m.File.mime_type
  let create m = m

  module Write = struct
    include Pool_common.Model.String

    let field = Pool_message.Field.Styles
    let schema () = schema field ()
  end
end

module Icon = struct
  type t = File.t [@@deriving eq, show, sexp_of]

  let value m = m
  let of_file m = m

  module Write = struct
    include Pool_common.Model.String

    let field = Pool_message.Field.Icon
    let schema () = schema field ()
  end
end

module Logos = struct
  type t = File.t list [@@deriving eq, show, sexp_of]

  let value m = m
  let create m = Ok (CCList.map Common.Id.of_string m)

  let schema () =
    Common.Utils.schema_list_decoder
      create
      (CCList.map Common.Id.value)
      Pool_message.Field.TenantLogos
  ;;

  let of_files lst = lst
end

module PartnerLogos = struct
  type t = File.t list [@@deriving eq, show, sexp_of]

  let create m = Ok (CCList.map Common.Id.of_string m)
  let value m = m

  let schema () =
    Common.Utils.schema_list_decoder
      create
      (fun l -> l |> CCList.map Common.Id.value)
      Pool_message.Field.PartnerLogos
  ;;

  let of_files lst = lst
end

module Maintenance = struct
  include Pool_common.Model.Boolean

  let schema = schema Pool_message.Field.TenantMaintenanceFlag
end

module Disabled = struct
  include Pool_common.Model.Boolean

  let schema = schema Pool_message.Field.TenantDisabledFlag
end

type t =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t option
  ; url : Url.t
  ; database_label : Database.Label.t
  ; styles : Styles.t option
  ; icon : Icon.t option
  ; logos : Logos.t
  ; partner_logo : PartnerLogos.t
  ; maintenance : Maintenance.t
  ; disabled : Disabled.t
  ; default_language : Common.Language.t
  ; text_messages_enabled : bool
  ; created_at : CreatedAt.t
  ; updated_at : UpdatedAt.t
  }
[@@deriving eq, show, sexp_of]

let id { id; _ } = id

module Read = struct
  type t =
    { id : Id.t
    ; title : Title.t
    ; description : Description.t option
    ; url : Url.t
    ; database_label : Database.Label.t
    ; styles : Styles.t option
    ; icon : Icon.t option
    ; maintenance : Maintenance.t
    ; disabled : Disabled.t
    ; default_language : Common.Language.t
    ; text_messages_enabled : bool
    ; created_at : CreatedAt.t
    ; updated_at : UpdatedAt.t
    }
  [@@deriving eq, show]
end

module Write = struct
  type t =
    { id : Id.t
    ; title : Title.t
    ; description : Description.t option
    ; url : Url.t
    ; database : Database.t
    ; gtx_api_key : GtxApiKey.t option
    ; styles : Styles.Write.t option
    ; icon : Icon.Write.t option
    ; maintenance : Maintenance.t
    ; disabled : Disabled.t
    ; default_language : Common.Language.t
    ; created_at : CreatedAt.t
    ; updated_at : CreatedAt.t
    }
  [@@deriving eq, show]

  let create title description url database styles icon default_language =
    { id = Id.create ()
    ; title
    ; description
    ; url
    ; database
    ; gtx_api_key = None
    ; styles
    ; icon
    ; maintenance = Maintenance.create false
    ; disabled = Disabled.create false
    ; default_language
    ; created_at = CreatedAt.create ()
    ; updated_at = UpdatedAt.create ()
    }
  ;;
end

module Selection = struct
  type t =
    { url : Url.t
    ; database_label : Database.Label.t
    }
  [@@deriving eq, show]

  let create url database_label = { url; database_label }
  let url ({ url; _ } : t) = url |> Url.value
  let label ({ database_label; _ } : t) = database_label
end

let file_fields =
  Pool_message.Field.([ Styles; Icon ] @ LogoMapping.LogoType.all_fields)
;;
