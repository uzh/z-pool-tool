open Sexplib.Conv
module Common = Pool_common
module Database = Database
module Id = Common.Id
module CreatedAt = Common.CreatedAt
module UpdatedAt = Common.UpdatedAt
module File = Common.File
module LogoMapping = Entity_logo_mapping

module Title = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Title
  let schema () = schema field ()
end

module Description = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Description
  let schema () = schema field ()
end

module Url = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Url
  let schema () = schema field ()
end

module GtxApiKey = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.GtxApiKey
  let schema () = schema field ()
end

module GtxSender = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.GtxSender

  (* The GTX Rest API does not allow more than 11 characters *)
  let validation str =
    if CCString.length str > 11
    then Error (Pool_message.Error.MaxLength 11)
    else Ok str
  ;;

  let schema () = schema ~validation field ()
end

module Styles = struct
  type t = File.t [@@deriving eq, show, sexp_of]

  let value m = m
  let id m = m.File.id
  let mime_type m = m.File.mime_type
  let create m = m

  module Write = struct
    include Pool_model.Base.String

    let field = Pool_message.Field.Styles
    let schema () = schema field ()
  end
end

module Icon = struct
  type t = File.t [@@deriving eq, show, sexp_of]

  let value m = m
  let of_file m = m

  module Write = struct
    include Pool_model.Base.String

    let field = Pool_message.Field.Icon
    let schema () = schema field ()
  end
end

module Logos = struct
  type t = File.t list [@@deriving eq, show, sexp_of]

  let value m = m
  let create m = Ok (CCList.map Common.Id.of_string m)

  let schema () =
    Pool_conformist.schema_list_decoder
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
    Pool_conformist.schema_list_decoder
      create
      (CCList.map Common.Id.value)
      Pool_message.Field.PartnerLogos
  ;;

  let of_files lst = lst
end

module EmailLogo = struct
  type t = File.t [@@deriving eq, show, sexp_of]

  let value m = m

  module Write = struct
    include Pool_model.Base.String

    let field = Pool_message.Field.EmailLogo
    let schema () = schema field ()
  end
end

type t =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t option
  ; url : Url.t
  ; database_label : Database.Label.t
  ; gtx_sender : GtxSender.t
  ; styles : Styles.t option
  ; icon : Icon.t option
  ; logos : Logos.t
  ; partner_logo : PartnerLogos.t
  ; email_logo : EmailLogo.t option
  ; status : Database.Status.t
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
    ; gtx_sender : GtxSender.t
    ; styles : Styles.t option
    ; icon : Icon.t option
    ; email_logo : EmailLogo.t option
    ; status : Database.Status.t
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
    ; database_label : Database.Label.t
    ; gtx_sender : GtxSender.t
    ; gtx_api_key : GtxApiKey.t option
    ; styles : Styles.Write.t option
    ; email_logo : EmailLogo.Write.t option
    ; icon : Icon.Write.t option
    ; default_language : Common.Language.t
    ; created_at : CreatedAt.t
    ; updated_at : UpdatedAt.t
    }
  [@@deriving eq, show]

  let create
        title
        description
        url
        database_label
        gtx_sender
        styles
        icon
        email_logo
        default_language
    =
    { id = Id.create ()
    ; title
    ; description
    ; url
    ; database_label
    ; gtx_api_key = None
    ; gtx_sender
    ; styles
    ; icon
    ; email_logo
    ; default_language
    ; created_at = CreatedAt.create_now ()
    ; updated_at = UpdatedAt.create_now ()
    }
  ;;

  let database_label (m : t) = m.database_label
end

let file_fields =
  Pool_message.Field.(
    [ Styles; Icon; EmailLogo ] @ LogoMapping.LogoType.all_fields)
;;
