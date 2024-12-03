open Sexplib.Conv

let print = Utils.ppx_printer

module Id = Pool_model.Base.Id

module Language = struct
  module Core = struct
    let field = Pool_message.Field.Language

    type t =
      | En [@name "EN"] [@printer print "EN"]
      | De [@name "DE"] [@printer print "DE"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core

  let label country_code = country_code |> show |> Utils.Countries.find_country_name
  let all_codes = all |> CCList.map show

  let field_of_t =
    let open Pool_message.Field in
    function
    | En -> LanguageEn
    | De -> LanguageDe
  ;;

  let read_opt str =
    let str = CCString.uppercase_ascii str in
    try Some (Utils.Json.read_variant t_of_yojson str) with
    | _ -> None
  ;;
end

module Version = struct
  include Pool_model.Base.Integer

  let field = Pool_message.Field.Version
  let schema = schema field CCResult.return
  let create () = 0
  let of_int i = i
  let increment m = m + 1
end

module CreatedAt = struct
  include Pool_model.Base.Ptime

  let field = Pool_message.Field.CreatedAt
  let schema = schema field CCResult.return
  let equal a b = Ptime.equal a b || Sihl.Configuration.is_test ()
end

module UpdatedAt = struct
  include Pool_model.Base.Ptime

  let field = Pool_message.Field.UpdatedAt
  let schema = schema field CCResult.return
  let equal a b = Ptime.equal a b || Sihl.Configuration.is_test ()
end

module File = struct
  module Name = struct
    include Pool_model.Base.String

    let create m =
      if CCString.is_empty m
      then Error Pool_message.(Error.Invalid Field.Filename)
      else Ok m
    ;;
  end

  module Size = struct
    include Pool_model.Base.Integer

    let create m =
      let open CCInt.Infix in
      if m >= CCInt.zero then Ok m else Error Pool_message.(Error.Invalid Field.Filesize)
    ;;
  end

  module Mime = struct
    type t =
      | Css
      | Gif
      | Ico
      | Jpeg
      | Pdf
      | Png
      | Svg
      | Webp
    [@@deriving eq, show, sexp_of]

    let of_string = function
      | "application/pdf" -> Ok Pdf
      | "text/css" -> Ok Css
      | "image/gif" -> Ok Gif
      | "image/vnd.microsoft.icon" -> Ok Ico
      | "image/jpeg" -> Ok Jpeg
      | "image/png" -> Ok Png
      | "image/svg+xml" -> Ok Svg
      | "image/webp" -> Ok Webp
      | _ -> Error Pool_message.(Error.Invalid Field.FileMimeType)
    ;;

    let to_string = function
      | Css -> "text/css"
      | Gif -> "image/gif"
      | Ico -> "image/vnd.microsoft.icon"
      | Jpeg -> "image/jpeg"
      | Pdf -> "application/pdf"
      | Png -> "image/png"
      | Svg -> "image/svg+xml"
      | Webp -> "image/webp"
    ;;

    let of_filename filename =
      match filename |> Filename.extension with
      | ".css" -> Ok Css
      | ".gif" -> Ok Gif
      | ".ico" -> Ok Ico
      | ".jpeg" | ".jpg" -> Ok Jpeg
      | ".pdf" -> Ok Pdf
      | ".png" -> Ok Png
      | ".svg" -> Ok Svg
      | ".webp" -> Ok Webp
      | _ -> Error Pool_message.(Error.Invalid Field.FileMimeType)
    ;;
  end

  type t =
    { id : Id.t
    ; name : Name.t
    ; size : Size.t
    ; mime_type : Mime.t
    ; created_at : CreatedAt.t
    ; updated_at : UpdatedAt.t
    }
  [@@deriving show, eq, sexp_of]

  let id m = m.id
  let size m = m.size
  let path m = Format.asprintf "/custom/assets/%s/%s" m.id m.name
  let externalized_path = CCFun.(path %> Sihl.Web.externalize_path)
end

module SortOrder = struct
  module Core = struct
    let field = Pool_message.Field.SortOrder

    type t =
      | Ascending [@name "ASC"] [@printer print "ASC"]
      | Descending [@name "DESC"] [@printer print "DESC"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core

  let default = Ascending

  let flip = function
    | Ascending -> Descending
    | Descending -> Ascending
  ;;

  let to_query_parts t = [ Core.field, show t ]
end

module MessageChannel = struct
  module Core = struct
    let field = Pool_message.Field.MessageChannel

    type t =
      | Email [@name "email"] [@printer print "email"]
      | TextMessage [@name "text_message"] [@printer print "text_message"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core

  let filtered_channels = function
    | true -> all
    | false -> CCList.remove ~eq:equal ~key:TextMessage all
  ;;
end

module Reminder = struct
  module EmailLeadTime = struct
    module TimeDurationCore = struct
      let name = Pool_message.Field.EmailLeadTime
    end

    include Pool_model.Base.Duration (TimeDurationCore)
  end

  module TextMessageLeadTime = struct
    module TimeDurationCore = struct
      let name = Pool_message.Field.TextMessageLeadTime
    end

    include Pool_model.Base.Duration (TimeDurationCore)
  end

  module SentAt = struct
    include Pool_model.Base.Ptime

    let field = Pool_message.Field.SentAt
    let schema = schema field CCResult.return
  end

  module Channel = struct
    module Core = struct
      let field = Pool_message.Field.MessageChannel

      type t =
        | Email [@name "email"] [@printer print "email"]
        | TextMessage [@name "text_message"] [@printer print "text_message"]
      [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
    end

    include Pool_model.Base.SelectorType (Core)
    include Core

    let filtered_channels = function
      | true -> all
      | false -> CCList.remove ~eq:equal ~key:TextMessage all
    ;;
  end
end

module ExperimentType = struct
  module Core = struct
    let field = Pool_message.Field.ExperimentType

    type t =
      | Lab [@name "lab"] [@printer print "lab"]
      | Online [@name "online"] [@printer print "online"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core
end

module VerificationCode = struct
  include Pool_model.Base.String

  let schema () = schema Pool_message.Field.VerificationCode ()

  let create ?(length = 6) () =
    let rec go n acc =
      if n = 0
      then acc
      else go (n - 1) (Format.asprintf "%s%s" acc (Random.int 10 |> CCInt.to_string))
    in
    go length ""
  ;;
end

module NotifyVia = struct
  module Core = struct
    let field = Pool_message.Field.NotifyVia

    type t =
      | Email [@name "email"] [@printer print "email"]
      | TextMessage [@name "text_message"] [@printer print "text_message"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core

  let to_human language m =
    let open Pool_message in
    let show field =
      CCString.capitalize_ascii
      @@
      match language with
      | Language.De -> Locales_de.field_to_string field
      | Language.En -> Locales_en.field_to_string field
    in
    match m with
    | Email -> Field.Email |> show
    | TextMessage -> Field.TextMessage |> show
  ;;

  let checked_by_default = function
    | Email -> true
    | TextMessage -> false
  ;;
end

module NotifyContact = struct
  include Pool_model.Base.Boolean

  let init = false
  let schema = schema Pool_message.Field.NotifyContact
end
