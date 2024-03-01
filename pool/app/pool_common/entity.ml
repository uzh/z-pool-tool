open CCFun
open Sexplib.Conv
open Ppx_yojson_conv_lib.Yojson_conv

module Model = struct
  include Entity_base_model
end

let print = Utils.ppx_printer

(* TODO [aerben] to get more type-safety, every entity should have its own ID *)
module Id = struct
  type t = string [@@deriving eq, show, sexp, yojson]

  let create () = Uuidm.v `V4 |> Uuidm.to_string
  let of_string m = m
  let value m = m
  let to_common m = m
  let of_common m = m
  let compare = CCString.compare

  let schema ?(field = Pool_message.Field.Id) () =
    Pool_common_utils.schema_decoder (of_string %> CCResult.return) value field
  ;;

  let sql_select_fragment ~field =
    [%string
      {sql|
        LOWER(CONCAT(
          SUBSTR(HEX(%{field}), 1, 8), '-',
          SUBSTR(HEX(%{field}), 9, 4), '-',
          SUBSTR(HEX(%{field}), 13, 4), '-',
          SUBSTR(HEX(%{field}), 17, 4), '-',
          SUBSTR(HEX(%{field}), 21)
        ))
    |sql}]
  ;;

  let sql_value_fragment name =
    [%string {sql| UNHEX(REPLACE(%{name}, '-', '')) |sql}]
  ;;
end

module Language = struct
  module Core = struct
    let field = Pool_message.Field.Language

    type t =
      | En [@name "EN"] [@printer print "EN"]
      | De [@name "DE"] [@printer print "DE"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Entity_base_model.SelectorType (Core)
  include Core

  let label country_code =
    country_code |> show |> Utils.Countries.find_country_name
  ;;

  let all_codes = all |> CCList.map show

  let field_of_t =
    let open Pool_message.Field in
    function
    | En -> LanguageEn
    | De -> LanguageDe
  ;;
end

module Version = struct
  type t = int [@@deriving eq, ord, show, yojson, sexp_of]

  let value m = m
  let create () = 0
  let of_int i = i
  let increment m = m + 1
end

module CreatedAt = struct
  include Model.Ptime

  let equal a b = Ptime.equal a b || Sihl.Configuration.is_test ()
  let create = Ptime_clock.now
end

module UpdatedAt = struct
  include Model.Ptime

  let equal a b = Ptime.equal a b || Sihl.Configuration.is_test ()
  let create = Ptime_clock.now
end

module File = struct
  module Name = struct
    type t = string [@@deriving eq, show, sexp_of]

    let create m =
      if CCString.is_empty m
      then Error Pool_message.(Error.Invalid Field.Filename)
      else Ok m
    ;;

    let value m = m
  end

  module Size = struct
    type t = int [@@deriving eq, show, sexp_of]

    let create m =
      let open CCInt.Infix in
      if m >= CCInt.zero
      then Ok m
      else Error Pool_message.(Error.Invalid Field.Filesize)
    ;;

    let value m = m
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

  include Entity_base_model.SelectorType (Core)
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

  include Entity_base_model.SelectorType (Core)
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

    include Model.Duration (TimeDurationCore)
  end

  module TextMessageLeadTime = struct
    module TimeDurationCore = struct
      let name = Pool_message.Field.TextMessageLeadTime
    end

    include Model.Duration (TimeDurationCore)
  end

  module SentAt = struct
    type t = Ptime.t [@@deriving eq, show]

    let create m = m
    let create_now () = Ptime_clock.now ()
    let value m = m
    let sexp_of_t = Pool_common_utils.Time.ptime_to_sexp
  end

  module Channel = struct
    module Core = struct
      let field = Pool_message.Field.MessageChannel

      type t =
        | Email [@name "email"] [@printer print "email"]
        | TextMessage [@name "text_message"] [@printer print "text_message"]
      [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
    end

    include Entity_base_model.SelectorType (Core)
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

  include Entity_base_model.SelectorType (Core)
  include Core
end

module VerificationCode = struct
  type t = string [@@deriving eq, show]

  let value m = m
  let of_string m = m

  let create ?(length = 6) () =
    let rec go n acc =
      if n = 0
      then acc
      else
        go
          (n - 1)
          (Format.asprintf "%s%s" acc (Random.int 10 |> CCInt.to_string))
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

  include Entity_base_model.SelectorType (Core)
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
  include Entity_base_model.Boolean

  let init = false
  let schema = schema Pool_message.Field.NotifyContact
end
