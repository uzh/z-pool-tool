open CCFun
open Sexplib.Conv
module PoolError = Entity_message
module Model = Entity_base_model

let print = Utils.ppx_printer

(* TODO [aerben] to get more type-safety, every entity should have its own ID *)
module Id = struct
  type t = string [@@deriving eq, show, sexp, yojson]

  let create () = Uuidm.v `V4 |> Uuidm.to_string
  let of_string m = m
  let value m = m

  let schema ?(field = PoolError.Field.Id) () =
    Pool_common_utils.schema_decoder (of_string %> CCResult.return) value field
  ;;
end

module Language = struct
  module Core = struct
    let field = PoolError.Field.Language

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
    let open Entity_message.Field in
    function
    | En -> LanguageEn
    | De -> LanguageDe
  ;;
end

module Version = struct
  type t = int [@@deriving eq, show, yojson, sexp_of]

  let value m = m
  let create () = 0
  let of_int i = i
  let increment m = m + 1
end

module CreatedAt = struct
  include Model.Ptime

  let equal c1 c2 = if Sihl.Configuration.is_test () then true else equal c1 c2
  let create = Ptime_clock.now
end

module UpdatedAt = struct
  include Model.Ptime

  let equal c1 c2 = if Sihl.Configuration.is_test () then true else equal c1 c2
  let create = Ptime_clock.now
end

module File = struct
  module Name = struct
    type t = string [@@deriving eq, show, sexp_of]

    let create m =
      if CCString.is_empty m
      then Error PoolError.(Invalid Field.Filename)
      else Ok m
    ;;

    let value m = m
  end

  module Size = struct
    type t = int [@@deriving eq, show, sexp_of]

    let create m =
      let open CCInt.Infix in
      if m >= CCInt.zero then Ok m else Error PoolError.(Invalid Field.Filesize)
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
      | _ -> Error PoolError.(Invalid Field.FileMimeType)
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
      | _ -> Error PoolError.(Invalid Field.FileMimeType)
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

  let path m =
    Sihl.Web.externalize_path
      (Format.asprintf "/custom/assets/%s/%s" m.id m.name)
  ;;
end

module SortOrder = struct
  module Core = struct
    let field = Entity_message.Field.SortOrder

    type t =
      | Ascending [@name "ASC"] [@printer print "ASC"]
      | Descending [@name "DESC"] [@printer print "DESC"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Entity_base_model.SelectorType (Core)
  include Core

  let default = Ascending
end

module Reminder = struct
  module LeadTime = struct
    type t = Ptime.Span.t [@@deriving eq, show]

    let create m =
      if Ptime.Span.abs m |> Ptime.Span.equal m
      then Ok m
      else Error PoolError.NegativeAmount
    ;;

    let t_of_yojson = Utils_time.ptime_span_of_yojson
    let yojson_of_t = Utils_time.yojson_of_ptime_span
    let value m = m

    let schema () =
      let open CCResult in
      let decode str = Pool_common_utils.Time.parse_time_span str >>= create in
      let encode span = Pool_common_utils.Time.print_time_span span in
      Pool_common_utils.schema_decoder decode encode PoolError.Field.LeadTime
    ;;
  end

  module SentAt = struct
    type t = Ptime.t [@@deriving eq, show]

    let create m = m
    let create_now () = Ptime_clock.now ()
    let value m = m
    let sexp_of_t = Pool_common_utils.Time.ptime_to_sexp
  end
end

module ExperimentType = struct
  module Core = struct
    let field = PoolError.Field.ExperimentType

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
    let field = PoolError.Field.NotifyVia

    type t =
      | Email [@name "email"] [@printer print "email"]
      | TextMessage [@name "text_message"] [@printer print "text_message"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Entity_base_model.SelectorType (Core)
  include Core

  let to_human language m =
    let open Entity_message in
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
end
