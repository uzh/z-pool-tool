open Sexplib.Conv
module PoolError = Entity_message

module Id = struct
  type t = string [@@deriving eq, show, sexp_of]

  let create () = Uuidm.v `V4 |> Uuidm.to_string
  let of_string m = m
  let value m = m
end

module Language = struct
  type t =
    | En [@name "EN"]
    | De [@name "DE"]
  [@@deriving eq, show, yojson, sexp_of]

  let code = function
    | En -> "EN"
    | De -> "DE"
  ;;

  let of_string = function
    | "EN" -> Ok En
    | "DE" -> Ok De
    | _ -> Error PoolError.(Invalid Language)
  ;;

  let t =
    let open CCResult in
    (* TODO: Belongs to Repo (search for all caqti types in entities) *)
    Caqti_type.(
      custom
        ~encode:(fun m -> m |> code |> pure)
        ~decode:(fun m -> map_err (fun _ -> "decode language") @@ of_string m)
        string)
  ;;

  let label country_code = country_code |> code |> Utils.Countries.find

  let schema () =
    Pool_common_utils.schema_decoder
      of_string
      code
      PoolError.Language
      "language"
  ;;

  let all () = [ En; De ]
  let all_codes () = [ En; De ] |> CCList.map code

  let field_name_of_t =
    let open Entity_message in
    function
    | En -> LanguageEn
    | De -> LanguageDe
  ;;
end

module Version = struct
  type t = int [@@deriving eq, show, yojson]

  let value m = m
  let create () = 0
  let of_int i = i
  let increment m = m + 1
end

module CreatedAt = struct
  type t = Ptime.t [@@deriving eq, show]

  let create = Ptime_clock.now
  let value m = m
  let sexp_of_t = Utils.Time.ptime_to_sexp
end

module UpdatedAt = struct
  type t = Ptime.t [@@deriving eq, show]

  let create = Ptime_clock.now
  let value m = m
  let sexp_of_t = Utils.Time.ptime_to_sexp
end

module File = struct
  module Name = struct
    type t = string [@@deriving eq, show, sexp_of]

    let create m =
      if CCString.is_empty m then Error PoolError.(Invalid Filename) else Ok m
    ;;

    let value m = m
  end

  module Size = struct
    type t = int [@@deriving eq, show, sexp_of]

    let create m =
      let open CCInt.Infix in
      if m >= CCInt.zero then Ok m else Error PoolError.(Invalid Filesize)
    ;;

    let value m = m
  end

  module Mime = struct
    type t =
      | Css
      | Gif
      | Ico
      | Jpeg
      | Png
      | Svg
      | Webp
    [@@deriving eq, show, sexp_of]

    let of_string = function
      | "text/css" -> Ok Css
      | "image/gif" -> Ok Gif
      | "image/vnd.microsoft.icon" -> Ok Ico
      | "image/jpeg" -> Ok Jpeg
      | "image/png" -> Ok Png
      | "image/svg+xml" -> Ok Svg
      | "image/webp" -> Ok Webp
      | _ -> Error PoolError.(Invalid FileMimeType)
    ;;

    let to_string = function
      | Css -> "text/css"
      | Gif -> "image/gif"
      | Ico -> "image/vnd.microsoft.icon"
      | Jpeg -> "image/jpeg"
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
      | ".png" -> Ok Png
      | ".svg" -> Ok Svg
      | ".webp" -> Ok Webp
      | _ -> Error PoolError.(Invalid FileMimeType)
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
