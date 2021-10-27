module Id = struct
  type t = string [@@deriving eq, show]

  let create () = Uuidm.create `V4 |> Uuidm.to_string
  let of_string m = m
  let value m = m
end

module Database = struct
  module Url = struct
    type t = string [@@deriving eq]

    let create url =
      if String.length url <= 0 then Error "Invalid database url!" else Ok url
    ;;

    let schema () =
      Conformist.custom
        (Utils.schema_decoder create "database url")
        CCList.pure
        "database_url"
    ;;
  end

  module Label = struct
    type t = string [@@deriving eq, show]

    let value m = m
    let of_string m = m

    let create label =
      if String.length label <= 0 || String.contains label ' '
      then Error "Invalid database label!"
      else Ok label
    ;;

    let schema () =
      Conformist.custom
        (Utils.schema_decoder create "database label")
        CCList.pure
        "database_label"
    ;;
  end

  type t =
    { url : Url.t
    ; label : Label.t
    }
  [@@deriving eq]

  let create url label = Ok { url; label }

  let add_pool model =
    Sihl.Database.add_pool
      ~pool_size:
        (Sihl.Configuration.read_string "DATABASE_POOL_SIZE"
        |> CCFun.flip CCOpt.bind CCInt.of_string
        |> CCOpt.value ~default:10)
      model.label
      model.url
  ;;

  let read_pool m = m.label
  let pp formatter m = Label.pp formatter m.label
end

module CreatedAt = struct
  type t = Ptime.t [@@deriving eq, show]

  let create = Ptime_clock.now
  let value m = m
end

module UpdatedAt = struct
  type t = Ptime.t [@@deriving eq, show]

  let create = Ptime_clock.now
  let value m = m
end

module File = struct
  module Name = struct
    type t = string [@@deriving eq, show]

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
    [@@deriving eq, show]

    let of_string = function
      | "text/css" -> Ok Css
      | "image/gif" -> Ok Gif
      | "image/vnd.microsoft.icon" -> Ok Ico
      | "image/jpeg" -> Ok Jpeg
      | "image/png" -> Ok Png
      | "image/svg+xml" -> Ok Svg
      | "image/webp" -> Ok Webp
      | _ -> Error "Invalid mime type provided"
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
      | ".git" -> Ok Gif
      | ".ico" -> Ok Ico
      | ".jpeg" | ".jpg" -> Ok Jpeg
      | ".png" -> Ok Png
      | ".svg" -> Ok Svg
      | ".webp" -> Ok Webp
      | _ -> Error "Invalid mime type provided"
    ;;
  end

  type t =
    { id : Id.t
    ; name : Name.t
    ; mime_type : Mime.t
    ; created_at : CreatedAt.t
    ; updated_at : UpdatedAt.t
    }
  [@@deriving show, eq]

  let id m = m.id

  let path m =
    Sihl.Web.externalize_path (Format.asprintf "/assets/%s/%s" m.id m.name)
  ;;
end
