open Pool_common_utils
module PoolError = Entity_message

module Id = struct
  type t = string [@@deriving eq, show]

  let create () = Uuidm.create `V4 |> Uuidm.to_string
  let of_string m = m
  let value m = m
end

module Language = struct
  type t =
    | En [@name "EN"]
    | De [@name "DE"]
  [@@deriving eq, show, yojson]

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
    Conformist.custom
      (Pool_common_utils.schema_decoder of_string PoolError.Language)
      (fun l -> [ code l ])
      "default_language"
  ;;

  let all () = [ En; De ]
  let all_codes () = [ En; De ] |> CCList.map code
end

module Database = struct
  module Url = struct
    type t = string [@@deriving eq]

    let create url =
      if CCString.is_empty url
      then Error PoolError.(Invalid DatabaseUrl)
      else Ok url
    ;;

    let schema () =
      Conformist.custom
        (schema_decoder create PoolError.DatabaseUrl)
        CCList.pure
        "database_url"
    ;;
  end

  module Label = struct
    type t = string [@@deriving eq, show]

    let value m = m
    let of_string m = m

    let create label =
      if CCString.is_empty label || String.contains label ' '
      then Error PoolError.(Invalid DatabaseLabel)
      else Ok label
    ;;

    let schema () =
      Conformist.custom
        (schema_decoder create PoolError.DatabaseLabel)
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
        |> CCFun.flip CCOption.bind CCInt.of_string
        |> CCOption.value ~default:10)
      model.label
      model.url
  ;;

  let read_pool m = m.label
  let pp formatter m = Label.pp formatter m.label
end

module ChangeSet = struct
  module Version = struct
    type t = int [@@deriving eq, show, yojson]

    let value m = m
    let create () = 0
    let of_int i = i
    let increment m = m + 1
  end

  type t = (string * Version.t) list [@@deriving eq, show, yojson]

  let value m = m
  let create version_list = version_list
  let to_string m = m |> yojson_of_t |> Yojson.Safe.to_string
  let of_string m = m |> Yojson.Safe.from_string |> t_of_yojson
  let empty : t = []

  let find_version set key =
    CCList.assoc_opt ~eq:CCString.equal key set |> CCOption.map Version.value
  ;;

  let check_for_update (old_set : t) (current_set : t) new_data =
    let go_change = find_version old_set in
    let go_form key = CCList.assoc_opt ~eq:CCString.equal key new_data in
    let errors =
      CCList.filter_map
        (fun (key, cv) ->
          match go_change key, go_form key with
          | Some rv, Some _ ->
            if cv > rv
            then
              let open PoolError in
              let err_field =
                match key with
                | "firstname_version" -> Firstname
                | "lastname_version" -> Lastname
                | _ -> ChangeSetVersion
              in
              Some (Error (MeantimeUpdate err_field))
            else None
          | _ -> None)
        current_set
    in
    if CCList.is_empty errors then Ok new_data else CCList.hd errors
  ;;
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

    let create m =
      if CCString.is_empty m then Error PoolError.(Invalid Filename) else Ok m
    ;;

    let value m = m
  end

  module Size = struct
    type t = int [@@deriving eq, show]

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
    [@@deriving eq, show]

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
  [@@deriving show, eq]

  let id m = m.id
  let size m = m.size

  let path m =
    Sihl.Web.externalize_path (Format.asprintf "/assets/%s/%s" m.id m.name)
  ;;
end
