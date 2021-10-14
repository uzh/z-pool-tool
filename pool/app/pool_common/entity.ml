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
