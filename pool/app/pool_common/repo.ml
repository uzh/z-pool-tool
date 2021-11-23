module Id = struct
  include Entity.Id

  let t = Caqti_type.string
end

module Database = struct
  include Entity.Database

  module Url = struct
    include Url

    let t = Caqti_type.string
  end

  module Label = struct
    include Label

    let t =
      let open CCResult in
      Caqti_type.(
        custom
          ~encode:pure
          ~decode:(fun m -> map_err (fun _ -> "decode label") @@ create m)
          string)
    ;;
  end

  let t =
    let open CCResult in
    let encode m = Ok (m.url, m.label) in
    let decode (url, label) =
      map_err (fun _ -> "decode database")
      @@ let* url = Url.create url in
         let* label = Label.create label in
         Ok { url; label }
    in
    Caqti_type.(custom ~encode ~decode (tup2 Url.t Label.t))
  ;;
end

module ChangeSet = struct
  include Entity.ChangeSet

  module Version = struct
    include Version

    let t = Caqti_type.int
  end
end

module CreatedAt = struct
  include Entity.CreatedAt

  let t = Caqti_type.ptime
end

module UpdatedAt = struct
  include Entity.UpdatedAt

  let t = Caqti_type.ptime
end

module File = struct
  include Entity.File

  module Name = struct
    include Name

    let t = Caqti_type.string
  end

  module Size = struct
    include Size

    let t = Caqti_type.int
  end

  module Mime = struct
    include Mime

    let t =
      let open CCResult in
      Caqti_type.(
        custom
          ~encode:(fun m -> m |> to_string |> pure)
          ~decode:(fun m ->
            map_err (fun _ -> "decode mime type") @@ of_string m)
          string)
    ;;
  end

  let t =
    let encode m =
      Ok (m.id, (m.name, (m.size, (m.mime_type, (m.created_at, m.updated_at)))))
    in
    let decode (id, (name, (size, (mime_type, (created_at, updated_at))))) =
      Ok { id; name; size; mime_type; created_at; updated_at }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2 Id.t (tup2 Name.t (tup2 Size.t (tup2 Mime.t (tup2 ptime ptime))))))
  ;;
end
