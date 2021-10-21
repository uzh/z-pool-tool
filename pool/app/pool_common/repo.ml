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
      let encode m = Ok m in
      Caqti_type.(custom ~encode ~decode:create string)
    ;;
  end

  let t =
    let encode m = Ok (m.url, m.label) in
    let decode (url, label) =
      let open CCResult in
      let* url = Url.create url in
      let* label = Label.create label in
      Ok { url; label }
    in
    Caqti_type.(custom ~encode ~decode (tup2 Url.t Label.t))
  ;;
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

  let t =
    let encode m =
      Ok (m.id, (m.filename, (m.mime_type, (m.created_at, m.updated_at))))
    in
    let decode (id, (filename, (mime_type, (created_at, updated_at)))) =
      Ok { id; filename; mime_type; created_at; updated_at }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2 string (tup2 string (tup2 string (tup2 ptime ptime)))))
  ;;
end
