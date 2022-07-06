open Entity

module Id = struct
  include Id

  let t =
    let encode = Utils.fcn_ok value in
    let decode = Utils.fcn_ok of_string in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Language = struct
  include Language

  let encode m = m |> show |> CCResult.pure

  let decode m =
    m
    |> create
    |> CCResult.map_err (fun _ ->
           Locales_en.error_to_string Entity_message.(Decode Field.Language))
  ;;

  let t = Caqti_type.(custom ~encode ~decode string)
end

module Version = struct
  include Version

  let t = Caqti_type.int
end

module CreatedAt = struct
  include CreatedAt

  let t = Caqti_type.ptime
end

module UpdatedAt = struct
  include UpdatedAt

  let t = Caqti_type.ptime
end

module File = struct
  include File

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

module Reminder = struct
  module Subject = struct
    include Reminder.Text

    let t = Caqti_type.string
  end

  module Text = struct
    include Reminder.Text

    let t = Caqti_type.string
  end

  module LeadTime = struct
    include Reminder.LeadTime

    let t = Caqti_type.ptime_span
  end

  module SentAt = struct
    include Reminder.SentAt

    let t = Caqti_type.ptime
  end
end
