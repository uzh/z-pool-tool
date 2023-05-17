open CCFun
open Entity

let make_caqti_type caqti_type create value =
  let encode = value %> CCResult.return in
  let decode =
    create %> CCResult.map_err (Utils_to_string.error_to_string Language.En)
  in
  Caqti_type.(custom ~encode ~decode caqti_type)
;;

module Model = struct
  module SelectorType (Core : Entity_base_model.SelectorCoreTypeSig) = struct
    include Entity_base_model.SelectorType (Core)

    let t = make_caqti_type Caqti_type.string create Core.show
  end
end

module Id = struct
  include Id

  let t = make_caqti_type Caqti_type.string (of_string %> CCResult.return) value
end

module Language = Model.SelectorType (Language)

module Version = struct
  include Version

  let t = make_caqti_type Caqti_type.int (of_int %> CCResult.return) value
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

    let t = make_caqti_type Caqti_type.string create value
  end

  module Size = struct
    include Size

    let t = make_caqti_type Caqti_type.int create value
  end

  module Mime = struct
    include Mime

    let t = make_caqti_type Caqti_type.string of_string to_string
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
  module LeadTime = struct
    include Reminder.LeadTime

    let t = make_caqti_type Caqti_type.ptime_span create value
  end

  module SentAt = struct
    include Reminder.SentAt

    let t = make_caqti_type Caqti_type.ptime (create %> CCResult.return) value
  end
end

module ExperimentType = Model.SelectorType (ExperimentType)

module Token = struct
  include Token

  let t =
    make_caqti_type Caqti_type.string CCFun.(of_string %> CCResult.return) value
  ;;
end
