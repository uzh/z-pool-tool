open CCFun.Infix
include Entity

module Id = struct
  include Id
  include Pool_common.Repo.Id
end

module Key = struct
  include Key

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module Argument = struct
  include Argument

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

let t =
  let encode m =
    Ok (m.id, (m.key, (m.argument, (m.created_at, m.updated_at))))
  in
  let decode (id, (key, (argument, (created_at, updated_at)))) =
    let open CCResult in
    Ok { id; key; argument; created_at; updated_at }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Id.t
         (tup2
            Key.t
            (tup2
               (option Argument.t)
               (tup2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t)))))
;;

module EventLog = struct
  include Entity.EventLog

  module Hostname = struct
    include Hostname

    let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
  end

  module Status = struct
    include Status

    let t =
      Caqti_type.(
        custom
          ~encode:(yojson_of_t %> Yojson.Safe.to_string %> CCResult.return)
          ~decode:(Yojson.Safe.from_string %> t_of_yojson %> CCResult.return)
          string)
    ;;
  end

  module Message = struct
    include Message

    let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
  end

  let t =
    let encode m =
      Ok
        ( m.event_id
        , (m.hostname, (m.status, (m.message, (m.created_at, m.updated_at)))) )
    in
    let decode
      (event_id, (hostname, (status, (message, (created_at, updated_at)))))
      =
      let open CCResult in
      Ok { event_id; hostname; status; message; created_at; updated_at }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Id.t
           (tup2
              Hostname.t
              (tup2
                 Status.t
                 (tup2
                    (option Message.t)
                    (tup2
                       Pool_common.Repo.CreatedAt.t
                       Pool_common.Repo.UpdatedAt.t))))))
  ;;
end
