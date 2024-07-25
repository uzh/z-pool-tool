open CCFun
module JobName = Pool_common.Repo.Model.SelectorType (Entity.JobName)
module Status = Pool_common.Repo.Model.SelectorType (Entity.Status)

module Id = struct
  include Entity.Id

  let t =
    Database.Repo.make_caqti_type
      Caqti_type.string
      (of_string %> CCResult.return)
      value
  ;;
end

module PersistedAt = struct
  include Entity.PersistedAt

  let t = Caqti_type.ptime
end

module RunAt = struct
  include Entity.PersistedAt

  let t = Caqti_type.ptime
end

module PolledAt = struct
  include Entity.PersistedAt

  let t = Caqti_type.ptime
end

module HandledAt = struct
  include Entity.PersistedAt

  let t = Caqti_type.ptime
end

module ErrorAt = struct
  include Entity.ErrorAt

  let t = Caqti_type.ptime
end

module Instance = struct
  include Entity.Instance

  let t =
    let open Database.Caqti_encoders in
    let decode
      ( id
      , ( name
        , ( input
          , ( message_template
            , ( tries
              , ( max_tries
                , ( run_at
                  , ( status
                    , ( persisted_at
                      , ( polled_at
                        , ( handled_at
                          , ( last_error
                            , (last_error_at, (database_label, (clone_of, ())))
                            ) ) ) ) ) ) ) ) ) ) ) )
      =
      Ok
        { id
        ; name
        ; input
        ; message_template
        ; run_at
        ; tries
        ; max_tries
        ; status
        ; persisted_at
        ; polled_at
        ; handled_at
        ; last_error
        ; last_error_at
        ; database_label
        ; clone_of
        }
    in
    let encode (m : t) : ('a Data.t, string) result =
      Ok
        Data.
          [ m.id
          ; m.name
          ; m.input
          ; m.message_template
          ; m.tries
          ; m.max_tries
          ; m.run_at
          ; m.status
          ; m.persisted_at
          ; m.polled_at
          ; m.handled_at
          ; m.last_error
          ; m.last_error_at
          ; m.database_label
          ; m.clone_of
          ]
    in
    let open Schema in
    custom
      ~encode
      ~decode
      Caqti_type.
        [ Id.t
        ; JobName.t
        ; string
        ; option string
        ; int
        ; int
        ; RunAt.t
        ; Status.t
        ; PersistedAt.t
        ; option PolledAt.t
        ; option HandledAt.t
        ; option string
        ; option ErrorAt.t
        ; Database.Repo.Label.t
        ; option Id.t
        ]
  ;;
end

module Mapping = struct
  open Entity_mapping

  module Write = struct
    include Write

    let t =
      let encode m = Ok (m.queue_uuid, m.entity_uuid) in
      let decode _ =
        Pool_common.Utils.failwith Pool_message.Error.WriteOnlyModel
      in
      Caqti_type.(custom ~encode ~decode (t2 Id.t Pool_common.Repo.Id.t))
    ;;
  end

  let t =
    let open Pool_common in
    let encode (_ : Entity_mapping.t) =
      Utils.failwith Pool_message.Error.ReadOnlyModel
    in
    let decode (entity_uuid, job) = Ok { Entity_mapping.entity_uuid; job } in
    Caqti_type.(custom ~encode ~decode (t2 Pool_common.Repo.Id.t Instance.t))
  ;;
end
