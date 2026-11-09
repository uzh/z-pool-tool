open CCFun.Infix
include Entity

module Id = struct
  include Id
  include Pool_common.Repo.Id
end

module Job = struct
  include Job

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string of_string to_string
end

let t =
  let encode m = Ok (m.id, (m.job, (m.created_at, m.updated_at))) in
  let decode (id, (job, (created_at, updated_at))) =
    let open CCResult in
    Ok { id; job; created_at; updated_at }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2 Id.t (t2 Job.t (t2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t))))
;;

(* Keeps the job undecoded, so a single unknown job (e.g. a removed variant) does not
   fail the whole query *)
module Raw = struct
  type t = Id.t * string * Pool_common.CreatedAt.t * Pool_common.UpdatedAt.t

  let to_entity ((id, job, created_at, updated_at) : t) =
    Job.of_string job |> CCResult.map (fun job -> { id; job; created_at; updated_at })
  ;;

  let t =
    let open Database.Caqti_encoders in
    let decode (id, (job, (created_at, (updated_at, ())))) =
      Ok (id, job, created_at, updated_at)
    in
    let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
    let open Schema in
    custom
      ~encode
      ~decode
      Caqti_type.
        [ Id.t; string; Pool_common.Repo.CreatedAt.t; Pool_common.Repo.UpdatedAt.t ]
  ;;
end

module EventLog = struct
  include Entity.EventLog

  module ServiceIdentifier = struct
    include ServiceIdentifier

    let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
  end

  module Status = struct
    include Status

    let t =
      Caqti_type.(
        custom
          ~encode:(Status.show %> CCResult.return)
          ~decode:
            (of_string %> CCResult.map_err Pool_common.(Utils.error_to_string Language.En))
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
        , (m.service_identifier, (m.status, (m.message, (m.created_at, m.updated_at)))) )
    in
    let decode
          (event_id, (service_identifier, (status, (message, (created_at, updated_at)))))
      =
      let open CCResult in
      Ok { event_id; service_identifier; status; message; created_at; updated_at }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Id.t
           (t2
              ServiceIdentifier.t
              (t2
                 Status.t
                 (t2
                    (option Message.t)
                    (t2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t))))))
  ;;
end
