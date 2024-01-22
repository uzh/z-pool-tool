open Entity

module NoShow = struct
  include NoShow

  let t = Caqti_type.bool
end

module Participated = struct
  include Participated

  let t = Caqti_type.bool
end

module MatchesFilter = struct
  include MatchesFilter

  let t = Caqti_type.bool
end

module CanceledAt = struct
  include CanceledAt

  let t = Caqti_type.ptime
end

module MarkedAsDeleted = struct
  include MarkedAsDeleted

  let t = Caqti_type.bool
end

module ExternalDataId = struct
  include ExternalDataId

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

let t =
  let encode _ =
    failwith
      Pool_common.(Message.ReadOnlyModel |> Utils.error_to_string Language.En)
  in
  let decode
    ( id
    , ( contact
      , ( no_show
        , ( participated
          , ( matches_filter
            , ( canceled_at
              , ( marked_as_deleted
                , ( external_data_id
                  , (reminder_manually_last_sent_at, (created_at, updated_at))
                  ) ) ) ) ) ) ) )
    =
    let open CCResult in
    Ok
      { id
      ; contact
      ; no_show
      ; participated
      ; matches_filter = MatchesFilter.create matches_filter
      ; canceled_at = CCOption.map CanceledAt.value canceled_at
      ; marked_as_deleted
      ; external_data_id
      ; reminder_manually_last_sent_at
      ; custom_fields = None
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         Pool_common.Repo.Id.t
         (t2
            Contact.Repo.Entity.t
            (t2
               (option NoShow.t)
               (t2
                  (option Participated.t)
                  (t2
                     MatchesFilter.t
                     (t2
                        (option CanceledAt.t)
                        (t2
                           MarkedAsDeleted.t
                           (t2
                              (option ExternalDataId.t)
                              (t2
                                 (option Pool_common.Repo.Reminder.SentAt.t)
                                 (t2
                                    Pool_common.Repo.CreatedAt.t
                                    Pool_common.Repo.UpdatedAt.t)))))))))))
;;

module Write = struct
  type t =
    { id : Pool_common.Id.t
    ; session_id : Session.Id.t
    ; contact_id : Pool_common.Id.t
    ; no_show : NoShow.t option
    ; participated : Participated.t option
    ; matches_filter : MatchesFilter.t
    ; canceled_at : CanceledAt.t option
    ; marked_as_deleted : MarkedAsDeleted.t
    ; external_data_id : ExternalDataId.t option
    ; reminder_manually_last_sent_at : Pool_common.Reminder.SentAt.t option
    ; created_at : Pool_common.CreatedAt.t
    ; updated_at : Pool_common.UpdatedAt.t
    }

  let of_entity (session_id : Session.Id.t) (m : Entity.t) : t =
    { id = m.id
    ; session_id
    ; contact_id = Contact.id m.contact
    ; no_show = m.no_show
    ; participated = m.participated
    ; matches_filter = m.matches_filter
    ; canceled_at = m.canceled_at
    ; marked_as_deleted = m.marked_as_deleted
    ; external_data_id = m.external_data_id
    ; reminder_manually_last_sent_at = m.reminder_manually_last_sent_at
    ; created_at = m.created_at
    ; updated_at = m.updated_at
    }
  ;;

  let t =
    let encode (m : t) =
      Ok
        ( m.id
        , ( m.session_id
          , ( m.contact_id
            , ( m.no_show
              , ( m.participated
                , ( m.matches_filter
                  , ( m.canceled_at
                    , ( m.marked_as_deleted
                      , ( m.external_data_id
                        , ( m.reminder_manually_last_sent_at
                          , (m.created_at, m.updated_at) ) ) ) ) ) ) ) ) ) )
    in
    let decode _ =
      failwith
        Pool_common.(
          Message.WriteOnlyModel |> Utils.error_to_string Language.En)
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Pool_common.Repo.Id.t
           (t2
              Session.Repo.Id.t
              (t2
                 Pool_common.Repo.Id.t
                 (t2
                    (option NoShow.t)
                    (t2
                       (option Participated.t)
                       (t2
                          MatchesFilter.t
                          (t2
                             (option CanceledAt.t)
                             (t2
                                MarkedAsDeleted.t
                                (t2
                                   (option ExternalDataId.t)
                                   (t2
                                      (option
                                         Pool_common.Repo.Reminder.SentAt.t)
                                      (t2
                                         Pool_common.Repo.CreatedAt.t
                                         Pool_common.Repo.UpdatedAt.t))))))))))))
  ;;
end

module Public = struct
  open Public

  let t =
    let encode (m : t) = Ok (m.id, m.canceled_at) in
    let decode (id, canceled_at) =
      let open CCResult in
      Ok { id; canceled_at }
    in
    Caqti_type.(
      custom ~encode ~decode (t2 Pool_common.Repo.Id.t (option CanceledAt.t)))
  ;;
end

module ExternalDataIdentifier = struct
  open ExternalDataIdentifier

  let t =
    let encode _ =
      failwith
        Pool_common.(Message.ReadOnlyModel |> Utils.error_to_string Language.En)
    in
    let decode
      ( external_data_id
      , ( experiment_id
        , (experiment_title, (session_id, (session_start, session_duration))) )
      )
      =
      Ok
        { external_data_id
        ; experiment_id
        ; experiment_title
        ; session_id
        ; session_start
        ; session_duration
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           ExternalDataId.t
           (t2
              Experiment.Repo.Entity.Id.t
              (t2
                 Experiment.Repo.Entity.Title.t
                 (t2
                    Session.Repo.Id.t
                    (t2 Session.Repo.Start.t Session.Repo.Duration.t))))))
  ;;
end
