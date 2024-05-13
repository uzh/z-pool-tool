open CCFun.Infix
open Entity

let make_caqti_type = Pool_common.Repo.make_caqti_type

module Id = struct
  include Id

  let t = make_caqti_type Caqti_type.string (of_string %> CCResult.return) value
end

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

  let t = make_caqti_type Caqti_type.ptime create value
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
  let open Database.Caqti_encoders in
  let decode
    ( id
    , ( contact
      , ( no_show
        , ( participated
          , ( matches_filter
            , ( canceled_at
              , ( marked_as_deleted
                , ( external_data_id
                  , ( reminder_manually_last_sent_at
                    , (created_at, (updated_at, ())) ) ) ) ) ) ) ) ) )
    =
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
  let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
  let open Schema in
  custom
    ~encode
    ~decode
    Caqti_type.
      [ Pool_common.Repo.Id.t
      ; Contact.Repo.t
      ; option NoShow.t
      ; option Participated.t
      ; MatchesFilter.t
      ; option CanceledAt.t
      ; MarkedAsDeleted.t
      ; option ExternalDataId.t
      ; option Pool_common.Repo.Reminder.SentAt.t
      ; Pool_common.Repo.CreatedAt.t
      ; Pool_common.Repo.UpdatedAt.t
      ]
;;

module Write = struct
  type t =
    { id : Pool_common.Id.t
    ; session_id : Session.Id.t
    ; contact_id : Contact.Id.t
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
    let open Database.Caqti_encoders in
    let decode _ =
      Pool_common.Utils.failwith Pool_message.Error.WriteOnlyModel
    in
    let encode (m : t) : ('a Data.t, string) result =
      Ok
        Data.
          [ m.id
          ; m.session_id
          ; m.contact_id
          ; m.no_show
          ; m.participated
          ; m.matches_filter
          ; m.canceled_at
          ; m.marked_as_deleted
          ; m.external_data_id
          ; m.reminder_manually_last_sent_at
          ; m.created_at
          ; m.updated_at
          ]
    in
    let open Schema in
    custom
      ~encode
      ~decode
      Caqti_type.
        [ Pool_common.Repo.Id.t
        ; Session.Repo.Id.t
        ; Contact.Repo.Id.t
        ; option NoShow.t
        ; option Participated.t
        ; MatchesFilter.t
        ; option CanceledAt.t
        ; MarkedAsDeleted.t
        ; option ExternalDataId.t
        ; option Pool_common.Repo.Reminder.SentAt.t
        ; Pool_common.Repo.CreatedAt.t
        ; Pool_common.Repo.UpdatedAt.t
        ]
  ;;
end

module Public = struct
  open Public

  let t =
    let encode (m : t) =
      Ok (m.id, m.participated, m.canceled_at, m.created_at, m.updated_at)
    in
    let decode (id, participated, canceled_at, created_at, updated_at) =
      let open CCResult in
      Ok { id; participated; canceled_at; created_at; updated_at }
    in
    let open Pool_common.Repo in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t5
           Id.t
           (option Participated.t)
           (option CanceledAt.t)
           CreatedAt.t
           UpdatedAt.t))
  ;;
end

module ExternalDataIdentifier = struct
  open ExternalDataIdentifier

  let t =
    let open Database.Caqti_encoders in
    let decode
      ( external_data_id
      , ( experiment_id
        , ( experiment_title
          , (session_id, (session_start, (session_duration, ()))) ) ) )
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
    let encode _ =
      Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel
    in
    let open Schema in
    custom
      ~encode
      ~decode
      [ ExternalDataId.t
      ; Experiment.Repo.Entity.Id.t
      ; Experiment.Repo.Entity.Title.t
      ; Session.Repo.Id.t
      ; Session.Repo.Start.t
      ; Session.Repo.Duration.t
      ]
  ;;
end

let with_session =
  let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
  let decode = CCResult.return in
  Caqti_type.(custom ~encode ~decode (t2 Session.Repo.t t))
;;
