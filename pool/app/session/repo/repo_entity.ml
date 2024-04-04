open Entity
module RepoId = Pool_common.Repo.Id
module Reminder = Pool_common.Reminder
module RepoReminder = Pool_common.Repo.Reminder
module Experiment = Experiment.Repo.Entity

module Id = struct
  include RepoId
end

module Start = struct
  include Entity.Start

  let t = Caqti_type.ptime
end

module Duration = struct
  include Entity.Duration

  let t = Caqti_type.ptime_span
end

(* TODO [aerben] these circumvent our smart constructors, good? *)
let t =
  let encode m =
    let location = Pool_location.Repo.of_entity m.location in
    Ok
      ( m.id
      , ( m.follow_up_to
        , ( m.has_follow_ups
          , ( m.start
            , ( m.duration
              , ( m.internal_description
                , ( m.public_description
                  , ( m.max_participants
                    , ( m.min_participants
                      , ( m.overbook
                        , ( m.email_reminder_lead_time
                          , ( m.email_reminder_sent_at
                            , ( m.text_message_reminder_lead_time
                              , ( m.text_message_reminder_sent_at
                                , ( m.assignment_count
                                  , ( m.no_show_count
                                    , ( m.participant_count
                                      , ( m.closed_at
                                        , ( m.canceled_at
                                          , ( m.created_at
                                            , ( m.updated_at
                                              , (m.experiment, location) ) ) )
                                        ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
  in
  let decode
    ( id
    , ( follow_up_to
      , ( has_follow_ups
        , ( start
          , ( duration
            , ( internal_description
              , ( public_description
                , ( max_participants
                  , ( min_participants
                    , ( overbook
                      , ( email_reminder_lead_time
                        , ( email_reminder_sent_at
                          , ( text_message_reminder_lead_time
                            , ( text_message_reminder_sent_at
                              , ( assignment_count
                                , ( no_show_count
                                  , ( participant_count
                                    , ( closed_at
                                      , ( canceled_at
                                        , ( created_at
                                          , (updated_at, (experiment, location))
                                          ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
    )
    =
    let location = Pool_location.Repo.to_entity location [] in
    Ok
      { id
      ; follow_up_to
      ; has_follow_ups
      ; start
      ; duration
      ; internal_description
      ; public_description
      ; location
      ; max_participants
      ; min_participants
      ; overbook
      ; email_reminder_lead_time
      ; email_reminder_sent_at
      ; text_message_reminder_lead_time
      ; text_message_reminder_sent_at
      ; assignment_count
      ; no_show_count
      ; participant_count
      ; closed_at
      ; canceled_at
      ; experiment
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         RepoId.t
         (t2
            (option RepoId.t)
            (t2
               bool
               (t2
                  Start.t
                  (t2
                     Duration.t
                     (t2
                        (option string)
                        (t2
                           (option string)
                           (t2
                              int
                              (t2
                                 int
                                 (t2
                                    int
                                    (t2
                                       (option RepoReminder.EmailLeadTime.t)
                                       (t2
                                          (option RepoReminder.SentAt.t)
                                          (t2
                                             (option
                                                RepoReminder.TextMessageLeadTime
                                                .t)
                                             (t2
                                                (option RepoReminder.SentAt.t)
                                                (t2
                                                   int
                                                   (t2
                                                      int
                                                      (t2
                                                         int
                                                         (t2
                                                            (option ptime)
                                                            (t2
                                                               (option ptime)
                                                               (t2
                                                                  ptime
                                                                  (t2
                                                                     ptime
                                                                     (t2
                                                                        Experiment
                                                                        .t
                                                                        Pool_location
                                                                        .Repo
                                                                        .t)))))))))))))))))))))))
;;

module Write = struct
  type t =
    { id : Id.t
    ; follow_up_to : Id.t option
    ; start : Start.t
    ; duration : Ptime.Span.t
    ; internal_description : InternalDescription.t option
    ; public_description : PublicDescription.t option
    ; location_id : Pool_location.Id.t
    ; max_participants : ParticipantAmount.t
    ; min_participants : ParticipantAmount.t
    ; overbook : ParticipantAmount.t
    ; email_reminder_lead_time : Reminder.EmailLeadTime.t option
    ; email_reminder_sent_at : Reminder.SentAt.t option
    ; text_message_reminder_lead_time : Reminder.TextMessageLeadTime.t option
    ; text_message_reminder_sent_at : Reminder.SentAt.t option
    ; closed_at : Ptime.t option
    ; canceled_at : Ptime.t option
    }

  let entity_to_write
    (Entity.
       { id
       ; follow_up_to
       ; start
       ; duration
       ; internal_description
       ; public_description
       ; location
       ; max_participants
       ; min_participants
       ; overbook
       ; email_reminder_lead_time
       ; email_reminder_sent_at
       ; text_message_reminder_lead_time
       ; text_message_reminder_sent_at
       ; closed_at
       ; canceled_at
       ; _
       } :
      Entity.t)
    =
    { id
    ; follow_up_to
    ; start
    ; duration
    ; internal_description
    ; public_description
    ; location_id = location.Pool_location.id
    ; max_participants
    ; min_participants
    ; overbook
    ; email_reminder_lead_time
    ; email_reminder_sent_at
    ; text_message_reminder_lead_time
    ; text_message_reminder_sent_at
    ; closed_at
    ; canceled_at
    }
  ;;

  let t =
    let encode (m : t) =
      Ok
        ( m.id
        , ( m.follow_up_to
          , ( m.start
            , ( m.duration
              , ( m.internal_description
                , ( m.public_description
                  , ( m.location_id
                    , ( m.max_participants
                      , ( m.min_participants
                        , ( m.overbook
                          , ( m.email_reminder_lead_time
                            , ( m.email_reminder_sent_at
                              , ( m.text_message_reminder_lead_time
                                , ( m.text_message_reminder_sent_at
                                  , (m.closed_at, m.canceled_at) ) ) ) ) ) ) )
                    ) ) ) ) ) ) )
    in
    let decode
      ( id
      , ( follow_up_to
        , ( start
          , ( duration
            , ( internal_description
              , ( public_description
                , ( location_id
                  , ( max_participants
                    , ( min_participants
                      , ( overbook
                        , ( email_reminder_lead_time
                          , ( email_reminder_sent_at
                            , ( text_message_reminder_lead_time
                              , ( text_message_reminder_sent_at
                                , (closed_at, canceled_at) ) ) ) ) ) ) ) ) ) )
            ) ) ) )
      =
      Ok
        { id
        ; follow_up_to
        ; start
        ; duration
        ; internal_description
        ; public_description
        ; location_id
        ; max_participants
        ; min_participants
        ; overbook
        ; email_reminder_lead_time
        ; email_reminder_sent_at
        ; text_message_reminder_lead_time
        ; text_message_reminder_sent_at
        ; closed_at
        ; canceled_at
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           RepoId.t
           (t2
              (option RepoId.t)
              (t2
                 Start.t
                 (t2
                    Duration.t
                    (t2
                       (option string)
                       (t2
                          (option string)
                          (t2
                             Pool_location.Repo.Id.t
                             (t2
                                int
                                (t2
                                   int
                                   (t2
                                      int
                                      (t2
                                         (option
                                            Pool_common.Repo.Reminder
                                            .EmailLeadTime
                                            .t)
                                         (t2
                                            (option
                                               Pool_common.Repo.Reminder.SentAt
                                               .t)
                                            (t2
                                               (option
                                                  Pool_common.Repo.Reminder
                                                  .TextMessageLeadTime
                                                  .t)
                                               (t2
                                                  (option
                                                     Pool_common.Repo.Reminder
                                                     .SentAt
                                                     .t)
                                                  (t2
                                                     (option ptime)
                                                     (option ptime)))))))))))))))))
  ;;
end

module Public = struct
  type t =
    { id : Entity.Id.t
    ; follow_up_to : Entity.Id.t option
    ; start : Entity.Start.t
    ; duration : Ptime.Span.t
    ; description : Entity.PublicDescription.t option
    ; location_id : Pool_location.Id.t
    ; max_participants : Entity.ParticipantAmount.t
    ; min_participants : Entity.ParticipantAmount.t
    ; overbook : Entity.ParticipantAmount.t
    ; assignment_count : Entity.AssignmentCount.t
    ; canceled_at : Ptime.t option
    }
  [@@deriving eq, show]

  let of_entity (m : Entity.Public.t) : t =
    { id = m.Entity.Public.id
    ; follow_up_to = m.Entity.Public.follow_up_to
    ; start = m.Entity.Public.start
    ; duration = m.Entity.Public.duration
    ; description = m.Entity.Public.description
    ; location_id = m.Entity.Public.location.Pool_location.id
    ; max_participants = m.Entity.Public.max_participants
    ; min_participants = m.Entity.Public.min_participants
    ; overbook = m.Entity.Public.overbook
    ; assignment_count = m.Entity.Public.assignment_count
    ; canceled_at = m.Entity.Public.canceled_at
    }
  ;;

  let to_entity (m : t) location : Entity.Public.t =
    Entity.Public.
      { id = m.id
      ; follow_up_to = m.follow_up_to
      ; start = m.start
      ; duration = m.duration
      ; description = m.description
      ; location
      ; max_participants = m.max_participants
      ; min_participants = m.min_participants
      ; overbook = m.overbook
      ; assignment_count = m.assignment_count
      ; canceled_at = m.canceled_at
      }
  ;;

  let t =
    let encode (m : t) =
      Ok
        ( m.id
        , ( m.follow_up_to
          , ( m.start
            , ( m.duration
              , ( m.description
                , ( m.location_id
                  , ( m.max_participants
                    , ( m.min_participants
                      , (m.overbook, (m.assignment_count, m.canceled_at)) ) ) )
                ) ) ) ) )
    in
    let decode
      ( id
      , ( follow_up_to
        , ( start
          , ( duration
            , ( description
              , ( location_id
                , ( max_participants
                  , ( min_participants
                    , (overbook, (assignment_count, canceled_at)) ) ) ) ) ) ) )
      )
      =
      Ok
        { id
        ; follow_up_to
        ; start
        ; duration
        ; description
        ; location_id
        ; max_participants
        ; min_participants
        ; overbook
        ; assignment_count
        ; canceled_at
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           RepoId.t
           (t2
              (option RepoId.t)
              (t2
                 Start.t
                 (t2
                    Duration.t
                    (t2
                       (option string)
                       (t2
                          Pool_location.Repo.Id.t
                          (t2 int (t2 int (t2 int (t2 int (option ptime))))))))))))
  ;;
end

module Calendar = struct
  include Entity.Calendar
  open CCResult.Infix

  let read_only =
    Pool_common.(Message.ReadOnlyModel |> Utils.error_to_string Language.En)
  ;;

  module Location = struct
    let t =
      let encode (_ : location) = failwith read_only in
      let decode (id, name) =
        let url =
          Format.asprintf "admin/locations/%s" (Pool_location.Id.value id)
          |> Sihl.Web.externalize_path
        in
        Ok { id; name; url }
      in
      Caqti_type.(
        custom
          ~encode
          ~decode
          (t2 Pool_location.Repo.Id.t Pool_location.Repo.Name.t))
    ;;
  end

  let t =
    let encode (_ : t) = failwith read_only in
    let decode
      ( id
      , ( title
        , ( experiment_id
          , ( contact_email
            , ( start
              , ( duration
                , ( internal_description
                  , ( max_participants
                    , ( min_participants
                      , (overbook, (assignment_count, location)) ) ) ) ) ) ) )
        ) )
      =
      let* end_ =
        Entity.End.create start duration
        |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
      in
      let links = Entity.Calendar.create_links experiment_id id location in
      Ok
        { id
        ; title
        ; experiment_id
        ; contact_email
        ; start
        ; end_
        ; internal_description
        ; links
        ; max_participants
        ; min_participants
        ; overbook
        ; assignment_count
        ; location
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           RepoId.t
           (t2
              Experiment.Title.t
              (t2
                 Experiment.Id.t
                 (t2
                    (option Pool_user.Repo.EmailAddress.t)
                    (t2
                       Start.t
                       (t2
                          Duration.t
                          (t2
                             (option string)
                             (t2 int (t2 int (t2 int (t2 int Location.t))))))))))))
  ;;
end
