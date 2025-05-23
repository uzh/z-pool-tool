open Entity
module RepoId = Pool_common.Repo.Id
module Reminder = Pool_common.Reminder
module RepoReminder = Pool_common.Repo.Reminder
module ExperimentRepo = Experiment.Repo.Entity

module Id = struct
  include RepoId
end

module Start = struct
  include Entity.Start

  let t = Caqti_type.ptime
end

module End = struct
  include Entity.End

  let t = Caqti_type.ptime
end

module Duration = struct
  include Entity.Duration

  let t = Caqti_type.ptime_span
end

(* TODO [aerben] these circumvent our smart constructors, good? *)
let t =
  let encode m =
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
                                            , (m.updated_at, (m.experiment, m.location))
                                            ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
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
                                              , (updated_at, (experiment, location)) ) )
                                          ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
    =
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
                                             (option RepoReminder.TextMessageLeadTime.t)
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
                                                                  Pool_common.Repo
                                                                  .CreatedAt
                                                                  .t
                                                                  (t2
                                                                     Pool_common.Repo
                                                                     .UpdatedAt
                                                                     .t
                                                                     (t2
                                                                        ExperimentRepo.t
                                                                        Pool_location.Repo
                                                                        .t)))))))))))))))))))))))
;;

module Write = struct
  type t =
    { id : Id.t
    ; follow_up_to : Id.t option
    ; start : Start.t
    ; duration : PtimeSpan.t
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
                                  , (m.closed_at, m.canceled_at) ) ) ) ) ) ) ) ) ) ) ) )
          ) )
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
                                    , (closed_at, canceled_at) ) ) ) ) ) ) ) ) ) ) ) ) )
          )
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
                                            Pool_common.Repo.Reminder.EmailLeadTime.t)
                                         (t2
                                            (option Pool_common.Repo.Reminder.SentAt.t)
                                            (t2
                                               (option
                                                  Pool_common.Repo.Reminder
                                                  .TextMessageLeadTime
                                                  .t)
                                               (t2
                                                  (option
                                                     Pool_common.Repo.Reminder.SentAt.t)
                                                  (t2 (option ptime) (option ptime)))))))))))))))))
  ;;
end

module Public = struct
  open Entity.Public

  let t =
    let encode (m : t) =
      Ok
        ( m.id
        , ( m.experiment_id
          , ( m.experiment_title
            , ( m.follow_up_to
              , ( m.start
                , ( m.duration
                  , ( m.description
                    , ( m.max_participants
                      , ( m.min_participants
                        , ( m.overbook
                          , ( m.assignment_count
                            , (m.canceled_at, (m.closed_at, m.location)) ) ) ) ) ) ) ) )
            ) ) )
    in
    let decode
          ( id
          , ( experiment_id
            , ( experiment_title
              , ( follow_up_to
                , ( start
                  , ( duration
                    , ( description
                      , ( max_participants
                        , ( min_participants
                          , ( overbook
                            , (assignment_count, (canceled_at, (closed_at, location))) )
                          ) ) ) ) ) ) ) ) )
      =
      Ok
        { id
        ; experiment_id
        ; experiment_title
        ; follow_up_to
        ; start
        ; duration
        ; description
        ; location
        ; max_participants
        ; min_participants
        ; overbook
        ; assignment_count
        ; canceled_at
        ; closed_at
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           RepoId.t
           (t2
              ExperimentRepo.Id.t
              (t2
                 ExperimentRepo.PublicTitle.t
                 (t2
                    (option RepoId.t)
                    (t2
                       Start.t
                       (t2
                          Duration.t
                          (t2
                             (option string)
                             (t2
                                int
                                (t2
                                   int
                                   (t2
                                      int
                                      (t2
                                         int
                                         (t2
                                            (option ptime)
                                            (t2 (option ptime) Pool_location.Repo.t))))))))))))))
  ;;
end

module Calendar = struct
  include Entity.Calendar
  open CCResult.Infix

  let read_only =
    Pool_message.Error.ReadOnlyModel |> Pool_common.(Utils.error_to_string Language.En)
  ;;

  module Location = struct
    let t =
      let encode (_ : location) = failwith read_only in
      let decode (id, name) = Ok { id; name } in
      Caqti_type.(
        custom ~encode ~decode (t2 Pool_location.Repo.Id.t Pool_location.Repo.Name.t))
    ;;
  end

  let t actor guardian =
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
                        , (min_participants, (overbook, (assignment_count, location))) )
                      ) ) ) ) ) ) )
      =
      let* end_ =
        Entity.End.build start duration
        |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
      in
      let links = Entity.Calendar.make_links actor guardian experiment_id id location in
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
              ExperimentRepo.Title.t
              (t2
                 ExperimentRepo.Id.t
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
