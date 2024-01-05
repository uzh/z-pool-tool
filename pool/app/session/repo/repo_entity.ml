module RepoId = Pool_common.Repo.Id
module Reminder = Pool_common.Reminder
module RepoReminder = Pool_common.Repo.Reminder

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

type t =
  { id : Entity.Id.t
  ; follow_up_to : Entity.Id.t option
  ; has_follow_ups : bool
  ; start : Entity.Start.t
  ; duration : Ptime.Span.t
  ; description : Entity.Description.t option
  ; limitations : Entity.Limitations.t option
  ; location_id : Pool_location.Id.t
  ; max_participants : Entity.ParticipantAmount.t
  ; min_participants : Entity.ParticipantAmount.t
  ; overbook : Entity.ParticipantAmount.t
  ; email_reminder_lead_time : Reminder.EmailLeadTime.t option
  ; email_reminder_sent_at : Reminder.SentAt.t option
  ; text_message_reminder_lead_time : Reminder.TextMessageLeadTime.t option
  ; text_message_reminder_sent_at : Reminder.SentAt.t option
  ; assignment_count : Entity.AssignmentCount.t
  ; no_show_count : Entity.NoShowCount.t
  ; participant_count : Entity.ParticipantCount.t
  ; closed_at : Ptime.t option
  ; canceled_at : Ptime.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let of_entity (m : Entity.t) =
  { id = m.Entity.id
  ; follow_up_to = m.Entity.follow_up_to
  ; has_follow_ups = m.Entity.has_follow_ups
  ; start = m.Entity.start
  ; duration = m.Entity.duration
  ; description = m.Entity.description
  ; limitations = m.Entity.limitations
  ; location_id = m.Entity.location.Pool_location.id
  ; max_participants = m.Entity.max_participants
  ; min_participants = m.Entity.min_participants
  ; overbook = m.Entity.overbook
  ; email_reminder_lead_time = m.Entity.email_reminder_lead_time
  ; email_reminder_sent_at = m.Entity.email_reminder_sent_at
  ; text_message_reminder_lead_time = m.Entity.text_message_reminder_lead_time
  ; text_message_reminder_sent_at = m.Entity.text_message_reminder_sent_at
  ; assignment_count = m.Entity.assignment_count
  ; no_show_count = m.Entity.no_show_count
  ; participant_count = m.Entity.participant_count
  ; closed_at = m.Entity.closed_at
  ; canceled_at = m.Entity.canceled_at
  ; created_at = m.Entity.created_at
  ; updated_at = m.Entity.updated_at
  }
;;

let to_entity (m : t) location : Entity.t =
  Entity.
    { id = m.id
    ; follow_up_to = m.follow_up_to
    ; has_follow_ups = m.has_follow_ups
    ; start = m.start
    ; duration = m.duration
    ; description = m.description
    ; limitations = m.limitations
    ; location
    ; max_participants = m.max_participants
    ; min_participants = m.min_participants
    ; overbook = m.overbook
    ; email_reminder_lead_time = m.email_reminder_lead_time
    ; email_reminder_sent_at = m.email_reminder_sent_at
    ; text_message_reminder_lead_time = m.text_message_reminder_lead_time
    ; text_message_reminder_sent_at = m.text_message_reminder_sent_at
    ; assignment_count = m.assignment_count
    ; no_show_count = m.no_show_count
    ; participant_count = m.participant_count
    ; closed_at = m.closed_at
    ; canceled_at = m.canceled_at
    ; created_at = m.created_at
    ; updated_at = m.updated_at
    }
;;

(* TODO [aerben] these circumvent our smart constructors, good? *)
let t =
  let encode m =
    Ok
      ( m.id
      , ( m.follow_up_to
        , ( m.has_follow_ups
          , ( m.start
            , ( m.duration
              , ( m.description
                , ( m.limitations
                  , ( m.location_id
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
                                            , (m.created_at, m.updated_at) ) )
                                        ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
  in
  let decode
    ( id
    , ( follow_up_to
      , ( has_follow_ups
        , ( start
          , ( duration
            , ( description
              , ( limitations
                , ( location_id
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
                                        , (canceled_at, (created_at, updated_at))
                                        ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
    =
    Ok
      { id
      ; follow_up_to
      ; has_follow_ups
      ; start
      ; duration
      ; description
      ; limitations
      ; location_id
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
                              Pool_location.Repo.Id.t
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
                                                   RepoReminder
                                                   .TextMessageLeadTime
                                                   .t)
                                                (t2
                                                   (option
                                                      RepoReminder.SentAt.t)
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
                                                                     ptime))))))))))))))))))))))
;;

module Write = struct
  type t =
    { id : Entity.Id.t
    ; follow_up_to : Entity.Id.t option
    ; start : Entity.Start.t
    ; duration : Ptime.Span.t
    ; description : Entity.Description.t option
    ; limitations : Entity.Limitations.t option
    ; location_id : Pool_location.Id.t
    ; max_participants : Entity.ParticipantAmount.t
    ; min_participants : Entity.ParticipantAmount.t
    ; overbook : Entity.ParticipantAmount.t
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
       ; description
       ; limitations
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
    ; description
    ; limitations
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
              , ( m.description
                , ( m.limitations
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
            , ( description
              , ( limitations
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
        ; description
        ; limitations
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
    ; description : Entity.Description.t option
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

  module ContactPerson = struct
    let t =
      let encode (_ : contact_person) = failwith read_only in
      let decode (firstname, lastname, email) =
        let fullname =
          Format.asprintf "%s %s" firstname lastname |> CCString.trim
        in
        Ok { name = fullname; email }
      in
      Caqti_type.(
        custom ~encode ~decode (t3 string string Pool_user.Repo.EmailAddress.t))
    ;;
  end

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
          , ( start
            , ( duration
              , ( description
                , ( max_participants
                  , ( min_participants
                    , (overbook, (assignment_count, (location, contact_person)))
                    ) ) ) ) ) ) ) )
      =
      let* end_ =
        Entity.End.create start duration
        |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
      in
      let links = Entity.Calendar.create_links experiment_id id location in
      Ok
        { id
        ; experiment_id
        ; title
        ; start
        ; end_
        ; description
        ; links
        ; max_participants
        ; min_participants
        ; overbook
        ; assignment_count
        ; location
        ; contact_person
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           RepoId.t
           (t2
              Experiment.Repo.Entity.Title.t
              (t2
                 Experiment.Repo.Entity.Id.t
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
                                      (t2 Location.t (option ContactPerson.t)))))))))))))
  ;;
end
