module Id = Pool_common.Id
module RepoId = Pool_common.Repo.Id

type t =
  { id : Pool_common.Id.t
  ; follow_up_to : Pool_common.Id.t option
  ; start : Entity.Start.t
  ; duration : Ptime.Span.t
  ; description : Entity.Description.t option
  ; location_id : Pool_location.Id.t
  ; max_participants : Entity.ParticipantAmount.t
  ; min_participants : Entity.ParticipantAmount.t
  ; overbook : Entity.ParticipantAmount.t
  ; reminder_subject : Pool_common.Reminder.Subject.t option
  ; reminder_text : Pool_common.Reminder.Text.t option
  ; reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; reminder_sent_at : Pool_common.Reminder.SentAt.t
  ; assignment_count : Entity.AssignmentCount.t
  ; canceled_at : Ptime.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let of_entity (m : Entity.t) =
  { id = m.Entity.id
  ; follow_up_to = m.Entity.follow_up_to
  ; start = m.Entity.start
  ; duration = m.Entity.duration
  ; description = m.Entity.description
  ; location_id = m.Entity.location.Pool_location.id
  ; max_participants = m.Entity.max_participants
  ; min_participants = m.Entity.min_participants
  ; overbook = m.Entity.overbook
  ; reminder_subject = m.Entity.reminder_subject
  ; reminder_text = m.Entity.reminder_text
  ; reminder_lead_time = m.Entity.reminder_lead_time
  ; reminder_sent_at = m.Entity.reminder_sent_at
  ; assignment_count = m.Entity.assignment_count
  ; canceled_at = m.Entity.canceled_at
  ; created_at = m.Entity.created_at
  ; updated_at = m.Entity.updated_at
  }
;;

let to_entity (m : t) location : Entity.t =
  Entity.
    { id = m.id
    ; follow_up_to = m.follow_up_to
    ; start = m.start
    ; duration = m.duration
    ; description = m.description
    ; location
    ; max_participants = m.max_participants
    ; min_participants = m.min_participants
    ; overbook = m.overbook
    ; reminder_subject = m.reminder_subject
    ; reminder_text = m.reminder_text
    ; reminder_lead_time = m.reminder_lead_time
    ; reminder_sent_at = m.reminder_sent_at
    ; assignment_count = m.assignment_count
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
        , ( m.start
          , ( m.duration
            , ( m.description
              , ( m.location_id
                , ( m.max_participants
                  , ( m.min_participants
                    , ( m.overbook
                      , ( m.reminder_subject
                        , ( m.reminder_text
                          , ( m.reminder_lead_time
                            , ( m.reminder_sent_at
                              , ( m.assignment_count
                                , (m.canceled_at, (m.created_at, m.updated_at))
                                ) ) ) ) ) ) ) ) ) ) ) ) ) )
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
                    , ( overbook
                      , ( reminder_subject
                        , ( reminder_text
                          , ( reminder_lead_time
                            , ( reminder_sent_at
                              , ( assignment_count
                                , (canceled_at, (created_at, updated_at)) ) ) )
                          ) ) ) ) ) ) ) ) ) ) )
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
      ; reminder_subject
      ; reminder_text
      ; reminder_lead_time
      ; reminder_sent_at
      ; assignment_count
      ; canceled_at
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         RepoId.t
         (tup2
            (option RepoId.t)
            (tup2
               ptime
               (tup2
                  ptime_span
                  (tup2
                     (option string)
                     (tup2
                        Pool_location.Repo.Id.t
                        (tup2
                           int
                           (tup2
                              int
                              (tup2
                                 int
                                 (tup2
                                    (option Pool_common.Repo.Reminder.Subject.t)
                                    (tup2
                                       (option Pool_common.Repo.Reminder.Text.t)
                                       (tup2
                                          (option
                                             Pool_common.Repo.Reminder.LeadTime
                                             .t)
                                          (tup2
                                             Pool_common.Repo.Reminder.SentAt.t
                                             (tup2
                                                int
                                                (tup2
                                                   (option ptime)
                                                   (tup2 ptime ptime)))))))))))))))))
;;

module Write = struct
  type t =
    { id : Pool_common.Id.t
    ; follow_up_to : Pool_common.Id.t option
    ; start : Entity.Start.t
    ; duration : Ptime.Span.t
    ; description : Entity.Description.t option
    ; location_id : Pool_location.Id.t
    ; max_participants : Entity.ParticipantAmount.t
    ; min_participants : Entity.ParticipantAmount.t
    ; overbook : Entity.ParticipantAmount.t
    ; reminder_subject : Pool_common.Reminder.Subject.t option
    ; reminder_text : Pool_common.Reminder.Text.t option
    ; reminder_lead_time : Pool_common.Reminder.LeadTime.t option
    ; reminder_sent_at : Pool_common.Reminder.SentAt.t
    ; canceled_at : Ptime.t option
    }

  let entity_to_write
      (Entity.
         { id
         ; follow_up_to
         ; start
         ; duration
         ; description
         ; location
         ; max_participants
         ; min_participants
         ; overbook
         ; reminder_subject
         ; reminder_text
         ; reminder_lead_time
         ; reminder_sent_at
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
    ; location_id = location.Pool_location.id
    ; max_participants
    ; min_participants
    ; overbook
    ; reminder_subject
    ; reminder_text
    ; reminder_lead_time
    ; reminder_sent_at
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
                , ( m.location_id
                  , ( m.max_participants
                    , ( m.min_participants
                      , ( m.overbook
                        , ( m.reminder_subject
                          , ( m.reminder_text
                            , ( m.reminder_lead_time
                              , (m.reminder_sent_at, m.canceled_at) ) ) ) ) ) )
                  ) ) ) ) ) )
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
                      , ( overbook
                        , ( reminder_subject
                          , ( reminder_text
                            , ( reminder_lead_time
                              , (reminder_sent_at, canceled_at) ) ) ) ) ) ) ) )
              ) ) ) )
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
        ; reminder_lead_time
        ; reminder_subject
        ; reminder_text
        ; reminder_sent_at
        ; canceled_at
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           RepoId.t
           (tup2
              (option RepoId.t)
              (tup2
                 ptime
                 (tup2
                    ptime_span
                    (tup2
                       (option string)
                       (tup2
                          Pool_location.Repo.Id.t
                          (tup2
                             int
                             (tup2
                                int
                                (tup2
                                   int
                                   (tup2
                                      (option
                                         Pool_common.Repo.Reminder.Subject.t)
                                      (tup2
                                         (option
                                            Pool_common.Repo.Reminder.Text.t)
                                         (tup2
                                            (option
                                               Pool_common.Repo.Reminder
                                               .LeadTime
                                               .t)
                                            (tup2
                                               Pool_common.Repo.Reminder.SentAt
                                               .t
                                               (option ptime)))))))))))))))
  ;;
end

module Public = struct
  type t =
    { id : Pool_common.Id.t
    ; follow_up_to : Pool_common.Id.t option
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
                      , (overbook, (assignment_count, canceled_at)) ) ) ) ) ) )
          ) )
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
        (tup2
           RepoId.t
           (tup2
              (option RepoId.t)
              (tup2
                 ptime
                 (tup2
                    ptime_span
                    (tup2
                       (option string)
                       (tup2
                          Pool_location.Repo.Id.t
                          (tup2
                             int
                             (tup2 int (tup2 int (tup2 int (option ptime))))))))))))
  ;;
end
