open Entity
open CCFun.Infix
module ExperimentRepo = Experiment.Repo.Entity
open Session.Repo

let map_err fnc input =
  fnc input |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
;;

module InternalDescription = struct
  include Session.InternalDescription

  let t =
    Caqti_type.(
      custom ~encode:(value %> CCResult.return) ~decode:(map_err create) string)
  ;;
end

module PublicDescription = struct
  include Session.PublicDescription

  let t =
    Caqti_type.(
      custom ~encode:(value %> CCResult.return) ~decode:(map_err create) string)
  ;;
end

module ParticipantAmount = struct
  include Session.ParticipantAmount

  let t =
    Caqti_type.(
      custom ~encode:(value %> CCResult.return) ~decode:(map_err create) int)
  ;;
end

module AssignmentCount = struct
  include Session.AssignmentCount

  let t =
    Caqti_type.(
      custom ~encode:(value %> CCResult.return) ~decode:(map_err create) int)
  ;;
end

module NoShowCount = struct
  include Session.NoShowCount

  let t =
    Caqti_type.(
      custom ~encode:(value %> CCResult.return) ~decode:(map_err create) int)
  ;;
end

module ParticipantCount = struct
  include Session.ParticipantCount

  let t =
    Caqti_type.(
      custom ~encode:(value %> CCResult.return) ~decode:(map_err create) int)
  ;;
end

let t =
  let encode m =
    Ok
      ( m.id
      , ( m.start
        , ( m.duration
          , ( m.internal_description
            , ( m.public_description
              , ( m.max_participants
                , ( m.assignment_count
                  , ( m.no_show_count
                    , ( m.participant_count
                      , (m.created_at, (m.updated_at, m.experiment)) ) ) ) ) )
            ) ) ) )
  in
  let decode
        ( id
        , ( start
          , ( duration
            , ( internal_description
              , ( public_description
                , ( max_participants
                  , ( assignment_count
                    , ( no_show_count
                      , ( participant_count
                        , (created_at, (updated_at, experiment)) ) ) ) ) ) ) )
          ) )
    =
    Ok
      { id
      ; start
      ; duration
      ; internal_description
      ; public_description
      ; max_participants
      ; assignment_count
      ; no_show_count
      ; participant_count
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
         Id.t
         (t2
            Start.t
            (t2
               Duration.t
               (t2
                  (option InternalDescription.t)
                  (t2
                     (option PublicDescription.t)
                     (t2
                        (option ParticipantAmount.t)
                        (t2
                           AssignmentCount.t
                           (t2
                              NoShowCount.t
                              (t2
                                 ParticipantCount.t
                                 (t2
                                    Pool_common.Repo.CreatedAt.t
                                    (t2
                                       Pool_common.Repo.UpdatedAt.t
                                       ExperimentRepo.t))))))))))))
;;

module Write = struct
  type t =
    { id : Session.Id.t
    ; experiment_id : Experiment.Id.t
    ; start : Session.Start.t
    ; duration : Session.Duration.t
    ; internal_description : Session.InternalDescription.t option
    ; public_description : Session.PublicDescription.t option
    ; max_participants : Session.ParticipantAmount.t option
    }

  let of_entity
        (Entity.
           { id
           ; start
           ; duration
           ; internal_description
           ; public_description
           ; max_participants
           ; experiment
           ; _
           } :
          Entity.t)
    =
    { id
    ; experiment_id = experiment.Experiment.id
    ; start
    ; duration
    ; internal_description
    ; public_description
    ; max_participants
    }
  ;;

  let t =
    let encode (m : t) =
      Ok
        ( m.id
        , ( m.experiment_id
          , ( m.start
            , ( m.duration
              , ( m.internal_description
                , (m.public_description, m.max_participants) ) ) ) ) )
    in
    let decode _ =
      Pool_common.Utils.failwith Pool_message.Error.WriteOnlyModel
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Id.t
           (t2
              ExperimentRepo.Id.t
              (t2
                 Start.t
                 (t2
                    Duration.t
                    (t2
                       (option InternalDescription.t)
                       (t2
                          (option PublicDescription.t)
                          (option ParticipantAmount.t))))))))
  ;;
end
