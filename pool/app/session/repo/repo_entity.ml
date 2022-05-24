module Id = Pool_common.Id
module RepoId = Pool_common.Repo.Id

type t =
  { id : Pool_common.Id.t
  ; start : Entity.Start.t
  ; duration : Ptime.Span.t
  ; description : Entity.Description.t option
  ; location_id : Pool_location.Id.t
  ; max_participants : Entity.ParticipantAmount.t
  ; min_participants : Entity.ParticipantAmount.t
  ; overbook : Entity.ParticipantAmount.t
  ; canceled_at : Ptime.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let of_entity (m : Entity.t) =
  { id = m.Entity.id
  ; start = m.Entity.start
  ; duration = m.Entity.duration
  ; description = m.Entity.description
  ; location_id = m.Entity.location.Pool_location.id
  ; max_participants = m.Entity.max_participants
  ; min_participants = m.Entity.min_participants
  ; overbook = m.Entity.overbook
  ; canceled_at = m.Entity.canceled_at
  ; created_at = m.Entity.created_at
  ; updated_at = m.Entity.updated_at
  }
;;

let to_entity (m : t) location : Entity.t =
  Entity.
    { id = m.id
    ; start = m.start
    ; duration = m.duration
    ; description = m.description
    ; location
    ; max_participants = m.max_participants
    ; min_participants = m.min_participants
    ; overbook = m.overbook
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
      , ( m.start
        , ( m.duration
          , ( m.description
            , ( m.location_id
              , ( m.max_participants
                , ( m.min_participants
                  , (m.overbook, (m.canceled_at, (m.created_at, m.updated_at)))
                  ) ) ) ) ) ) )
  in
  let decode
      ( id
      , ( start
        , ( duration
          , ( description
            , ( location_id
              , ( max_participants
                , ( min_participants
                  , (overbook, (canceled_at, (created_at, updated_at))) ) ) ) )
          ) ) )
    =
    Ok
      { id
      ; start
      ; duration
      ; description
      ; location_id
      ; max_participants
      ; min_participants
      ; overbook
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
                           (tup2 int (tup2 (option ptime) (tup2 ptime ptime)))))))))))
;;

module Write = struct
  let t =
    let encode m =
      Ok
        ( m.id
        , ( m.start
          , ( m.duration
            , ( m.description
              , ( m.location_id
                , ( m.max_participants
                  , (m.min_participants, (m.overbook, m.canceled_at)) ) ) ) ) )
        )
    in
    let decode _ = failwith "Write only model" in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           RepoId.t
           (tup2
              ptime
              (tup2
                 ptime_span
                 (tup2
                    (option string)
                    (tup2
                       Pool_location.Repo.Id.t
                       (tup2 int (tup2 int (tup2 int (option ptime))))))))))
  ;;
end

module Public = struct
  type t =
    { id : Pool_common.Id.t
    ; start : Entity.Start.t
    ; duration : Ptime.Span.t
    ; description : Entity.Description.t option
    ; location_id : Pool_location.Id.t
    ; canceled_at : Ptime.t option
    }
  [@@deriving eq, show]

  let of_entity (m : Entity.Public.t) : t =
    { id = m.Entity.Public.id
    ; start = m.Entity.Public.start
    ; duration = m.Entity.Public.duration
    ; description = m.Entity.Public.description
    ; location_id = m.Entity.Public.location.Pool_location.id
    ; canceled_at = m.Entity.Public.canceled_at
    }
  ;;

  let to_entity (m : t) location : Entity.Public.t =
    Entity.Public.
      { id = m.id
      ; start = m.start
      ; duration = m.duration
      ; description = m.description
      ; location
      ; canceled_at = m.canceled_at
      }
  ;;

  let t =
    let encode (m : t) =
      Ok
        ( m.id
        , ( m.start
          , (m.duration, (m.description, (m.location_id, m.canceled_at))) ) )
    in
    let decode
        (id, (start, (duration, (description, (location_id, canceled_at)))))
      =
      Ok { id; start; duration; description; location_id; canceled_at }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           RepoId.t
           (tup2
              ptime
              (tup2
                 ptime_span
                 (tup2
                    (option string)
                    (tup2 Pool_location.Repo.Id.t (option ptime)))))))
  ;;
end
