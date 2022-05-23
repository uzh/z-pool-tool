open Entity
module Id = Pool_common.Id
module RepoId = Pool_common.Repo.Id

(* TODO [aerben] these circumvent our smart constructors, good? *)
let t =
  let encode m =
    Ok
      ( Id.value m.id
      , ( m.start
        , ( m.duration
          , ( m.description
            , ( m.max_participants
              , ( m.min_participants
                , ( m.overbook
                  , ( m.assignments_count
                    , (m.canceled_at, (m.created_at, m.updated_at)) ) ) ) ) ) )
        ) )
  in
  let decode
      ( id
      , ( start
        , ( duration
          , ( description
            , ( max_participants
              , ( min_participants
                , ( overbook
                  , (assignments_count, (canceled_at, (created_at, updated_at)))
                  ) ) ) ) ) ) )
    =
    Ok
      { id = Id.of_string id
      ; start
      ; duration
      ; description
      ; max_participants
      ; min_participants
      ; overbook
      ; assignments_count
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
                     int
                     (tup2
                        int
                        (tup2
                           int
                           (tup2 int (tup2 (option ptime) (tup2 ptime ptime)))))))))))
;;

module Write = struct
  type t =
    { id : Pool_common.Id.t
    ; start : Start.t
    ; duration : Ptime.Span.t
    ; description : Description.t option
    ; max_participants : ParticipantAmount.t
    ; min_participants : ParticipantAmount.t
    ; overbook : ParticipantAmount.t
    ; canceled_at : Ptime.t option
    }

  let entity_to_write
      (Entity.
         { id
         ; start
         ; duration
         ; description
         ; max_participants
         ; min_participants
         ; overbook
         ; canceled_at
         ; _
         } :
        Entity.t)
    =
    { id
    ; start
    ; duration
    ; description
    ; max_participants
    ; min_participants
    ; overbook
    ; canceled_at
    }
  ;;

  let t =
    let encode (m : t) =
      Ok
        ( Id.value m.id
        , ( m.start
          , ( m.duration
            , ( m.description
              , ( m.max_participants
                , (m.min_participants, (m.overbook, m.canceled_at)) ) ) ) ) )
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
        (tup2
           RepoId.t
           (tup2
              ptime
              (tup2
                 ptime_span
                 (tup2
                    (option string)
                    (tup2 int (tup2 int (tup2 int (option ptime)))))))))
  ;;
end

module Public = struct
  open Entity.Public

  let t =
    let encode (m : t) =
      Ok (Id.value m.id, (m.start, (m.duration, (m.description, m.canceled_at))))
    in
    let decode (id, (start, (duration, (description, canceled_at)))) =
      Ok { id = Id.of_string id; start; duration; description; canceled_at }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           RepoId.t
           (tup2 ptime (tup2 ptime_span (tup2 (option string) (option ptime))))))
  ;;
end
