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
                , (m.overbook, (m.canceled_at, (m.created_at, m.updated_at))) )
              ) ) ) ) )
  in
  let decode
      ( id
      , ( start
        , ( duration
          , ( description
            , ( max_participants
              , ( min_participants
                , (overbook, (canceled_at, (created_at, updated_at))) ) ) ) ) )
      )
    =
    Ok
      { id = Id.of_string id
      ; start
      ; duration
      ; description
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
                     int
                     (tup2
                        int
                        (tup2 int (tup2 (option ptime) (tup2 ptime ptime))))))))))
;;

module Write = struct
  let t =
    let encode m =
      Ok
        ( Id.value m.id
        , ( m.start
          , ( m.duration
            , ( m.description
              , ( m.max_participants
                , (m.min_participants, (m.overbook, m.canceled_at)) ) ) ) ) )
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
