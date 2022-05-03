module ShowUp = struct
  include Entity.ShowUp

  let t = Caqti_type.bool
end

module Participated = struct
  include Entity.Participated

  let t = Caqti_type.bool
end

module MatchesFilter = struct
  include Entity.MatchesFilter

  let t = Caqti_type.bool
end

module CanceledAt = struct
  include Entity.CanceledAt

  let t = Caqti_type.(option ptime)
end

type t =
  { id : Pool_common.Id.t
  ; session_id : Pool_common.Id.t
  ; subject_id : Pool_common.Id.t
  ; show_up : ShowUp.t
  ; participated : Participated.t
  ; matches_filter : MatchesFilter.t
  ; canceled_at : CanceledAt.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

let to_entity (m : t) (subject : Subject.t) : Entity.t =
  Entity.
    { id = m.id
    ; subject
    ; show_up = m.show_up
    ; participated = m.participated
    ; matches_filter = m.matches_filter
    ; canceled_at = m.canceled_at
    ; created_at = m.created_at
    ; updated_at = m.updated_at
    }
;;

let of_entity (session_id : Pool_common.Id.t) (m : Entity.t) : t =
  { id = m.Entity.id
  ; session_id
  ; subject_id = Subject.id m.Entity.subject
  ; show_up = m.Entity.show_up
  ; participated = m.Entity.participated
  ; matches_filter = m.Entity.matches_filter
  ; canceled_at = m.Entity.canceled_at
  ; created_at = m.Entity.created_at
  ; updated_at = m.Entity.updated_at
  }
;;

let t =
  let encode m =
    Ok
      ( Pool_common.Id.value m.id
      , ( Pool_common.Id.value m.session_id
        , ( Pool_common.Id.value m.subject_id
          , ( ShowUp.value m.show_up
            , ( m.participated
              , (m.matches_filter, (m.canceled_at, (m.created_at, m.updated_at)))
              ) ) ) ) )
  in
  let decode
      ( id
      , ( session_id
        , ( subject_id
          , ( show_up
            , ( participated
              , (matches_filter, (canceled_at, (created_at, updated_at))) ) ) )
        ) )
    =
    let open CCResult in
    Ok
      { id = Pool_common.Id.of_string id
      ; session_id = Pool_common.Id.of_string session_id
      ; subject_id = Pool_common.Id.of_string subject_id
      ; show_up = ShowUp.create show_up
      ; participated = Participated.create participated
      ; matches_filter = MatchesFilter.create matches_filter
      ; canceled_at = CanceledAt.value canceled_at
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Pool_common.Repo.Id.t
         (tup2
            Pool_common.Repo.Id.t
            (tup2
               Pool_common.Repo.Id.t
               (tup2
                  ShowUp.t
                  (tup2
                     Participated.t
                     (tup2
                        MatchesFilter.t
                        (tup2
                           CanceledAt.t
                           (tup2
                              Pool_common.Repo.CreatedAt.t
                              Pool_common.Repo.UpdatedAt.t)))))))))
;;
