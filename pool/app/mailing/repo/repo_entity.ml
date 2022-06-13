module Id = struct
  include Pool_common.Id
  include Pool_common.Repo.Id
end

module StartAt = struct
  include Entity.StartAt

  let t = Caqti_type.ptime
end

module EndAt = struct
  include Entity.EndAt

  let t = Caqti_type.ptime
end

module Rate = struct
  include Entity.Rate

  let t =
    let decode m =
      m
      |> create
      |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
    in
    Caqti_type.(custom ~encode:(Utils.fcn_ok value) ~decode int)
  ;;
end

module Distribution = struct
  include Entity.Distribution

  let t =
    let open CCResult in
    Caqti_type.(
      custom
        ~encode:(fun m -> m |> yojson_of_t |> Yojson.Safe.to_string |> pure)
        ~decode:(fun m -> m |> Yojson.Safe.from_string |> t_of_yojson |> pure)
        string)
  ;;
end

type t =
  { id : Id.t
  ; experiment_id : Pool_common.Id.t
  ; start_at : StartAt.t
  ; end_at : EndAt.t
  ; rate : Rate.t
  ; distribution : Distribution.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving show]

let to_entity
    { id; start_at; end_at; rate; distribution; created_at; updated_at; _ }
  =
  Entity.{ id; start_at; end_at; rate; distribution; created_at; updated_at }
;;

let of_entity
    (experiment_id : Pool_common.Id.t)
    { Entity.id; start_at; end_at; rate; distribution; created_at; updated_at }
  =
  { id
  ; experiment_id
  ; start_at
  ; end_at
  ; rate
  ; distribution
  ; created_at
  ; updated_at
  }
;;

let t =
  let encode m =
    Ok
      ( m.id
      , ( m.experiment_id
        , ( m.start_at
          , (m.end_at, (m.rate, (m.distribution, (m.created_at, m.updated_at))))
          ) ) )
  in
  let decode
      ( id
      , ( experiment_id
        , (start_at, (end_at, (rate, (distribution, (created_at, updated_at)))))
        ) )
    =
    let open CCResult in
    Ok
      { id
      ; experiment_id
      ; start_at
      ; end_at
      ; rate
      ; distribution
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Id.t
         (tup2
            Pool_common.Repo.Id.t
            (tup2
               StartAt.t
               (tup2
                  EndAt.t
                  (tup2
                     Rate.t
                     (tup2
                        (option Distribution.t)
                        (tup2
                           Pool_common.Repo.CreatedAt.t
                           Pool_common.Repo.UpdatedAt.t))))))))
;;

module Update = struct
  type t =
    { id : Id.t
    ; start_at : StartAt.t
    ; end_at : EndAt.t
    ; rate : Rate.t
    ; distribution : Distribution.t
    }

  let t =
    let encode { Entity.id; start_at; end_at; rate; distribution; _ } =
      Ok (id, (start_at, (end_at, (rate, distribution))))
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
           Id.t
           (tup2 StartAt.t (tup2 EndAt.t (tup2 Rate.t (option Distribution.t))))))
  ;;
end
