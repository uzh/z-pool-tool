open CCFun

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

module Limit = struct
  include Entity.Limit

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.int
      (of_int %> CCResult.return)
      value
  ;;
end

module InvitationCount = struct
  include Entity.InvitationCount

  let t = Pool_common.Repo.make_caqti_type Caqti_type.int create value
end

module Distribution = struct
  include Entity.Distribution

  let t =
    Caqti_type.(
      custom
        ~encode:(yojson_of_t %> Yojson.Safe.to_string %> CCResult.return)
        ~decode:(Yojson.Safe.from_string %> t_of_yojson %> CCResult.return)
        string)
  ;;
end

type t =
  { id : Id.t
  ; experiment_id : Experiment.Id.t
  ; start_at : StartAt.t
  ; end_at : EndAt.t
  ; limit : Limit.t
  ; distribution : Distribution.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving show]

let to_entity
  { id; start_at; end_at; limit; distribution; created_at; updated_at; _ }
  =
  Entity.{ id; start_at; end_at; limit; distribution; created_at; updated_at }
;;

let of_entity
  (experiment_id : Experiment.Id.t)
  { Entity.id; start_at; end_at; limit; distribution; created_at; updated_at }
  =
  { id
  ; experiment_id
  ; start_at
  ; end_at
  ; limit
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
          , (m.end_at, (m.limit, (m.distribution, (m.created_at, m.updated_at))))
          ) ) )
  in
  let decode
    ( id
    , ( experiment_id
      , (start_at, (end_at, (limit, (distribution, (created_at, updated_at)))))
      ) )
    =
    let open CCResult in
    Ok
      { id
      ; experiment_id
      ; start_at
      ; end_at
      ; limit
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
            Experiment.Repo.Entity.Id.t
            (tup2
               StartAt.t
               (tup2
                  EndAt.t
                  (tup2
                     Limit.t
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
    ; limit : Limit.t
    ; distribution : Distribution.t
    }

  let t =
    let encode { Entity.id; start_at; end_at; limit; distribution; _ } =
      Ok (id, (start_at, (end_at, (limit, distribution))))
    in
    let decode _ =
      let open Pool_common in
      failwith (Message.WriteOnlyModel |> Utils.error_to_string Language.En)
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Id.t
           (tup2
              StartAt.t
              (tup2 EndAt.t (tup2 Limit.t (option Distribution.t))))))
  ;;
end

module Status = struct
  include Entity_status

  module ToHandle = struct
    include ToHandle

    let t = Pool_common.Repo.make_caqti_type Caqti_type.int create value
  end

  module LastRun = struct
    include LastRun

    let t =
      Pool_common.Repo.make_caqti_type
        Caqti_type.bool
        (create %> CCResult.return)
        value
    ;;
  end

  let t =
    let encode _ =
      let open Pool_common in
      failwith (Message.ReadOnlyModel |> Utils.error_to_string Language.En)
    in
    let decode (mailing, (to_handle, last_run)) =
      let mailing = to_entity mailing in
      Ok { mailing; to_handle; last_run }
    in
    Caqti_type.(custom ~encode ~decode (tup2 t (tup2 ToHandle.t LastRun.t)))
  ;;
end
