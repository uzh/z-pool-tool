open Entity
module Common = Pool_common
module Id = Common.Id
module RepoId = Common.Repo.Id

module Title = struct
  include Title

  let t =
    let encode = Utils.fcn_ok value in
    let decode m =
      m |> create |> CCResult.map_err Common.(Utils.error_to_string Language.En)
    in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Filter = struct
  let t =
    let open CCResult in
    Caqti_type.(
      custom
        ~encode:(fun m ->
          m |> yojson_of_filter |> Yojson.Safe.to_string |> pure)
        ~decode:(fun m ->
          m
          |> Yojson.Safe.from_string
          |> filter_of_yojson
          |> CCResult.map_err Common.(Utils.error_to_string Language.En))
        string)
  ;;
end

let t =
  let encode (m : t) =
    Ok (m.id, (m.filter, (m.title, (m.created_at, m.updated_at))))
  in
  let decode (id, (filter, (title, (created_at, updated_at)))) =
    let open CCResult in
    Ok { id; filter; title; created_at; updated_at }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         RepoId.t
         (tup2
            Filter.t
            (tup2
               (option Title.t)
               (tup2 Common.Repo.CreatedAt.t Common.Repo.UpdatedAt.t)))))
;;

module Write = struct
  let t =
    let encode (m : t) = Ok (m.id, (m.filter, m.title)) in
    let decode _ = failwith "Write only model" in
    Caqti_type.(
      custom ~encode ~decode (tup2 RepoId.t (tup2 Filter.t (option Title.t))))
  ;;
end
