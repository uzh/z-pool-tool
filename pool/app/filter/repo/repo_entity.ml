open Entity
module Common = Pool_common
module Id = Common.Id
module RepoId = Common.Repo.Id

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
          |> map_err (fun err ->
               Pool_common.Utils.error_to_string Pool_common.Language.En err))
        string)
  ;;
end

let t =
  let encode (m : t) = Ok (m.id, (m.filter, (m.created_at, m.updated_at))) in
  let decode (id, (filter, (created_at, updated_at))) =
    let open CCResult in
    Ok { id; filter; created_at; updated_at }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         RepoId.t
         (tup2 Filter.t (tup2 Common.Repo.CreatedAt.t Common.Repo.UpdatedAt.t))))
;;

module Write = struct
  let t =
    let encode (m : t) = Ok (m.id, m.filter) in
    let decode _ = failwith "Write only model" in
    Caqti_type.(custom ~encode ~decode (tup2 RepoId.t Filter.t))
  ;;
end
