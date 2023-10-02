open CCFun
open Entity
module Common = Pool_common
module Id = Common.Repo.Id

module Title = struct
  include Title

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module Query = struct
  let t =
    Caqti_type.(
      custom
        ~encode:(yojson_of_query %> Yojson.Safe.to_string %> CCResult.return)
        ~decode:
          (Yojson.Safe.from_string
           %> query_of_yojson
           %> CCResult.map_err Common.(Utils.error_to_string Language.En))
        string)
  ;;
end

let t =
  let encode (m : t) =
    Ok (m.id, (m.query, (m.title, (m.created_at, m.updated_at))))
  in
  let decode (id, (query, (title, (created_at, updated_at)))) =
    let open CCResult in
    Ok { id; query; title; created_at; updated_at }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Id.t
         (tup2
            Query.t
            (tup2
               (option Title.t)
               (tup2 Common.Repo.CreatedAt.t Common.Repo.UpdatedAt.t)))))
;;

module Write = struct
  let t =
    let encode (m : t) = Ok (m.id, (m.query, m.title)) in
    let decode _ = failwith "Write only model" in
    Caqti_type.(
      custom ~encode ~decode (tup2 Id.t (tup2 Query.t (option Title.t))))
  ;;
end
