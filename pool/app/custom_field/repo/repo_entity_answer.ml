open Entity_answer

module Answer = struct
  include Answer

  let t =
    let encode t =
      t |> yojson_of_t |> Yojson.Safe.to_string |> CCResult.return
    in
    let decode s =
      let read s = s |> Yojson.Safe.from_string |> t_of_yojson in
      try Ok (read s) with
      | _ ->
        Error
          Pool_common.(
            Utils.error_to_string Language.En Message.(Invalid Field.Answer))
    in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

let t =
  let encode m = Ok (m.id, (m.answer, m.version)) in
  let decode (id, (answer, version)) = Ok { id; answer; version } in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2 Pool_common.Repo.Id.t (tup2 Answer.t Pool_common.Repo.Version.t)))
;;
