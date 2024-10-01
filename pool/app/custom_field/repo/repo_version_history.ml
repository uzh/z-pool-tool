module Database = Database
module Dynparam = Database.Dynparam

let select_names_of_custom_fields_and_options ids =
  let ids =
    CCList.mapi
      (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1))
      ids
    |> CCString.concat ","
  in
  let columns =
    [ Entity.Id.sql_select_fragment ~field:"uuid"; "name" ]
    |> CCString.concat ","
  in
  [%string
    {sql| 
        SELECT %{columns} FROM pool_custom_fields WHERE uuid IN (%{ids}) 
          UNION
        SELECT %{columns} FROM pool_custom_field_options WHERE uuid IN (%{ids}) 
    |sql}]
;;

let find_names pool =
  let open Caqti_request.Infix in
  let open Dynparam in
  function
  | [] -> Lwt.return []
  | ids ->
    let (Pack (pt, pv)) =
      (CCList.fold_left (fun dyn id -> dyn |> add Pool_common.Repo.Id.t id))
        empty
        ids
    in
    let request =
      select_names_of_custom_fields_and_options ids
      |> pt ->* Caqti_type.(t2 Pool_common.Repo.Id.t Repo_entity.Name.t)
    in
    Database.collect pool request pv
;;
