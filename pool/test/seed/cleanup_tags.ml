let cleanup_tags database_label =
  let open CCFun in
  let open Caqti_request.Infix in
  let delete_tagged =
    {sql|DELETE FROM pool_tagging|sql} |> Caqti_type.(unit ->. unit)
  in
  let delete_tags =
    {sql|DELETE FROM pool_tags|sql} |> Caqti_type.(unit ->. unit)
  in
  let delete_tag_targets =
    {sql|
      DELETE FROM guardian_targets
      WHERE kind = '`Tag'
    |sql}
    |> Caqti_type.(unit ->. unit)
  in
  let exec =
    flip (Utils.Database.exec (Pool_database.Label.value database_label)) ()
  in
  Lwt_list.iter_s exec [ delete_tagged; delete_tags; delete_tag_targets ]
;;
