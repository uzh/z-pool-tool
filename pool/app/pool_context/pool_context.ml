include Entity

let is_from_root { database_label; _ } = Pool_database.is_root database_label

let user_is_admin = function
  | Guest | Contact _ -> false
  | Admin _ -> true
;;

module Logger = struct
  module Tags = struct
    let req (req : Sihl.Web.Request.t) : Logs.Tag.set =
      let open CCOption in
      let default = "undefined" in
      let id = Sihl.Web.Id.find req |> value ~default in
      let database_label, user =
        find req
        |> of_result
        >|= (fun { database_label; user; _ } ->
              database_label |> Pool_database.Label.value, user |> show_log_user)
        |> value ~default:(default, default)
      in
      let open Logs.Tag in
      empty
      |> add Logger.tag_req id
      |> add Logger.tag_database database_label
      |> add Logger.tag_user user
    ;;

    let context { database_label; user; _ } : Logs.Tag.set =
      let open Logs.Tag in
      empty
      |> add Logger.tag_database (database_label |> Pool_database.Label.value)
      |> add Logger.tag_user (user |> show_log_user)
    ;;
  end
end
