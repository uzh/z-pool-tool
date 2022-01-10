include Event
include Entity

let user_is_admin pool (user : Sihl_user.t) =
  if Sihl_user.is_admin user
  then (
    let%lwt admin = Repo.find_role_by_user pool user in
    Lwt.return @@ CCResult.is_ok admin)
  else Lwt.return_false
;;

let insert = Repo.insert
let find_any_admin_by_user_id = Repo.find_any_admin_by_user_id
let find_duplicates = Utils.todo

let find_all pool () =
  let open Lwt.Infix in
  let to_any results = CCList.map (fun m -> Any m) results in
  Lwt_list.fold_left_s
    (fun _ status ->
      let all_admins =
        match status with
        | `Assistant -> Repo.find_all_by_role pool AssistantC >|= to_any
        | `Experimenter -> Repo.find_all_by_role pool ExperimenterC >|= to_any
        | `LocationManager ->
          Repo.find_all_by_role pool LocationManagerC >|= to_any
        | `Recruiter -> Repo.find_all_by_role pool RecruiterC >|= to_any
        | `Operator -> Repo.find_all_by_role pool OperatorC >|= to_any
      in
      all_admins)
    []
    all_admin_roles
;;

module Human = struct
  let user (admin : any) =
    match admin with
    | Any (Assistant _ as admin) -> user admin
    | Any (Experimenter _ as admin) -> user admin
    | Any (LocationManager _ as admin) -> user admin
    | Any (Recruiter _ as admin) -> user admin
    | Any (Operator _ as admin) -> user admin
  ;;
end
