module Database = Pool_database
module RepoEntity = Repo_entity

(* TODO: Make sure only distinct results are returned *)

let find_current_by_contact_request () =
  let open Experiment.Repo in
  let open Caqti_request.Infix in
  let experiment_type =
    {sql|
      pool_experiments.assignment_without_session = 1
    |sql}
  in
  Format.asprintf
    "%s WHERE %s AND %s AND %s AND (%s OR %s)"
    Public.pool_invitations_left_join
    experiment_type
    Public.condition_not_assigned
    Public.condition_registration_not_disabled
    Public.condition_allow_uninvited_signup
    Public.condition_is_invited
  |> Repo.find_request_sql
  |> Pool_common.Repo.Id.t ->* RepoEntity.t
;;

let find_current_by_contact pool contact =
  let open Utils.Lwt_result.Infix in
  Utils.Database.collect
    (Pool_database.Label.value pool)
    (find_current_by_contact_request ())
    (Contact.id contact)
  (* TODO: This has to be made superfluous by a background job (#164) *)
  >|> Lwt_list.filter_s
        Filter.(
          fun { Entity.experiment; _ } ->
            experiment.Experiment.filter
            |> CCOption.map_or ~default:Lwt.return_true (fun { query; _ } ->
              Filter.contact_matches_filter pool query contact))
;;
