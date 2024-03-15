module Database = Pool_database
module Dynparam = Utils.Database.Dynparam
open Caqti_request.Infix

let count_invitations_request ?(by_count = false) () =
  let base =
    {sql|
    SELECT COUNT(1)
    FROM pool_invitations
    WHERE experiment_uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  in
  match by_count with
  | false -> base
  | true -> Format.asprintf "%s \n %s" base "AND send_count = ?"
;;

let find_unique_counts_request =
  {sql|
    SELECT DISTINCT send_count
    FROM pool_invitations
    WHERE experiment_uuid = UNHEX(REPLACE(?, '-', ''))
    ORDER BY send_count
  |sql}
  |> Caqti_type.(string ->* int)
;;

let total_invitation_count_by_experiment pool experiment_id =
  let open Caqti_request.Infix in
  Utils.Database.find
    (pool |> Pool_database.Label.value)
    (count_invitations_request () |> Caqti_type.(string ->! int))
    (Experiment.Id.value experiment_id)
;;

let by_experiment pool ({ Experiment.id; _ } as experiment) =
  let open Utils.Lwt_result.Infix in
  let%lwt counts =
    Utils.Database.collect
      (pool |> Database.Label.value)
      find_unique_counts_request
      (Experiment.Id.value id)
  in
  let base_dyn =
    Dynparam.(empty |> add Caqti_type.string (Experiment.Id.value id))
  in
  let%lwt total_sent = total_invitation_count_by_experiment pool id in
  let%lwt sent_by_count =
    counts
    |> Lwt_list.map_s (fun send_count ->
      let (Dynparam.Pack (pt, pv)) =
        base_dyn |> Dynparam.add Caqti_type.int send_count
      in
      let request =
        count_invitations_request ~by_count:true () |> pt ->! Caqti_type.int
      in
      Utils.Database.find (pool |> Pool_database.Label.value) request pv
      |> Lwt.map (fun count -> send_count, count))
  in
  let* total_match_filter =
    let query =
      experiment.Experiment.filter
      |> CCOption.map (fun { Filter.query; _ } -> query)
    in
    Filter.(
      count_filtered_contacts
        ~include_invited:true
        pool
        (Matcher (Experiment.Id.to_common id))
        query)
  in
  Lwt.return_ok
    Entity.Statistics.{ total_sent; total_match_filter; sent_by_count }
;;
