module Id = Pool_common.Id

let equal_operator_event (t1, o1) (t2, o2) =
  Pool_common.Id.equal t1 t2 && CCString.equal o1.Sihl_user.id o2.Sihl_user.id
;;

type event =
  | OperatorAssigned of Id.t * Admin.operator Admin.t
  | OperatorDivested of Id.t * Admin.operator Admin.t
  | StatusReportGenerated of unit

let handle_event _ : event -> unit Lwt.t = function
  | OperatorAssigned (tenant_id, user) ->
    Guard.Persistence.save_rule_exn
      ( `One (Guard.Uuid.of_string_exn (Admin.user user).Sihl_user.id)
      , `Manage
      , `One (Id.to_uuidm tenant_id) )
  | OperatorDivested (tenant_id, user) ->
    Guard.Persistence.delete_rule_exn
      ( `One (Guard.Uuid.of_string_exn (Admin.user user).Sihl_user.id)
      , `Manage
      , `One (Id.to_uuidm tenant_id) )
  | StatusReportGenerated _ -> Utils.todo ()
;;

let[@warning "-4"] equal_event event1 event2 =
  match event1, event2 with
  | ( OperatorAssigned (tenant_id_one, user_one)
    , OperatorAssigned (tenant_id_two, user_two) )
  | ( OperatorDivested (tenant_id_one, user_one)
    , OperatorDivested (tenant_id_two, user_two) ) ->
    CCString.equal (tenant_id_one |> Id.value) (tenant_id_two |> Id.value)
    && CCString.equal
         (Admin.user user_one).Sihl_user.id
         (Admin.user user_two).Sihl_user.id
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | OperatorAssigned (tenant_id, user) | OperatorDivested (tenant_id, user) ->
    Id.pp formatter tenant_id;
    Admin.pp formatter user
  | StatusReportGenerated () -> Utils.todo ()
;;
