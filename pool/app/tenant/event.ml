module Id = Pool_common.Id

let equal_operator_event (t1, o1) (t2, o2) =
  Pool_common.Id.equal t1 t2 && CCString.equal o1.Sihl_user.id o2.Sihl_user.id
;;

type event =
  | OperatorAssigned of Id.t * Admin.t
  | OperatorUnassigned of Id.t * Admin.t
  | StatusReportGenerated of unit
[@@deriving variants]

let handle_event pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  function
  | OperatorAssigned (tenant_id, user) ->
    let open Guard in
    let%lwt () =
      Persistence.Rule.save
        ~ctx:(Pool_database.to_ctx pool)
        ( ActorSpec.Id
            (`Admin, Uuid.Actor.of_string_exn (Admin.user user).Sihl_user.id)
        , Action.Manage
        , TargetSpec.Id (`Tenant, tenant_id |> Uuid.target_of Id.value) )
      ||> CCResult.get_or_failwith
    in
    Lwt.return_unit
  | OperatorUnassigned (tenant_id, user) ->
    let open Guard in
    Persistence.Rule.delete_exn
      ~ctx:(Pool_database.to_ctx pool)
      ( ActorSpec.Id
          (`Admin, Uuid.Actor.of_string_exn (Admin.user user).Sihl_user.id)
      , Action.Manage
      , TargetSpec.Id (`Tenant, tenant_id |> Uuid.target_of Id.value) )
  | StatusReportGenerated _ -> Utils.todo ()
;;

let[@warning "-4"] equal_event event1 event2 =
  match event1, event2 with
  | ( OperatorAssigned (tenant_id_one, user_one)
    , OperatorAssigned (tenant_id_two, user_two) )
  | ( OperatorUnassigned (tenant_id_one, user_one)
    , OperatorUnassigned (tenant_id_two, user_two) ) ->
    CCString.equal (tenant_id_one |> Id.value) (tenant_id_two |> Id.value)
    && CCString.equal
         (Admin.user user_one).Sihl_user.id
         (Admin.user user_two).Sihl_user.id
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | OperatorAssigned (tenant_id, user) | OperatorUnassigned (tenant_id, user) ->
    Id.pp formatter tenant_id;
    Admin.pp formatter user
  | StatusReportGenerated () -> Utils.todo ()
;;

let show_event = function
  | OperatorAssigned _ -> "OperatorAssigned"
  | OperatorUnassigned _ -> "OperatorUnassigned"
  | StatusReportGenerated _ -> "StatusReportGenerated"
;;
