open Entity

let equal_operator_event (t1, o1) (t2, o2) =
  equal t1 t2 && String.equal o1.Sihl_user.id o2.Sihl_user.id
;;

type event =
  | TenantAdded of t
  | TenantEdited of t
  | OperatorAssignedToTenant of t * Sihl_user.t
  | OperatorDivestedFromTenant of t * Sihl_user.t

let handle_event : event -> unit Lwt.t = function
  | TenantAdded tenant -> Repo.insert tenant
  | TenantEdited tenant -> Repo.update tenant
  | OperatorAssignedToTenant (tenant, user) ->
    Permission.assign user (Role.operator (tenant.id |> Id.to_human))
  | OperatorDivestedFromTenant (tenant, user) ->
    Permission.divest user (Role.operator (tenant.id |> Id.to_human))
;;

let equal_event event1 event2 =
  match event1, event2 with
  | TenantAdded one, TenantAdded two | TenantEdited one, TenantEdited two ->
    equal one two
  | ( OperatorAssignedToTenant (tenant_one, user_one)
    , OperatorAssignedToTenant (tenant_two, user_two) )
  | ( OperatorDivestedFromTenant (tenant_one, user_one)
    , OperatorDivestedFromTenant (tenant_two, user_two) ) ->
    equal tenant_one tenant_two
    && String.equal user_one.Sihl_user.id user_two.Sihl_user.id
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | TenantAdded m | TenantEdited m -> pp formatter m
  | OperatorAssignedToTenant (tenant, user)
  | OperatorDivestedFromTenant (tenant, user) ->
    let () = pp formatter tenant in
    Sihl_user.pp formatter user
;;
