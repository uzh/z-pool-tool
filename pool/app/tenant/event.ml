open Entity

type event =
  | TenantAdded of t [@equal equal]
  | Tenant_edited of t
  | Operator_assigned_to_tenant of t * Sihl.User.t
  | Operator_divested_from_tenant of t * Sihl.User.t
[@@deriving eq, show]

let handle_event : event -> unit Lwt.t = function
  | TenantAdded tenant -> Repo.insert tenant
  | Tenant_edited tenant -> Repo.update tenant
  | Operator_assigned_to_tenant (tenant, user) ->
    Permission.assign user (Role.operator (tenant.id |> Id.to_human))
  | Operator_divested_from_tenant (tenant, user) ->
    Permission.divest user (Role.operator (tenant.id |> Id.to_human))
;;
