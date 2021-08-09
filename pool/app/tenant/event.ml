type event =
  [ `Tenant_added of Entity.t
  | `Tenant_edited of Entity.t
  | `Operator_assigned_to_tenant of Entity.t * Sihl.User.t
  | `Operator_divested_from_tenant of Entity.t * Sihl.User.t
  ]
[@@deriving eq, show]

let handle_event : event -> unit Lwt.t = function
  | `Tenant_added tenant -> Repo.insert tenant
  | `Tenant_edited tenant -> Repo.update tenant
  | `Operator_assigned_to_tenant (tenant, user) ->
    Permission.assign user (Role.operator (tenant.id |> Entity.Id.to_human))
  | `Operator_divested_from_tenant (tenant, user) ->
    Permission.divest user (Role.operator (tenant.id |> Entity.Id.to_human))
;;
