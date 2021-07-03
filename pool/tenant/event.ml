type t =
  [ `Tenant_added of Entity.tenant
  | `Tenant_edited of Entity.tenant
  | `Operator_assigned_to_tenant of Entity.tenant * Sihl.User.t
  | `Operator_divested_from_tenant of Entity.tenant * Sihl.User.t
  | `Operator_activated of Sihl.User.t
  | `Operator_deactivated of Sihl.User.t
  ]

type handle_event = t -> unit Lwt.t

let handle_event : handle_event = function
  | `Tenant_added tenant -> Repo.insert tenant
  | `Tenant_edited tenant -> Repo.update tenant
  | `Operator_assigned_to_tenant (tenant, user) ->
    Authz.assign user (Role.operator tenant)
  | `Operator_divested_from_tenant (tenant, user) ->
    Authz.divest user (Role.operator tenant)
  | `Operator_activated user -> Sihl.User.activate user
  | `Operator_deactivated user -> Sihl.User.deactivate user
;;
