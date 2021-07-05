type event =
  [ `Tenant_added of Entity.tenant
  | `Tenant_edited of Entity.tenant
  | `Operator_assigned_to_tenant of Entity.tenant * Sihl.User.t
  | `Operator_divested_from_tenant of Entity.tenant * Sihl.User.t
  ]

let handle_event : event -> unit Lwt.t = function
  | `Tenant_added tenant -> Repo.insert tenant
  | `Tenant_edited tenant -> Repo.update tenant
  | `Operator_assigned_to_tenant (tenant, user) ->
    Authz.assign user (Authz.operator tenant.id)
  | `Operator_divested_from_tenant (tenant, user) ->
    Authz.divest user (Authz.operator tenant.id)
;;
