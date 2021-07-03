type add_tenant_command =
  { title : string
  ; description : string
  ; url : string
  ; database : string
  ; styles : string
  ; icon : string
  ; logos : string
  ; partner_logos : string
  }

type handle_add_tenant = add_tenant_command -> (Event.t list, string) Result.t

type edit_tenant_command =
  { tenant_id : string
  ; title : string
  ; description : string
  ; url : string
  ; database : string
  ; styles : string
  ; icon : string
  ; logos : string
  ; partner_logos : string
  }

type handle_edit_tenant =
  add_tenant_command -> Entity.tenant -> (Event.t list, string) Result.t

type add_operator_to_tenant_command =
  { operator_id : string
  ; tenant_id : string
  }

type handle_app_operator_to_tenant =
  add_operator_to_tenant_command
  -> Sihl.User.t
  -> Entity.tenant
  -> (Event.t list, string) Result.t

type active_operator_command = { operator_id : string }

type handle_activate_operator_command =
  Sihl.User.t -> (Event.t list, string) Result.t

type deactive_operator_command = { operator_id : string }

type handle_deactivate_operator_command =
  Sihl.User.t -> (Event.t list, string) Result.t
