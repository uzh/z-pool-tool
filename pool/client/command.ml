type add_client_command =
  { title : string
  ; description : string
  ; url : string
  ; database : string
  ; styles : string
  ; icon : string
  ; logos : string
  ; partner_logos : string
  }

type handle_add_client = add_client_command -> (Event.t list, string) Result.t

type edit_client_command =
  { client_id : string
  ; title : string
  ; description : string
  ; url : string
  ; database : string
  ; styles : string
  ; icon : string
  ; logos : string
  ; partner_logos : string
  }

type handle_edit_client =
  add_client_command -> Entity.client -> (Event.t list, string) Result.t

type add_operator_to_client_command =
  { operator_id : string
  ; client_id : string
  }

type handle_app_operator_to_client =
  add_operator_to_client_command
  -> Sihl.User.t
  -> Entity.client
  -> (Event.t list, string) Result.t

type active_operator_command = { operator_id : string }

type handle_activate_operator_command =
  Sihl.User.t -> (Event.t list, string) Result.t

type deactive_operator_command = { operator_id : string }

type handle_deactivate_operator_command =
  Sihl.User.t -> (Event.t list, string) Result.t
