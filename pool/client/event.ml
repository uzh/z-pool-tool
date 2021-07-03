type t =
  [ `Client_added of Entity.client
  | `Client_edited of Entity.client
  | `Operator_assigned_to_client of Entity.client * Sihl.User.t
  | `Operator_divested_from_client of Entity.client * Sihl.User.t
  | `Operator_activated of Sihl.User.t
  | `Operator_deactivated of Sihl.User.t
  ]

type handle_event = t -> unit Sihl.io

let handle_event : handle_event = function
  | `Client_added client -> Repo.insert client
  | `Client_edited client -> Repo.update client
  | `Operator_assigned_to_client (client, user) ->
    Authz.assign user (Authz.Roles.operator client)
  | `Operator_divested_from_client (client, user) ->
    Authz.divest user (Authz.Roles.operator client)
  | `Operator_activated user -> Sihl.User.activate user
  | `Operator_deactivated user -> Sihl.User.deactivate user
;;
