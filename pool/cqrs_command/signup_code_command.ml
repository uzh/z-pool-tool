open Signup_code

let src = Logs.Src.create "signup_code.cqrs"
let webhook_url_schema field = Pool_conformist.optional @@ WebhookUrl.schema field ()

let signup_started_schema () =
  webhook_url_schema Pool_message.Field.SignUpStartedWebhookUrl
;;

let signup_completed_schema () =
  webhook_url_schema Pool_message.Field.SignUpCompletedWebhookUrl
;;

type create =
  { code : Code.t
  ; signup_started_webhook_url : WebhookUrl.t option
  ; signup_completed_webhook_url : WebhookUrl.t option
  }

type update =
  { signup_started_webhook_url : WebhookUrl.t option
  ; signup_completed_webhook_url : WebhookUrl.t option
  }

module Create : sig
  type t = create

  val handle
    :  ?tags:Logs.Tag.set
    -> ?id:Id.t
    -> Signup_code.t option
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Guard.ValidationSet.t
end = struct
  type t = create

  let schema =
    Pool_conformist.(
      make
        Field.[ Code.schema (); signup_started_schema (); signup_completed_schema () ]
        (fun code signup_started_webhook_url signup_completed_webhook_url ->
           { code; signup_started_webhook_url; signup_completed_webhook_url }))
  ;;

  let handle
        ?(tags = Logs.Tag.empty)
        ?id
        existing
        ({ code; signup_started_webhook_url; signup_completed_webhook_url } : t)
    =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    match existing with
    | Some (_ : Signup_code.t) -> Error Pool_message.(Error.AlreadyExisting Field.Code)
    | None ->
      Ok
        [ Created
            (create ?id ?signup_started_webhook_url ?signup_completed_webhook_url code)
          |> Pool_event.signupcode
        ]
  ;;

  let decode data =
    Pool_conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Access.create
end

module Update : sig
  type t = update

  val handle
    :  ?tags:Logs.Tag.set
    -> Signup_code.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Id.t -> Guard.ValidationSet.t
end = struct
  type t = update

  let schema =
    Pool_conformist.(
      make
        Field.[ signup_started_schema (); signup_completed_schema () ]
        (fun signup_started_webhook_url signup_completed_webhook_url ->
           { signup_started_webhook_url; signup_completed_webhook_url }))
  ;;

  let handle
        ?(tags = Logs.Tag.empty)
        signup_code
        ({ signup_started_webhook_url; signup_completed_webhook_url } : t)
    =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    Ok
      [ Updated
          { signup_code with signup_started_webhook_url; signup_completed_webhook_url }
        |> Pool_event.signupcode
      ]
  ;;

  let decode data =
    Pool_conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Access.update
end
