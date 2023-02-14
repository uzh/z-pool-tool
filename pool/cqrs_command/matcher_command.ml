let get_or_failwith = Pool_common.Utils.get_or_failwith

module Run : sig
  type t =
    { mailing : Mailing.t
    ; experiment : Experiment.t
    ; contacts : Contact.t list
    ; create_message :
        Contact.t -> (Sihl_email.t, Pool_common.Message.error) result
    }

  val handle : t list -> (Pool_event.t list, Pool_common.Message.error) result

  val effects
    :  Pool_database.Label.t
    -> (Guard.Authorizer.effect list, Pool_common.Message.error) Lwt_result.t
end = struct
  type t =
    { mailing : Mailing.t
    ; experiment : Experiment.t
    ; contacts : Contact.t list
    ; create_message :
        Contact.t -> (Sihl_email.t, Pool_common.Message.error) result
    }

  let handle mailings =
    let open CCFun.Infix in
    let open CCList in
    mailings
    |> map (fun { experiment; contacts; create_message; _ } ->
         let open Invitation_command.Create in
         { experiment; contacts; invited_contacts = []; create_message }
         |> handle)
       %> all_ok
       %> CCResult.map flatten
  ;;

  let effects db_label =
    let open Utils.Lwt_result.Infix in
    let* tenant = Pool_tenant.find_by_label db_label in
    Lwt.return_ok
      [ ( `Update
        , `Target
            (tenant.Pool_tenant.id |> Guard.Uuid.target_of Pool_tenant.Id.value)
        )
      ]
  ;;
end
