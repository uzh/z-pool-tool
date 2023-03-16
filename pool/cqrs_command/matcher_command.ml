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
  val effects : Pool_tenant.t -> Guard.EffectSet.t
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

  let effects { Pool_tenant.id; _ } =
    let open Guard in
    let target_id = id |> Uuid.target_of Pool_tenant.Id.value in
    EffectSet.(
      And
        [ One (Action.Update, TargetSpec.Id (`Tenant, target_id))
        ; One (Action.Read, TargetSpec.Entity `Mailing)
        ; One (Action.Create, TargetSpec.Entity `Invitation)
        ])
  ;;
end
