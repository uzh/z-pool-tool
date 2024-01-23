module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "i18n.cqrs"

module Update : sig
  include Common.CommandSig

  type t = I18n.Content.t

  val handle
    :  ?tags:Logs.Tag.set
    -> I18n.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Pool_common.Id.t -> Guard.ValidationSet.t
end = struct
  type t = I18n.Content.t

  let schema = Conformist.(make Field.[ I18n.Content.schema () ] CCFun.id)

  let handle ?(tags = Logs.Tag.empty) property (command : t) =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let open I18n in
    let system_events =
      let open Key in
      match property |> key with
      | AssistantRoleHint
      | CreditsText
      | ExperimenterRoleHint
      | GreetingsText
      | PasswordPolicyText
      | SignUpCTA
      | TermsAndConditions
      | WelcomeText -> []
      | PrivacyPolicy ->
        System_event.
          [ Job.I18nPageUpdated |> create |> created |> Pool_event.system_event
          ]
    in
    Ok ((I18n.Updated (property, command) |> Pool_event.i18n) :: system_events)
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = I18n.Guard.Access.update
end
