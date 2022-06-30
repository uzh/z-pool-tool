open Entity

let create_invitation (experiment : Experiment.t) contact subject content =
  (* TODO[tinhub]: Sihl 4.0: add text elements to for subject *)
  let open Experiment in
  let name = Contact.fullname contact in
  let email = Contact.email_address contact in
  let experiment_description =
    experiment.description |> Experiment.Description.value
  in
  Email.Helper.prepare_boilerplate_email
    subject
    (email |> Pool_user.EmailAddress.value)
    content
    [ "name", name; "experimentDescription", experiment_description ]
;;

type create =
  { experiment : Experiment.t
  ; contact : Contact.t
  }
[@@deriving eq, show]

type resent =
  { invitation : t
  ; experiment : Experiment.t
  }
[@@deriving eq, show]

type event =
  | Created of create
  | Resent of resent * (string * string)
  | InvitationsSent of Experiment.t * (Contact.t * (string * string)) list
    (* TODO[timhub]: ensure type safety, see reminder command *)
[@@deriving eq, show]

let handle_event pool event =
  match event with
  (* TODO[timhub]: bulk insert *)
  | Created { experiment; contact } ->
    create contact |> Repo.insert pool experiment.Experiment.id
  | Resent ({ invitation; experiment }, (subject, text)) ->
    let%lwt () =
      Repo.update pool { invitation with resent_at = Some (ResentAt.create ()) }
    in
    let%lwt email =
      create_invitation experiment invitation.contact subject text
    in
    Service.Email.send ~ctx:(Pool_tenant.to_ctx pool) email
  | InvitationsSent (experiment, data) ->
    let%lwt emails =
      Lwt_list.map_s
        (fun email ->
          let contact, (subject, text) = email in
          create_invitation experiment contact subject text)
        data
    in
    Service.Email.bulk_send ~ctx:(Pool_tenant.to_ctx pool) emails
;;
