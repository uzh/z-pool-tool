open Entity

type create =
  { experiment : Experiment.t
  ; mailing : Mailing.t option
  ; invitations : t list
  }
[@@deriving eq, show]

type event =
  | Created of create
  | Resent of (t * Mailing.Id.t option)
[@@deriving eq, show]

let handle_event pool = function
  | Created { invitations; _ } when CCList.is_empty invitations -> Lwt.return_unit
  | Created { invitations; experiment; mailing } ->
    let mailing_id = CCOption.map (fun { Mailing.id; _ } -> id) mailing in
    Repo.bulk_insert ?mailing_id pool invitations experiment.Experiment.id
  | Resent (invitation, mailing_id) ->
    Repo.resend
      ?mailing_id
      pool
      { invitation with
        resent_at = Some (ResentAt.create ())
      ; send_count = SendCount.increment invitation.send_count
      }
;;
