module ResentAt = struct
  include Pool_common.Model.Ptime

  let create = Ptime_clock.now
end

module ResendCount = struct
  open Pool_common
  include Model.Integer

  let field = Message.Field.Count
  let create m = if m > 0 then Ok m else Error Message.(Invalid field)
  let of_int m = if m > 0 then m else Utils.failwith Message.(Invalid field)
  let init = 1
  let schema = schema field create
end

type t =
  { id : Pool_common.Id.t
  ; contact : Contact.t
  ; resent_at : ResentAt.t option
  ; resend_count : ResendCount.t
  ; created_at : Pool_common.CreatedAt.t
       [@equal fun a b -> Ptime.equal a b || Sihl.Configuration.is_test ()]
  ; updated_at : Pool_common.UpdatedAt.t
       [@equal fun a b -> Ptime.equal a b || Sihl.Configuration.is_test ()]
  }
[@@deriving eq, show]

let create ?(id = Pool_common.Id.create ()) contact =
  { id
  ; contact
  ; resent_at = None
  ; resend_count = ResendCount.init
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;

let equal_queue_entry (mail1, queue1) (mail2, queue2) =
  CCString.equal mail1.Sihl_email.recipient mail2.Sihl_email.recipient
  && CCString.equal mail1.Sihl_email.subject mail2.Sihl_email.subject
  && CCString.equal mail1.Sihl_email.text mail2.Sihl_email.text
  && CCString.equal queue1.Sihl_queue.id queue2.Sihl_queue.id
;;

type notification_history =
  { invitation : t
  ; queue_entries : (Sihl_email.t * Sihl_queue.instance) list
       [@equal
         fun queue_entries1 queue_entries2 ->
           CCList.map2 equal_queue_entry queue_entries1 queue_entries2
           |> CCList.for_all CCFun.id]
  }
[@@deriving eq, show]

let email_experiment_elements (experiment : Experiment.t) =
  let open Experiment in
  [ "experimentPublicTitle", experiment.public_title |> PublicTitle.value
  ; ( "experimentDescription"
    , experiment.description |> CCOption.map_or ~default:"" Description.value )
  ]
;;

let searchable_by = Contact.searchable_by

let sortable_by =
  searchable_by
  @ (Pool_common.Message.
       [ Field.ResentAt, "pool_invitations.resent_at"
       ; Field.CreatedAt, "pool_invitations.created_at"
       ]
     |> Query.Column.create_list)
;;
