module Comment = struct
  type t = string [@@deriving eq, show]

  let create m = m
  let value m = m

  let schema () =
    Pool_common.(
      Utils.schema_decoder
        (fun m -> Ok (m |> create))
        value
        Message.Field.Comment)
  ;;
end

type t =
  { id : Pool_common.Id.t
  ; contact : Contact.t
  ; experiment : Experiment.t
  ; comment : Comment.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create ?(id = Pool_common.Id.create ()) contact experiment comment =
  { id
  ; contact
  ; experiment
  ; comment
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;

module ExperimentList = struct
  type waiting_list_entry =
    { id : Pool_common.Id.t
    ; contact : Contact.Preview.t
    ; comment : Comment.t option
    ; created_at : Pool_common.CreatedAt.t
    ; updated_at : Pool_common.UpdatedAt.t
    }
  [@@deriving eq, show]

  type t =
    { experiment : Experiment.t
    ; waiting_list_entries : waiting_list_entry list
    }
  [@@deriving eq, show]
end

let searchable_by = Contact.searchable_by

let sortable_by =
  searchable_by
  @ (Pool_common.Message.[ Field.CreatedAt, "pool_invitations.created_at" ]
     |> Query.Column.create_list)
;;
