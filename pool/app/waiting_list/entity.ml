type t =
  { id : Pool_common.Id.t
  ; contact : Contact.t
  ; experiment : Experiment.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create ?(id = Pool_common.Id.create ()) contact experiment =
  { id
  ; contact
  ; experiment
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;

module ExperimentList = struct
  type waiting_list_entry =
    { id : Pool_common.Id.t
    ; contact : Contact.Preview.t
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
