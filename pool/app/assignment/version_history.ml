open Ppx_yojson_conv_lib.Yojson_conv
open Entity

module Record = struct
  open Entity

  let model = Pool_message.Field.Assignment

  type t =
    { id : Id.t
         [@equal fun a b -> Id.equal a b || Sihl.Configuration.is_test ()]
    ; contact : Contact.Id.t
    ; no_show : NoShow.t option
    ; participated : Participated.t option
    ; matches_filter : MatchesFilter.t
    ; canceled_at : CanceledAt.t option
    ; marked_as_deleted : MarkedAsDeleted.t
    ; external_data_id : ExternalDataId.t option
    ; reminder_manually_last_sent_at : Pool_common.Reminder.SentAt.t option
    ; custom_fields : Custom_field.Id.t list option
    ; created_at : Pool_common.CreatedAt.t
    ; updated_at : Pool_common.UpdatedAt.t
    }
  [@@deriving eq, show, yojson]
end

let to_record
  ({ id
   ; contact
   ; no_show
   ; participated
   ; matches_filter
   ; canceled_at
   ; marked_as_deleted
   ; external_data_id
   ; reminder_manually_last_sent_at
   ; custom_fields
   ; created_at
   ; updated_at
   } :
    Entity.t)
  : Record.t
  =
  let custom_fields =
    custom_fields |> CCOption.map (CCList.map Custom_field.Public.id)
  in
  Record.
    { id
    ; contact = Contact.id contact
    ; no_show
    ; participated
    ; matches_filter
    ; canceled_at
    ; marked_as_deleted
    ; external_data_id
    ; reminder_manually_last_sent_at
    ; custom_fields
    ; created_at
    ; updated_at
    }
;;

include Changelog.T (Record)
