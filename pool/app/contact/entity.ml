include Changelog.DefaultSettings
open Ppx_yojson_conv_lib.Yojson_conv
open CCFun.Infix

let model = Pool_message.Field.Contact

module Id = struct
  include Pool_model.Base.Id

  let of_common = Pool_common.Id.value %> of_string
  let to_common = value %> Pool_common.Id.of_string
  let of_user = Pool_user.Id.value %> of_string
  let to_user = value %> Pool_user.Id.of_string
end

module NumberOfInvitations = struct
  include Pool_model.Base.Integer

  let init = 0
  let increment m = m + 1
  let update step m = m + step |> max 0
end

module NumberOfAssignments = struct
  include Pool_model.Base.Integer

  let init = 0
  let update step m = m + step |> max 0
end

module NumberOfShowUps = struct
  include Pool_model.Base.Integer

  let init = 0
  let update step m = m + step |> max 0
end

module NumberOfNoShows = struct
  include Pool_model.Base.Integer

  let init = 0
  let update step m = m + step |> max 0
end

module NumberOfParticipations = struct
  include Pool_model.Base.Integer

  let init = 0
  let update step m = m + step |> max 0
end

module AdminComment = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.AdminComment
  let schema () = schema field ()
  let of_string m = m
end

type t =
  { user : Pool_user.t
  ; terms_accepted_at : Pool_user.TermsAccepted.t option
  ; language : Pool_common.Language.t option
  ; experiment_type_preference : Pool_common.ExperimentType.t option
  ; cell_phone : Pool_user.CellPhone.t option
  ; paused : Pool_user.Paused.t
  ; disabled : Pool_user.Disabled.t
  ; verified : Pool_user.Verified.t option
  ; email_verified : Pool_user.EmailVerified.t option
  ; num_invitations : NumberOfInvitations.t
  ; num_assignments : NumberOfAssignments.t
  ; num_show_ups : NumberOfShowUps.t
  ; num_no_shows : NumberOfNoShows.t
  ; num_participations : NumberOfParticipations.t
  ; firstname_version : Pool_common.Version.t
  ; lastname_version : Pool_common.Version.t
  ; paused_version : Pool_common.Version.t
  ; language_version : Pool_common.Version.t
  ; experiment_type_preference_version : Pool_common.Version.t
  ; import_pending : Pool_user.ImportPending.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, fields, show, yojson, ord]

let user { user; _ } = user
let id m : Id.t = m.user.Pool_user.id |> Id.of_user
let fullname m = m.user |> Pool_user.fullname
let firstname m = m.user |> Pool_user.firstname
let lastname m = m.user |> Pool_user.lastname
let lastname_firstname m = m.user |> Pool_user.fullname ~reversed:true
let email_address m = m.user.Pool_user.email

let set_email_address m updated_email_address =
  let user = Pool_user.{ m.user with email = updated_email_address } in
  { m with user }
;;

let set_firstname m updated_firstname =
  let user = Pool_user.{ m.user with firstname = updated_firstname } in
  { m with user }
;;

let set_lastname m updated_lastname =
  let user = Pool_user.{ m.user with lastname = updated_lastname } in
  { m with user }
;;

let set_language m language = { m with language }
let set_cellphone m cell_phone = { m with cell_phone }

let create ?terms_accepted_at ?language user =
  { user
  ; terms_accepted_at
  ; language
  ; experiment_type_preference = None
  ; cell_phone = None
  ; paused = Pool_user.Paused.create false
  ; disabled = Pool_user.Disabled.create false
  ; verified = None
  ; email_verified = None
  ; num_invitations = NumberOfInvitations.init
  ; num_assignments = NumberOfAssignments.init
  ; num_show_ups = NumberOfShowUps.init
  ; num_no_shows = NumberOfNoShows.init
  ; num_participations = NumberOfParticipations.init
  ; firstname_version = Pool_common.Version.create ()
  ; lastname_version = Pool_common.Version.create ()
  ; paused_version = Pool_common.Version.create ()
  ; language_version = Pool_common.Version.create ()
  ; experiment_type_preference_version = Pool_common.Version.create ()
  ; import_pending = Pool_user.ImportPending.create false
  ; created_at = Pool_common.CreatedAt.create_now ()
  ; updated_at = Pool_common.UpdatedAt.create_now ()
  }
;;

module Write = struct
  type t =
    { user_id : Id.t
    ; terms_accepted_at : Pool_user.TermsAccepted.t option
    ; language : Pool_common.Language.t option
    ; experiment_type_preference : Pool_common.ExperimentType.t option
    ; cell_phone : Pool_user.CellPhone.t option
    ; paused : Pool_user.Paused.t
    ; disabled : Pool_user.Disabled.t
    ; verified : Pool_user.Verified.t option
    ; email_verified : Pool_user.EmailVerified.t option
    ; num_invitations : NumberOfInvitations.t
    ; num_assignments : NumberOfAssignments.t
    ; num_show_ups : NumberOfShowUps.t
    ; num_no_shows : NumberOfNoShows.t
    ; num_participations : NumberOfParticipations.t
    ; firstname_version : Pool_common.Version.t
    ; lastname_version : Pool_common.Version.t
    ; paused_version : Pool_common.Version.t
    ; language_version : Pool_common.Version.t
    ; experiment_type_preference_version : Pool_common.Version.t
    ; import_pending : Pool_user.ImportPending.t
    }
  [@@deriving eq, show]
end

let to_write (m : t) : Write.t =
  { Write.user_id = m |> id
  ; terms_accepted_at = m.terms_accepted_at
  ; language = m.language
  ; experiment_type_preference = m.experiment_type_preference
  ; cell_phone = m.cell_phone
  ; paused = m.paused
  ; disabled = m.disabled
  ; verified = m.verified
  ; email_verified = m.email_verified
  ; num_invitations = m.num_invitations
  ; num_assignments = m.num_assignments
  ; num_show_ups = m.num_show_ups
  ; num_no_shows = m.num_no_shows
  ; num_participations = m.num_participations
  ; firstname_version = m.firstname_version
  ; lastname_version = m.lastname_version
  ; paused_version = m.paused_version
  ; language_version = m.paused_version
  ; experiment_type_preference_version = m.experiment_type_preference_version
  ; import_pending = m.import_pending
  }
;;

let is_inactive { user; _ } =
  Pool_user.Status.(equal user.Pool_user.status Inactive)
;;

let sexp_of_t t = t |> id |> Id.sexp_of_t

let update_num_invitations ~step ({ num_invitations; _ } as m) =
  { m with num_invitations = NumberOfInvitations.update step num_invitations }
;;

let update_num_assignments ~step ({ num_assignments; _ } as m) =
  { m with num_assignments = NumberOfAssignments.update step num_assignments }
;;

let update_num_show_ups ~step ({ num_show_ups; _ } as m) =
  { m with num_show_ups = NumberOfShowUps.update step num_show_ups }
;;

let update_num_no_shows ~step ({ num_no_shows; _ } as m) =
  { m with num_no_shows = NumberOfNoShows.update step num_no_shows }
;;

let update_num_participations ~step ({ num_participations; _ } as m) =
  { m with
    num_participations = NumberOfParticipations.update step num_participations
  }
;;

module Preview = struct
  type t =
    { user : Pool_user.t
    ; language : Pool_common.Language.t option
    ; cell_phone : Pool_user.CellPhone.t option
    ; paused : Pool_user.Paused.t
    ; verified : Pool_user.Verified.t option
    ; num_invitations : NumberOfInvitations.t
    ; num_assignments : NumberOfAssignments.t
    }
  [@@deriving eq, show]

  let fullname (m : t) = m.user |> Pool_user.fullname
  let email_address (m : t) = m.user.Pool_user.email
end

let profile_completion_cookie = "profile_completion"

let column_cell_phone =
  (Pool_message.Field.CellPhone, "pool_contacts.cell_phone")
  |> Query.Column.create
;;

let column_hide_paused =
  Query.Column.create (Pool_message.Field.HidePaused, "pool_contacts.paused = 0")
;;

let column_hide_unverified =
  Query.Column.create
    ( Pool_message.Field.HideUnverified
    , "pool_contacts.email_verified IS NOT NULL" )
;;

let filterable_by =
  Some
    Query.Filter.Condition.Human.
      [ Checkbox column_hide_unverified
      ; Checkbox column_hide_paused
      ; Checkbox Pool_user.column_inactive
      ]
;;

let default_filter =
  let open Query.Filter in
  [ Condition.(Checkbox (column_hide_unverified, true))
  ; Condition.(Checkbox (column_hide_paused, true))
  ; Condition.(Checkbox (Pool_user.column_inactive, true))
  ]
;;

let searchable_by = Pool_user.searchable_by
let sortable_by = Pool_user.sortable_by
let default_sort = Pool_user.default_sort
let default_query = Query.create ~sort:default_sort ~filter:default_filter ()
