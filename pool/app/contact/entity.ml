module User = Pool_user

module Sihl_user = struct
  include Sihl_user

  let equal a b =
    let open CCString in
    equal a.id b.id
    && equal a.email b.email
    && equal_status a.status b.status
    && CCBool.equal a.admin b.admin
    && CCBool.equal a.confirmed b.confirmed
  ;;

  let compare a b = CCString.compare a.Sihl_user.email b.Sihl_user.email
end

module MessageChannel = struct
  type t =
    | Email
    | SMS
  [@@deriving eq, show]
end

module NumberOfInvitations = struct
  type t = int [@@deriving eq, ord, show]

  let init = 0
  let value m = m
  let of_int m = m
  let increment m = m + 1
  let update step m = m + step |> max 0
end

module NumberOfAssignments = struct
  type t = int [@@deriving eq, ord, show]

  let init = 0
  let value m = m
  let of_int m = m
  let update step m = m + step |> max 0
end

module NumberOfShowUps = struct
  type t = int [@@deriving eq, ord, show]

  let init = 0
  let value m = m
  let of_int m = m
  let update step m = m + step |> max 0
end

module NumberOfNoShows = struct
  type t = int [@@deriving eq, ord, show]

  let init = 0
  let value m = m
  let of_int m = m
  let update step m = m + step |> max 0
end

module NumberOfParticipations = struct
  type t = int [@@deriving eq, ord, show]

  let init = 0
  let value m = m
  let of_int m = m
  let update step m = m + step |> max 0
end

module AdminComment = struct
  include Pool_common.Model.String

  let field = Pool_common.Message.Field.AdminComment
  let schema () = schema field ()
  let of_string m = m
end

type t =
  { user : Sihl_user.t
  ; terms_accepted_at : User.TermsAccepted.t option
  ; language : Pool_common.Language.t option
  ; experiment_type_preference : Pool_common.ExperimentType.t option
  ; cell_phone : User.CellPhone.t option
  ; paused : User.Paused.t
  ; disabled : User.Disabled.t
  ; verified : User.Verified.t option
  ; email_verified : User.EmailVerified.t option
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
[@@deriving eq, show, ord]

module Write = struct
  type t =
    { user_id : Pool_common.Id.t
    ; terms_accepted_at : User.TermsAccepted.t option
    ; language : Pool_common.Language.t option
    ; experiment_type_preference : Pool_common.ExperimentType.t option
    ; cell_phone : User.CellPhone.t option
    ; paused : User.Paused.t
    ; disabled : User.Disabled.t
    ; verified : User.Verified.t option
    ; email_verified : User.EmailVerified.t option
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

  let create m =
    { user_id = Pool_common.Id.of_string m.user.Sihl.Contract.User.id
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
end

let user { user; _ } = user
let id m = m.user.Sihl_user.id |> Pool_common.Id.of_string
let fullname m = m.user |> User.user_fullname
let firstname m = m.user |> User.user_firstname
let lastname m = m.user |> User.user_lastname
let lastname_firstname m = m.user |> User.user_lastname_firstname
let email_address m = m.user.Sihl_user.email |> User.EmailAddress.of_string

let sexp_of_t t =
  t |> id |> Pool_common.Id.value |> fun s -> Sexplib0.Sexp.Atom s
;;

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
    { user : Sihl_user.t
    ; language : Pool_common.Language.t option
    ; cell_phone : User.CellPhone.t option
    ; paused : User.Paused.t
    ; verified : User.Verified.t option
    ; num_invitations : NumberOfInvitations.t
    ; num_assignments : NumberOfAssignments.t
    }
  [@@deriving eq, show]

  let fullname (m : t) = m.user |> User.user_fullname

  let email_address (m : t) =
    m.user.Sihl_user.email |> User.EmailAddress.of_string
  ;;
end

let profile_completion_cookie = "profile_completion"

let column_cell_phone =
  (Pool_common.Message.Field.CellPhone, "pool_contacts.cell_phone")
  |> Query.Column.create
;;

let column_hide_paused =
  Query.Column.create
    (Pool_common.Message.Field.HidePaused, "pool_contacts.paused")
;;

let column_hide_unverified =
  Query.Column.create
    (Pool_common.Message.Field.HideUnverified, "pool_contacts.email_verified")
;;

let filterable_by =
  Some
    Query.Filter.Condition.Human.
      [ HideNone column_hide_unverified; HideTrue column_hide_paused ]
;;

let default_filter =
  let open Query.Filter in
  [ Condition.(HideSome (column_hide_unverified, false))
  ; Condition.(HideTrue (column_hide_paused, false))
  ]
;;

let searchable_by = Pool_user.searchable_by
let sortable_by = Pool_user.sortable_by
let default_sort = Pool_user.default_sort
let default_query = Pool_user.default_query
