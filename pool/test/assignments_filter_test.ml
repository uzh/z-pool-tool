(*

test 1: 1 assigned contact, 1 probe contact
test 2: 1 assigned contact marked as deleted, 1 contacts was found
test 3: 1 assigned contact marked as cancelled, no contacts found

*)

(**
subquery_for_invitation

let sessions = [ session ] in
let experiments = [
  Experiment sessions
] in

let assigned_contact = 
  { user_id : Id.t
  ; email : User.EmailAddress.t
  ; password : User.Password.t [@opaque]
  ; firstname : User.Firstname.t
  ; lastname : User.Lastname.t
  ; terms_accepted_at : User.TermsAccepted.t option
  ; language : Pool_common.Language.t option
  }
 in
let invitation = Assignment.create invited_contact in

let probe_contact = Contact.create contact in

let events = [
  Contact.created invited_contact;
] in
let _ = Pool_event.handle_events events in

let invitation_filter = 
  let open Filter in
  let key = Key.(Hardcoded Assignment) in
  let value = 
    let exp_ids = experiment_ids |> CCList.map (fun id -> Str (id |> Experiment.Id.value)) in
    (Lst exp_ids)
    in
  let predicate = (Predicate.create operator key value ) in
  Pred predicate
in

let found_contacts = Filter.find_filtered_contacts
  test_db base_filter (Some filter)

assert [1 == probe_contact]
assert []
*)
