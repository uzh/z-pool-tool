include Entity
include Event
module Guard = Entity_guard

let find_by_model = Repo.find_by_model
let find_by_group = Repo.find_by_group
let find_ungrouped_by_model = Repo.find_ungrouped_by_model
let find = Repo.find

let find_of_contact ?(required = false) pool user id =
  let open Pool_context in
  let find is_admin =
    Repo_public.find_all_by_contact ~is_admin ~required pool id
  in
  match user with
  | Guest -> Lwt.return ([], [])
  | Contact _ -> find false
  | Admin _ -> find true
;;

let find_all_by_contact = find_of_contact ~required:false
let find_all_required_by_contact = find_of_contact ~required:true

let find_unanswered_required_by_contact database_label user id =
  let open Pool_context in
  let find is_admin =
    Repo_public.find_unanswered_required_by_contact ~is_admin database_label id
  in
  match user with
  | Guest -> Lwt.return ([], [])
  | Contact _ -> find false
  | Admin _ -> find true
;;

let find_multiple_by_contact = Repo_public.find_multiple_by_contact
let find_by_contact = Repo_public.find_by_contact
let all_required_answered = Repo_public.all_required_answered
let all_answered = Repo_public.all_answered
let find_option = Repo_option.find

let find_options_by_field pool id =
  let open Utils.Lwt_result.Infix in
  Repo_option.find_by_field pool id ||> CCList.map Repo_entity.Option.to_entity
;;

let find_group = Repo_group.find
let find_groups_by_model = Repo_group.find_by_model

module Repo = struct
  module Id = struct
    include Pool_common.Repo.Id
  end

  module SelectOption = struct
    module Id = struct
      include Repo_entity.Option.Id
    end
  end
end

let create_answer is_admin answer new_value =
  let id = Answer.id_opt answer in
  let value = CCOption.bind answer Answer.value in
  (match is_admin with
   | true -> Answer.create ?id ~admin_value:new_value value
   | false -> Answer.create ?id (Some new_value))
  |> CCOption.pure
;;

let validate_htmx ~is_admin value (m : Public.t) =
  let open Public in
  let open CCResult.Infix in
  let no_value = Error Pool_common.Message.NoValue in
  (* Allow admins to reset required answers *)
  let required = Public.required m && not is_admin in
  let single_value =
    value
    |> CCList.head_opt
    |> CCFun.flip CCOption.bind (fun v ->
      if CCString.is_empty v then None else Some v)
  in
  let go validation value = validation |> fst |> fun rule -> rule value in
  match m with
  | Boolean (public, answer) ->
    (* TODO: Find UI way to do this *)
    let to_field a = Public.Boolean (public, a) |> CCResult.return in
    (match single_value, required with
     | Some value, _ ->
       value
       |> Utils.Bool.of_string
       |> create_answer is_admin answer
       |> to_field
     | None, false -> to_field None
     | None, true -> no_value)
  | Date (public, answer) ->
    let to_field a = Public.Date (public, a) in
    (match single_value, required with
     | Some value, _ ->
       value
       |> Ptime.date_of_string
       >|= create_answer is_admin answer
       >|= to_field
     | None, false -> to_field None |> CCResult.return
     | None, true -> no_value)
  | MultiSelect (public, options, answer) ->
    let to_field a = Public.MultiSelect (public, options, a) in
    (match value, required with
     | [], true -> no_value
     | vals, _ ->
       let open SelectOption in
       vals
       |> CCList.map (fun value ->
         CCList.find_opt
           (fun { Public.id; _ } -> Id.equal id (value |> Id.of_string))
           options
         |> CCOption.to_result
              Pool_common.Message.(Invalid Field.CustomFieldOption))
       |> CCList.all_ok
       >|= create_answer is_admin answer
       >|= to_field)
  | Number (({ validation; _ } as public), answer) ->
    let to_field a = Public.Number (public, a) in
    (match single_value, required with
     | Some value, _ ->
       value
       |> CCInt.of_string
       |> CCOption.to_result Message.(NotANumber value)
       >>= go validation
       >|= create_answer is_admin answer
       >|= to_field
     | None, false -> Ok (to_field None)
     | None, true -> no_value)
  | Select (public, options, answer) ->
    let to_field a = Public.Select (public, options, a) in
    (match single_value, required with
     | Some value, _ ->
       let open SelectOption in
       CCList.find_opt
         (fun option -> Id.equal option.Public.id (Id.of_string value))
         options
       |> CCOption.to_result Message.InvalidOptionSelected
       >|= create_answer is_admin answer
       >|= to_field
     | None, false -> Ok (to_field None)
     | None, true -> no_value)
  | Text (({ validation; _ } as public), answer) ->
    let to_field a = Public.Text (public, a) in
    (match single_value, required with
     | Some value, _ ->
       value |> go validation >|= create_answer is_admin answer >|= to_field
     | None, false -> Ok (to_field None)
     | None, true -> no_value)
;;

let validate_partial_update
  ?(is_admin = false)
  contact
  custom_field
  (field, current_version, value)
  =
  let open PartialUpdate in
  let check_version old_v t =
    let open Pool_common.Version in
    if old_v |> value > (current_version |> value)
    then Error Pool_common.Message.(MeantimeUpdate field)
    else t |> increment_version |> CCResult.return
  in
  let validate schema =
    let schema =
      Pool_common.Utils.PoolConformist.(make Field.[ schema () ] CCFun.id)
    in
    Conformist.decode_and_validate
      schema
      [ field |> Pool_common.Message.Field.show, value ]
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  in
  let open CCResult in
  match[@warning "-4"] field with
  | PoolField.Firstname ->
    User.Firstname.schema
    |> validate
    >|= (fun m -> Firstname (current_version, m))
    >>= check_version contact.Contact.firstname_version
    |> Lwt.return
  | PoolField.Lastname ->
    User.Lastname.schema
    |> validate
    >|= (fun m -> Lastname (current_version, m))
    >>= check_version contact.Contact.lastname_version
    |> Lwt.return
  | PoolField.Language ->
    (fun () -> Conformist.optional @@ Pool_common.Language.schema ())
    |> validate
    >|= (fun m -> Language (current_version, m))
    >>= check_version contact.Contact.language_version
    |> Lwt.return
  | _ ->
    let open Utils.Lwt_result.Infix in
    let check_permission m =
      Lwt_result.lift
      @@
      if Public.is_disabled is_admin m
      then Error Pool_common.Message.NotEligible
      else Ok m
    in
    let* custom_field =
      custom_field
      |> CCOption.to_result Pool_common.Message.InvalidHtmxRequest
      |> Lwt_result.lift
      >>= check_permission
      >>= CCFun.(validate_htmx ~is_admin value %> Lwt_result.lift)
    in
    let old_v = Public.version custom_field in
    custom_field |> custom |> check_version old_v |> Lwt_result.lift
;;
