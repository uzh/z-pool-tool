include Entity
include Event
open Pool_message
module Guard = Entity_guard
module VersionHistory = Version_history
module OptionVersionHistory = Version_history.OptionVersionHistory
module GroupVersionHistory = Version_history.GroupVersionHistory
module AnswerRecord = Version_history.AnswerRecord
module AnswerVersionHistory = Version_history.AnswerVersionHistory

let find_by_model = Repo.find_by_model
let find_by_group = Repo.find_by_group
let find_ungrouped_by_model = Repo.find_ungrouped_by_model
let find = Repo.find
let find_by_table_view = Repo.Sql.find_by_table_view

let find_of_contact ?(required = false) pool user id =
  let open Pool_context in
  let find is_admin = Repo_public.find_all_by_contact ~is_admin ~required pool id in
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

let find_unanswered_ungrouped_required_by_contact database_label user id =
  let open Pool_context in
  let find is_admin =
    Repo_public.find_unanswered_ungrouped_required_by_contact ~is_admin database_label id
  in
  match user with
  | Guest -> Lwt.return []
  | Contact _ -> find false
  | Admin _ -> find true
;;

let find_multiple_by_contact = Repo_public.find_multiple_by_contact
let find_by_contact = Repo_public.find_by_contact
let all_required_answered = Repo_public.all_required_answered
let all_answered = Repo_public.all_answered
let all_prompted_on_registration = Repo_public.all_prompted_on_registration
let find_public_by_contacts_and_view = Repo_public.Sql.find_by_contacts_and_view
let find_option = Repo_option.find

let find_options_by_field pool id =
  let open Utils.Lwt_result.Infix in
  Repo_option.find_by_field pool id ||> CCList.map Repo_entity.Option.to_entity
;;

let find_group = Repo_group.find
let find_groups_by_model = Repo_group.find_by_model
let find_names = Repo_version_history.find_names

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

let create_answer is_admin entity_uuid answer new_value =
  let id = Answer.id_opt answer in
  let value = CCOption.bind answer Answer.value in
  (match is_admin with
   | true -> Answer.create ?id entity_uuid ~admin_value:new_value value
   | false -> Answer.create ?id entity_uuid (Some new_value))
  |> CCOption.pure
;;

let validate_htmx ~is_admin ~entity_uuid value (m : Public.t) =
  let open Public in
  let open CCResult.Infix in
  let no_value = Error Error.NoValue in
  (* Allow admins to reset required answers *)
  let required = Public.required m && not is_admin in
  let single_value =
    value
    |> CCList.head_opt
    |> CCFun.flip CCOption.bind (fun v -> if CCString.is_empty v then None else Some v)
  in
  let validate validation value = validation |> fst |> fun rule -> rule value in
  match m with
  | Boolean (public, answer) ->
    (* Ignoring required, as false is default *)
    let to_field a = Public.Boolean (public, a) |> CCResult.return in
    let open CCOption.Infix in
    single_value
    >|= Utils.Bool.of_string
    |> CCOption.value ~default:false
    |> create_answer is_admin entity_uuid answer
    |> to_field
  | Date (public, answer) ->
    let to_field a = Public.Date (public, a) in
    (match single_value, required with
     | Some value, _ ->
       value
       |> Ptime.date_of_string
       >|= create_answer is_admin entity_uuid answer
       >|= to_field
     | None, false -> to_field None |> CCResult.return
     | None, true -> no_value)
  | MultiSelect (({ validation; _ } as public), options, answer) ->
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
         |> CCOption.to_result (Error.Invalid Field.CustomFieldOption))
       |> CCList.all_ok
       >>= validate validation
       >|= create_answer is_admin entity_uuid answer
       >|= to_field)
  | Number (({ validation; _ } as public), answer) ->
    let to_field a = Public.Number (public, a) in
    (match single_value, required with
     | Some value, _ ->
       value
       |> CCInt.of_string
       |> CCOption.to_result (Error.NotANumber value)
       >>= validate validation
       >|= create_answer is_admin entity_uuid answer
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
       |> CCOption.to_result Error.InvalidOptionSelected
       >|= create_answer is_admin entity_uuid answer
       >|= to_field
     | None, false -> Ok (to_field None)
     | None, true -> no_value)
  | Text (({ validation; _ } as public), answer) ->
    let to_field a = Public.Text (public, a) in
    (match single_value, required with
     | Some value, _ ->
       value
       |> validate validation
       >|= create_answer is_admin entity_uuid answer
       >|= to_field
     | None, false -> Ok (to_field None)
     | None, true -> no_value)
;;

let validate_partial_update
      ?(is_admin = false)
      contact
      custom_field
      (partial_field, current_version, value)
  =
  let open PartialUpdate in
  let check_version old_v t =
    let open Pool_common.Version in
    if old_v |> value > (current_version |> value)
    then Error (Error.MeantimeUpdate partial_field)
    else t |> increment_version |> CCResult.return
  in
  let entity_uuid = Contact.(contact |> id |> Id.to_common) in
  let validate schema =
    let schema = Pool_conformist.(make Field.[ schema () ] CCFun.id) in
    Conformist.decode_and_validate schema [ partial_field |> Field.show, value ]
    |> CCResult.map_err to_conformist_error
  in
  let open CCResult in
  match[@warning "-4"] partial_field with
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
      @@ if Public.is_disabled is_admin m then Error Error.NotEligible else Ok m
    in
    let* custom_field =
      custom_field
      |> CCOption.to_result Error.InvalidHtmxRequest
      |> Lwt_result.lift
      >>= check_permission
      >>= CCFun.(validate_htmx ~is_admin ~entity_uuid value %> Lwt_result.lift)
    in
    let old_v = Public.version custom_field in
    custom_field |> custom |> check_version old_v |> Lwt_result.lift
;;

(* Replace all ids with the names of the custom_fields and
   custom_field_options *)
let changelog_to_human pool language ({ Changelog.changes; _ } as changelog) =
  let open Changelog.Changes in
  let id_of_string = CCFun.(Id.validate %> CCOption.of_result) in
  let rec yojson_id = function
    | `String id -> id_of_string id |> CCOption.map_or ~default:[] CCList.return
    | `List lst -> CCList.fold_left (fun acc str -> acc @ yojson_id str) [] lst
    | _ -> []
  in
  let rec collect_uuids acc = function
    | Assoc lst ->
      List.fold_left
        (fun acc (key, v) ->
           let uuids =
             id_of_string key
             |> function
             | None -> acc
             | Some uuid -> uuid :: acc
           in
           collect_uuids uuids v)
        acc
        lst
    | Change (before, after) ->
      CCList.fold_left (fun acc cur -> acc @ yojson_id cur) [] [ before; after ] @ acc
  in
  let get_or = CCOption.get_or in
  let uuids = collect_uuids [] changes in
  let%lwt names = find_names pool uuids in
  let tbl = Hashtbl.create (CCList.length names) in
  let () =
    CCList.iter
      (fun (id, name) ->
         let name =
           let open Name in
           find_opt language name |> CCOption.value ~default:(get_hd name) |> value_name
         in
         Hashtbl.add tbl (Pool_common.Id.value id) name)
      names
  in
  let rec replace_names = function
    | Assoc lst ->
      Assoc
        (CCList.map
           (fun (key, changes) ->
              let key = Hashtbl.find_opt tbl key |> get_or ~default:key in
              let changes = replace_names changes in
              key, changes)
           lst)
    | Change (before, after) ->
      let rec replace = function
        | `String str -> `String (Hashtbl.find_opt tbl str |> get_or ~default:str)
        | `List lst -> `List (CCList.map replace lst)
        | change -> change
      in
      Change (replace before, replace after)
  in
  let changes = replace_names changes in
  Lwt.return Changelog.{ changelog with changes }
;;
