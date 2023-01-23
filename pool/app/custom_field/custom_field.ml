include Entity
include Event

let find_by_model = Repo.find_by_model
let find_by_group = Repo.find_by_group
let find_ungrouped_by_model = Repo.find_ungrouped_by_model
let find = Repo.find

let find_all_by_contact ?is_admin pool id =
  Repo_public.find_all_by_contact ?is_admin pool id
;;

let find_all_required_by_contact pool id =
  Repo_public.find_all_required_by_contact pool id
;;

let find_multiple_by_contact = Repo_public.find_multiple_by_contact
let find_by_contact = Repo_public.find_by_contact
let upsert_answer = Repo_public.upsert_answer
let all_required_answered = Repo_public.all_required_answered
let find_option = Repo_option.find

let find_options_by_field pool id =
  let open Utils.Lwt_result.Infix in
  Repo_option.find_by_field pool id ||> CCList.map Repo_entity.Option.to_entity
;;

let find_group = Repo_group.find
let find_groups_by_model = Repo_group.find_by_model

let validate_htmx value (m : Public.t) =
  let open Public in
  let open CCResult.Infix in
  let open CCFun in
  let no_value = Error Pool_common.Message.NoValue in
  let required = Public.required m in
  let single_value =
    value
    |> CCList.head_opt
    |> flip CCOption.bind (fun v ->
         if CCString.is_empty v then None else Some v)
  in
  let go validation value = validation |> fst |> fun rule -> rule value in
  match m with
  | Boolean (public, answer) ->
    (* TODO: Find UI way to do this *)
    let to_field a = Public.Boolean (public, a) |> CCResult.return in
    let id = Answer.id_opt answer in
    (match single_value, required with
     | Some value, _ ->
       value
       |> Utils.Bool.of_string
       |> Answer.create ?id
       |> CCOption.pure
       |> to_field
     | None, false -> to_field None
     | None, true -> no_value)
  | MultiSelect (public, options, _) ->
    let to_field a = Public.MultiSelect (public, options, a) in
    (match value, required with
     | [], true -> no_value
     | vals, _ ->
       let open SelectOption in
       vals
       |> CCList.map (fun value ->
            let id = value |> Id.of_string in
            CCList.find_opt (fun option -> Id.equal option.Public.id id) options
            |> CCOption.to_result
                 Pool_common.Message.(Invalid Field.CustomFieldOption)
            >|= Answer.create)
       |> CCList.all_ok
       >|= to_field)
  | Number (({ validation; _ } as public), answer) ->
    let to_field a = Public.Number (public, a) in
    let id = Answer.id_opt answer in
    (match single_value, required with
     | Some value, _ ->
       value
       |> CCInt.of_string
       |> CCOption.to_result Message.(NotANumber value)
       >>= go validation
       >|= Answer.create ?id %> CCOption.pure %> to_field
     | None, false -> Ok (to_field None)
     | None, true -> no_value)
  | Select (public, options, answer) ->
    let to_field a = Public.Select (public, options, a) in
    let id = Answer.id_opt answer in
    (match single_value, required with
     | Some value, _ ->
       let open SelectOption in
       CCList.find_opt
         (fun option -> Id.equal option.Public.id (Id.of_string value))
         options
       |> CCOption.to_result Message.InvalidOptionSelected
       >|= Answer.create ?id %> CCOption.pure %> to_field
     | None, false -> Ok (to_field None)
     | None, true -> no_value)
  | Text (({ validation; _ } as public), answer) ->
    let to_field a = Public.Text (public, a) in
    let id = Answer.id_opt answer in
    (match single_value, required with
     | Some value, _ ->
       value |> go validation >|= Answer.create ?id %> CCOption.pure %> to_field
     | None, false -> Ok (to_field None)
     | None, true -> no_value)
;;

let validate_multiselect (public, options) values =
  let ids = values |> CCList.map SelectOption.Id.of_string in
  options
  |> CCList.filter_map (fun ({ SelectOption.Public.id; _ } as option) ->
       if CCList.mem ~eq:SelectOption.Id.equal id ids
       then Answer.create option |> CCOption.pure
       else None)
  |> fun answers -> Public.multiselect public options answers
;;

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
