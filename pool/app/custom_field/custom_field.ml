include Entity
include Event

let find_all = Repo.find_all
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
  let open Lwt.Infix in
  Repo_option.find_by_field pool id >|= CCList.map Repo_entity.Option.to_entity
;;

let find_group = Repo_group.find
let find_groups_by_model = Repo_group.find_by_model

let validate_htmx value (m : Public.t) =
  let open Public in
  let open CCResult.Infix in
  let go validation value = validation |> fst |> fun rule -> rule value in
  match m with
  | Boolean (public, answer) ->
    let id = Answer.id_opt answer in
    value
    |> CCList.head_opt
    |> CCOption.map_or ~default:false Utils.Bool.of_string
    |> Answer.create ?id
    |> (fun a : t -> Boolean (public, a |> CCOption.pure))
    |> CCResult.return
  | MultiSelect (public, options, _) ->
    value
    |> CCList.map (fun value ->
         let id = value |> SelectOption.Id.of_string in
         CCList.find_opt
           (fun option -> SelectOption.Id.equal option.SelectOption.id id)
           options
         |> CCOption.to_result
              Pool_common.Message.(Invalid Field.CustomFieldOption)
         >|= Answer.create)
    |> CCList.all_ok
    >|= fun answers : t -> MultiSelect (public, options, answers)
  | Number (({ validation; _ } as public), answer) ->
    let id = Answer.id_opt answer in
    value
    |> CCList.head_opt
    |> CCOption.to_result Pool_common.Message.NoValue
    >>= fun value ->
    value
    |> CCInt.of_string
    |> CCOption.to_result Message.(NotANumber value)
    >>= go validation
    >|= Answer.create ?id
    >|= fun a : t -> Number (public, a |> CCOption.pure)
  | Select (public, options, answer) ->
    let id = Answer.id_opt answer in
    let selected =
      value
      |> CCList.head_opt
      |> CCOption.map SelectOption.Id.of_string
      |> CCFun.flip CCOption.bind (fun id ->
           CCList.find_opt
             (fun option -> SelectOption.Id.equal option.SelectOption.id id)
             options)
    in
    selected
    |> CCOption.to_result Message.InvalidOptionSelected
    >|= Answer.create ?id
    >|= fun a : t -> Select (public, options, a |> CCOption.pure)
  | Text (({ validation; _ } as public), answer) ->
    let id = Answer.id_opt answer in
    value
    |> CCList.head_opt
    |> CCOption.to_result Pool_common.Message.NoValue
    >>= go validation
    >|= Answer.create ?id
    >|= fun a : t -> Text (public, a |> CCOption.pure)
;;

let validate_multiselect (public, options) values =
  let ids = values |> CCList.map SelectOption.Id.of_string in
  options
  |> CCList.filter_map (fun ({ SelectOption.id; _ } as option) ->
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
