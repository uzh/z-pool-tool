include Entity
include Event

let find_all = Repo.find_all
let find_by_model = Repo.find_by_model
let find_by_group = Repo.find_by_group
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

let validate pool value (m : Public.t) =
  let open Public in
  let open CCResult.Infix in
  let go validation value = validation |> fst |> fun rule -> rule value in
  match m with
  | Boolean (public, answer) ->
    let id, version = Answer.answer_meta answer in
    value
    |> Utils.Bool.of_string
    |> Answer.create ?id ?version
    |> (fun a : t -> Boolean (public, a |> CCOption.pure))
    |> Lwt_result.return
  | MultiSelect (public, options, answers) ->
    (* Could just the updated answer be passed to the repo and deleted, if
       already existing? *)
    let open Lwt_result.Syntax in
    let* option = value |> SelectOption.Id.of_string |> Repo_option.find pool in
    let answers =
      CCList.find_opt
        (fun { Answer.value; _ } -> SelectOption.equal value option)
        answers
      |> function
      | None -> Answer.create option :: answers
      | Some answer ->
        CCList.remove ~eq:(Answer.equal SelectOption.equal) ~key:answer answers
    in
    (MultiSelect (public, options, answers) : t) |> Lwt_result.return
  | Number (({ validation; _ } as public), answer) ->
    let id, version = Answer.answer_meta answer in
    let res =
      value
      |> CCInt.of_string
      |> CCOption.to_result Message.(NotANumber value)
      >>= fun i ->
      i
      |> go validation
      >|= Answer.create ?id ?version
      >|= fun a : t -> (Number (public, a |> CCOption.pure) : t)
    in
    res |> Lwt_result.lift
  | Select (public, options, answer) ->
    let id, version = Answer.answer_meta answer in
    let value = value |> SelectOption.Id.of_string in
    let selected =
      CCList.find_opt
        (fun option -> SelectOption.Id.equal option.SelectOption.id value)
        options
    in
    selected
    |> CCOption.to_result Message.InvalidOptionSelected
    >|= Answer.create ?id ?version
    >|= (fun a : t -> Select (public, options, a |> CCOption.pure))
    |> Lwt_result.lift
  | Text (({ validation; _ } as public), answer) ->
    let id, version = Answer.answer_meta answer in
    value
    |> go validation
    >|= Answer.create ?id ?version
    >|= (fun a : t -> Text (public, a |> CCOption.pure))
    |> Lwt_result.lift
;;
