open Utils.Lwt_result.Infix
module Command = Cqrs_command.Tags_command
module Conformist = Pool_common.Utils.PoolConformist
module Field = Pool_common.Message.Field
module Model = Test_utils.Model

let get_or_failwith = Pool_common.Utils.get_or_failwith
let testable_tag = Alcotest.testable Tags.pp Tags.equal

let check_tag_result =
  let open Alcotest in
  check (result testable_tag Test_utils.error) "succeeds"
;;

module Data = struct
  module Tag = struct
    let id = Tags.Id.create ()
    let title = Tags.Title.of_string "Testing Tags"
    let title_updated = Tags.Title.of_string "Updated Testing Tag"

    let description =
      Tags.Description.of_string
        "The description to let you know, this is only a testing tag."
    ;;

    let description_updated = Tags.Description.of_string "Cleanup description"
    let model = Tags.Model.Contact

    let create =
      [ Field.(Title |> show), [ title |> Tags.Title.value ]
      ; Field.(Description |> show), [ description |> Tags.Description.value ]
      ; Field.(Model |> show), [ model |> Tags.Model.show ]
      ]
    ;;

    let update =
      [ Field.(Title |> show), [ title_updated |> Tags.Title.value ]
      ; ( Field.(Description |> show)
        , [ description_updated |> Tags.Description.value ] )
      ; Field.(Model |> show), [ model |> Tags.Model.show ]
      ]
    ;;

    let create_with_description () =
      Tags.create ~id ~description title model |> get_or_failwith
    ;;

    let create_without_description () =
      Tags.create ~id title model |> get_or_failwith
    ;;

    let updated_tag () =
      create_with_description ()
      |> fun tag ->
      Tags.
        { tag with
          title = title_updated
        ; description = Some description_updated
        }
    ;;
  end
end

let create_event () =
  let open Command.Create in
  let tag = Data.Tag.create_with_description () in
  let events =
    Data.Tag.create
    |> Http_utils.remove_empty_values
    |> decode
    |> get_or_failwith
    |> handle ~id:Data.Tag.id
  in
  let expected = Ok [ Tags.Created tag |> Pool_event.tags ] in
  Test_utils.check_result expected events
;;

let update_event () =
  let open Command.Update in
  let tag = Data.Tag.create_with_description () in
  let events =
    Data.Tag.update
    |> Http_utils.remove_empty_values
    |> decode
    |> get_or_failwith
    |> handle tag
  in
  let updated_tag = Data.Tag.updated_tag () in
  let expected = Ok [ Tags.Updated updated_tag |> Pool_event.tags ] in
  Test_utils.check_result expected events
;;

let create_persistent _ () =
  let tag = Data.Tag.create_with_description () in
  let%lwt () =
    Pool_event.handle_events
      Test_utils.Data.database_label
      [ Tags.Created tag |> Pool_event.tags ]
  in
  let%lwt found_tag = Tags.find Test_utils.Data.database_label Data.Tag.id in
  let expected = Ok tag in
  let () = check_tag_result expected found_tag in
  Lwt.return_unit
;;

let create_persistent_fail _ () =
  let tag = Data.Tag.create_with_description () in
  let%lwt () =
    Lwt.catch
      (fun () ->
        Pool_event.handle_events
          Test_utils.Data.database_label
          [ Tags.Created tag |> Pool_event.tags ])
      (fun exeption ->
        let correct_exn =
          CCString.mem
            ~sub:"failed: Error 1062, Duplicate entry"
            (Printexc.to_string exeption)
        in
        Alcotest.(check bool "duplicate error" true correct_exn) |> Lwt.return)
  in
  Lwt.return_unit
;;

let update_persistent _ () =
  let tag = Data.Tag.updated_tag () in
  let%lwt () =
    Pool_event.handle_events
      Test_utils.Data.database_label
      [ Tags.Updated tag |> Pool_event.tags ]
  in
  let%lwt found_tag = Tags.find Test_utils.Data.database_label Data.Tag.id in
  let expected = Ok tag in
  let () = check_tag_result expected found_tag in
  Lwt.return_unit
;;

let assign_tag_to_contact _ () =
  let open Command.AssignTagToContact in
  let%lwt contact = Test_utils.Repo.first_contact () in
  let%lwt tag = Test_utils.Repo.first_tag () in
  let%lwt events =
    [ Field.(Tag |> show), [ Tags.(tag.id |> Id.value) ] ]
    |> Http_utils.remove_empty_values
    |> decode
    |> Lwt_result.lift
    >>= Tags.find Test_utils.Data.database_label
    >== validate
    >== handle contact
    ||> get_or_failwith
  in
  let%lwt () = Pool_event.handle_events Test_utils.Data.database_label events in
  let%lwt found_tagged =
    Tags.(find_all_of_entity Test_utils.Data.database_label Model.Contact)
      (contact |> Contact.id)
  in
  let expected = [ tag ] in
  let () =
    Alcotest.(
      check (list testable_tag) "all needed tags assigned" expected found_tagged)
  in
  Lwt.return_unit
;;

let remove_tag_from_contact _ () =
  let open Command.RemoveTagFromContact in
  let%lwt contact = Test_utils.Repo.first_contact () in
  let%lwt tag = Test_utils.Repo.first_tag () in
  let events = tag |> handle contact |> get_or_failwith in
  let%lwt () = Pool_event.handle_events Test_utils.Data.database_label events in
  let%lwt found_tagged =
    Tags.(find_all_of_entity Test_utils.Data.database_label Model.Contact)
      (contact |> Contact.id)
  in
  let expected = [] in
  let () =
    Alcotest.(
      check (list testable_tag) "all needed tags assigned" expected found_tagged)
  in
  Lwt.return_unit
;;

let try_assign_experiment_tag_to_contact _ () =
  let open Command.AssignTagToContact in
  let%lwt contact = Test_utils.Repo.first_contact () in
  let tag = Tags.(create Data.Tag.title Model.Experiment) |> get_or_failwith in
  let%lwt () =
    Pool_event.handle_events
      Test_utils.Data.database_label
      [ Tags.Created tag |> Pool_event.tags ]
  in
  let%lwt events =
    [ Field.(Tag |> show), [ Tags.(tag.id |> Id.value) ] ]
    |> Http_utils.remove_empty_values
    |> decode
    |> Lwt_result.lift
    >>= Tags.find Test_utils.Data.database_label
    >== validate
    >== handle contact
  in
  let () =
    Alcotest.(
      check
        (result (list Test_utils.event) Test_utils.error)
        "invalid error"
        (Error Pool_common.Message.(Invalid Field.Tag))
        events)
  in
  Lwt.return_unit
;;
