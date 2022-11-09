let contact_email_address = "jane.doe@email.com"
let lang = Pool_common.Language.En

let allowed_email_suffixes =
  [ "mail.com" ]
  |> CCList.map Settings.EmailSuffix.create
  |> CCResult.flatten_l
  |> CCResult.get_exn
;;

module TestContacts = struct
  open Contact_test
  open Contact_command

  let create (email, user_id) =
    email
    |> contact_info
    |> sign_up_contact
    |> SignUp.decode
    |> Pool_common.Utils.get_or_failwith
    |> SignUp.handle ~user_id ~allowed_email_suffixes None
  ;;

  let verify contact =
    let open Utils.Lwt_result.Infix in
    let terms = AcceptTermsAndConditions.handle contact |> CCResult.get_exn in
    let%lwt verify =
      Contact.email_address contact
      |> Email.find_unverified_by_address Test_utils.Data.database_label
      ||> CCResult.get_exn
      ||> fun email ->
      VerifyEmail.(handle contact { email } |> CCResult.get_exn)
    in
    [ terms; verify ] |> CCList.flatten |> Lwt_result.return
  ;;

  let prepare data =
    let%lwt () =
      let open CCResult in
      data
      |> CCList.map create
      |> CCList.all_ok
      |> get_exn
      |> CCList.flatten
      |> Lwt_list.iter_s
           (Pool_event.handle_event Test_utils.Data.database_label)
    in
    let%lwt contacts =
      data
      |> CCList.map snd
      |> Contact.find_multiple Test_utils.Data.database_label
    in
    let%lwt () =
      let open Lwt.Infix in
      contacts
      |> Lwt_list.map_s verify
      >|= CCList.all_ok
      >|= CCResult.get_exn
      >|= CCList.flatten
      >>= Lwt_list.iter_s
            (Pool_event.handle_event Test_utils.Data.database_label)
    in
    contacts |> Lwt.return
  ;;
end

module CustomFieldData = struct
  let admin_data = Custom_field_test.Data.admin
  let nr_of_siblings_answer = 3

  let nr_of_siblings =
    Custom_field.(
      Number
        { id = Id.create ()
        ; model = Model.Contact
        ; name =
            Name.create [ lang ] [ lang, "Nr of siblings" ] |> CCResult.get_exn
        ; hint = [] |> Hint.create |> CCResult.get_exn
        ; validation = Validation.pure
        ; required = false |> Required.create
        ; disabled = false |> Disabled.create
        ; custom_field_group_id = None
        ; admin = admin_data
        })
  ;;

  let nr_of_siblings_public () =
    let open Custom_field in
    let answer = Answer.create nr_of_siblings_answer |> CCOption.pure in
    let version = 0 |> Pool_common.Version.of_int in
    Public.Number
      ( { Public.id = id nr_of_siblings
        ; name = name nr_of_siblings
        ; hint = hint nr_of_siblings
        ; validation = Validation.pure
        ; required = required nr_of_siblings
        ; admin_overwrite = admin_data.Admin.overwrite
        ; admin_input_only = admin_data.Admin.input_only
        ; version
        }
      , answer )
  ;;

  let create () = Custom_field.Created nr_of_siblings |> Pool_event.custom_field

  let answer contacts =
    CCList.map
      (fun contact ->
        Custom_field.AnswerUpserted
          (nr_of_siblings_public (), Contact.id contact)
        |> Pool_event.custom_field)
      contacts
  ;;

  let data =
    let rec go i acc =
      if i <= 0
      then acc
      else
        go
          (i - 1)
          ((Format.asprintf "%i-test@mail.com" i, Pool_common.Id.create ())
          :: acc)
    in
    go 5 []
  ;;

  let create_contacts () = TestContacts.prepare data

  let find_contacts () =
    data
    |> CCList.map snd
    |> Contact.find_multiple Test_utils.Data.database_label
  ;;
end

let nr_of_siblings =
  let open Filter in
  Pred
    (Predicate.create
       Key.(CustomField (CustomFieldData.nr_of_siblings |> Custom_field.id))
       Operator.Equal
       (Single (Nr (CustomFieldData.nr_of_siblings_answer |> CCFloat.of_int))))
;;

let email email_address =
  let open Filter in
  Pred
    (Predicate.create
       Key.(Hardcoded Email)
       Operator.Equal
       (Single (Str email_address)))
;;

let filter_contacts _ () =
  let%lwt () =
    let%lwt contacts = CustomFieldData.create_contacts () in
    let%lwt experiment =
      Experiment.find_all Test_utils.Data.database_label () |> Lwt.map CCList.hd
    in
    let%lwt () =
      (* Save field and answer with 3 *)
      CustomFieldData.(create () :: answer contacts)
      |> Lwt_list.iter_s
           (Pool_event.handle_event Test_utils.Data.database_label)
    in
    let filter = Filter.create nr_of_siblings in
    let experiment = Experiment.{ experiment with filter = Some filter } in
    let%lwt () =
      (* Save filter *)
      [ Filter.Created filter |> Pool_event.filter
      ; Experiment.Updated experiment |> Pool_event.experiment
      ]
      |> Lwt_list.iter_s
           (Pool_event.handle_event Test_utils.Data.database_label)
    in
    let expected = true in
    let%lwt filtered_contacts =
      Contact.find_filtered
        Test_utils.Data.database_label
        experiment.Experiment.id
        (Experiment.filter_predicate experiment)
    in
    let res =
      filtered_contacts
      |> (CCList.subset ~eq:(fun filtered contact ->
            Pool_common.Id.equal (Contact.id contact) (Contact.id filtered)))
           contacts
    in
    Alcotest.(check bool "succeeds" expected res) |> Lwt.return
  in
  Lwt.return_unit
;;

let filter_by_email _ () =
  let%lwt () =
    let%lwt contact =
      CustomFieldData.data
      |> CCList.hd
      |> snd
      |> Contact.find Test_utils.Data.database_label
      |> Lwt.map CCResult.get_exn
    in
    let%lwt experiment =
      Experiment.find_all Test_utils.Data.database_label () |> Lwt.map CCList.hd
    in
    let filter =
      Filter.(
        create
          (And
             [ nr_of_siblings
             ; email
                 (Contact.email_address contact |> Pool_user.EmailAddress.value)
             ]))
    in
    let experiment = Experiment.{ experiment with filter = Some filter } in
    let%lwt () =
      (* Save filter *)
      [ Filter.Created filter |> Pool_event.filter
      ; Experiment.Updated experiment |> Pool_event.experiment
      ]
      |> Lwt_list.iter_s
           (Pool_event.handle_event Test_utils.Data.database_label)
    in
    let expected = true in
    let%lwt filtered_contacts =
      Contact.find_filtered
        Test_utils.Data.database_label
        experiment.Experiment.id
        (Experiment.filter_predicate experiment)
    in
    let res = CCList.mem ~eq:Contact.equal contact filtered_contacts in
    Alcotest.(check bool "succeeds" expected res) |> Lwt.return
  in
  Lwt.return_unit
;;

let validate_filter_with_unknown_field _ () =
  let open Test_utils in
  let%lwt () =
    let experiment = Model.create_experiment () in
    let%lwt key_list = Filter.all_keys Data.database_label in
    let filter =
      let open Filter in
      Pred
        (Predicate.create
           Key.(CustomField ("Unknown field id" |> Custom_field.Id.of_string))
           Operator.Equal
           (Single (Nr 1.2)))
    in
    let events =
      Cqrs_command.Experiment_command.UpdateFilter.handle
        experiment
        key_list
        filter
    in
    let expected = Error Pool_common.Message.(Invalid Field.Key) in
    Test_utils.check_result expected events |> Lwt.return
  in
  Lwt.return_unit
;;

let validate_filter_with_invalid_value _ () =
  let open Test_utils in
  let%lwt () =
    let experiment = Model.create_experiment () in
    let%lwt key_list = Filter.all_keys Data.database_label in
    let filter =
      let open Filter in
      Pred
        (Predicate.create
           Key.(CustomField (CustomFieldData.nr_of_siblings |> Custom_field.id))
           Operator.Equal
           (Single (Str "Not a number")))
    in
    let events =
      Cqrs_command.Experiment_command.UpdateFilter.handle
        experiment
        key_list
        filter
    in
    let expected =
      Error Pool_common.Message.(FilterNotCompatible (Field.Value, Field.Key))
    in
    Test_utils.check_result expected events |> Lwt.return
  in
  Lwt.return_unit
;;
