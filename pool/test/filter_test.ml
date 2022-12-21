let contact_email_address = "jane.doe@email.com"
let lang = Pool_common.Language.En
let tenant = Tenant_test.Data.full_tenant

let allowed_email_suffixes =
  [ "mail.com" ]
  |> CCList.map Settings.EmailSuffix.create
  |> CCResult.flatten_l
  |> CCResult.get_exn
;;

let convert_id = CCFun.(Experiment.Id.value %> Pool_common.Id.of_string)

module TestContacts = struct
  let all () =
    Seed.Contacts.contact_ids
    |> Contact.find_multiple Test_utils.Data.database_label
  ;;

  let get_contact index =
    let open Utils.Lwt_result.Infix in
    index
    |> CCList.nth Seed.Contacts.contact_ids
    |> Contact.find Test_utils.Data.database_label
    ||> CCResult.get_exn
  ;;
end

module CustomFieldData = struct
  let admin_data = Custom_field_test.Data.admin
  let nr_of_siblings_answer = 3
  let published = () |> Custom_field.PublishedAt.create_now |> CCOption.pure

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
        ; published_at = published
        })
  ;;

  let nr_of_siblings_public answer_value =
    let open Custom_field in
    let answer = Answer.create answer_value |> CCOption.pure in
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

  let multi_select_option_data =
    let open Custom_field in
    let open CCList in
    range 0 5
    |> map (fun i -> [ lang, CCInt.to_string i ])
    |> map (Name.create [ lang ])
    |> CCList.all_ok
    |> CCResult.get_exn
    |> map (fun name -> Custom_field.SelectOption.Id.create (), name)
  ;;

  let multi_select_options =
    multi_select_option_data
    |> CCList.map (fun (id, name) -> Custom_field.SelectOption.create ~id name)
  ;;

  let multi_select_options_public =
    multi_select_option_data
    |> CCList.map (fun (id, name) ->
         Custom_field.SelectOption.Public.create ~id name)
  ;;

  let multi_select_options_by_index =
    CCList.map (CCList.nth multi_select_options)
  ;;

  let multi_select_options_public_by_index =
    CCList.map (CCList.nth multi_select_options_public)
  ;;

  let multi_select_custom_field =
    let open Custom_field in
    MultiSelect
      ( { id = Id.create ()
        ; model = Model.Contact
        ; name =
            Name.create [ lang ] [ lang, "Multi select" ] |> CCResult.get_exn
        ; hint = [] |> Hint.create |> CCResult.get_exn
        ; validation = Validation.pure
        ; required = false |> Required.create
        ; disabled = false |> Disabled.create
        ; custom_field_group_id = None
        ; admin = admin_data
        ; published_at = published
        }
      , multi_select_options )
  ;;

  let multi_select_custom_field_public answer_index =
    let open Custom_field in
    let answer =
      multi_select_options_public_by_index answer_index
      |> CCList.map Answer.create
    in
    let version = 0 |> Pool_common.Version.of_int in
    Public.MultiSelect
      ( { Public.id = id multi_select_custom_field
        ; name = name multi_select_custom_field
        ; hint = hint multi_select_custom_field
        ; validation = Validation.pure
        ; required = required multi_select_custom_field
        ; admin_overwrite = admin_data.Admin.overwrite
        ; admin_input_only = admin_data.Admin.input_only
        ; version
        }
      , multi_select_options_public
      , answer )
  ;;

  let create_nr_of_siblings () =
    Custom_field.Created nr_of_siblings |> Pool_event.custom_field
  ;;

  let answer_nr_of_siblings ?(answer_value = nr_of_siblings_answer) contacts =
    CCList.map
      (fun contact ->
        Custom_field.AnswerUpserted
          (nr_of_siblings_public answer_value, Contact.id contact)
        |> Pool_event.custom_field)
      contacts
  ;;

  let create_multi_select () =
    let open Custom_field in
    CCList.cons
      (Created multi_select_custom_field)
      (multi_select_options
      |> CCList.map (fun o -> OptionCreated (id multi_select_custom_field, o)))
    |> CCList.map Pool_event.custom_field
  ;;

  let answer_multi_select contacts answer_index =
    CCList.map
      (fun contact ->
        Custom_field.AnswerUpserted
          (multi_select_custom_field_public answer_index, Contact.id contact)
        |> Pool_event.custom_field)
      contacts
  ;;

  let publish_fields () =
    [ multi_select_custom_field; nr_of_siblings ]
    |> CCList.map (fun field ->
         Custom_field.Published field |> Pool_event.custom_field)
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

let firstname firstname =
  let open Filter in
  Pred
    (Predicate.create
       Key.(Hardcoded Firstname)
       Operator.Equal
       (Single (Str firstname)))
;;

let filter_contacts _ () =
  let%lwt () =
    let open Utils.Lwt_result.Infix in
    let%lwt contacts = TestContacts.all () in
    let%lwt experiment =
      Experiment.find_all Test_utils.Data.database_label () ||> CCList.hd
    in
    let%lwt () =
      (* Save field and answer with 3 *)
      CustomFieldData.(
        create_nr_of_siblings () :: answer_nr_of_siblings contacts)
      |> Lwt_list.iter_s
           (Pool_event.handle_event Test_utils.Data.database_label)
    in
    let filter = Filter.create None nr_of_siblings in
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
        (experiment.Experiment.id |> convert_id)
        experiment.Experiment.filter
      ||> CCResult.get_exn
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
    let open Utils.Lwt_result.Infix in
    let%lwt contact = TestContacts.get_contact 0 in
    let%lwt experiment =
      Experiment.find_all Test_utils.Data.database_label () ||> CCList.hd
    in
    let filter =
      Filter.(
        create
          None
          (And
             [ nr_of_siblings
             ; firstname (Contact.firstname contact |> Pool_user.Firstname.value)
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
        (experiment.Experiment.id |> convert_id)
        experiment.Experiment.filter
      ||> CCResult.get_exn
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
        []
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
        []
        filter
    in
    let expected =
      Error Pool_common.Message.(QueryNotCompatible (Field.Value, Field.Key))
    in
    Test_utils.check_result expected events |> Lwt.return
  in
  Lwt.return_unit
;;

let test_list_filter answer_index operator contact experiment expected =
  let%lwt () =
    let open Utils.Lwt_result.Infix in
    let filter =
      let open Filter in
      let value =
        Lst
          (answer_index
          |> CustomFieldData.multi_select_options_by_index
          |> CCList.map (fun option ->
               Option option.Custom_field.SelectOption.id))
      in
      create
        None
        Predicate.(
          Pred
            { key =
                Key.CustomField
                  (Custom_field.id CustomFieldData.multi_select_custom_field)
            ; operator
            ; value
            })
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
    let%lwt filtered_contacts =
      Contact.find_filtered
        Test_utils.Data.database_label
        (experiment.Experiment.id |> convert_id)
        experiment.Experiment.filter
      ||> CCResult.get_exn
    in
    let res = CCList.mem ~eq:Contact.equal contact filtered_contacts in
    Alcotest.(check bool "succeeds" expected res) |> Lwt.return
  in
  Lwt.return_unit
;;

let filter_by_list_contains_all _ () =
  let%lwt () =
    let open Utils.Lwt_result.Infix in
    let%lwt contact = TestContacts.get_contact 0 in
    let answer_index = [ 0; 1 ] in
    let%lwt () =
      (* Save field and answer *)
      CustomFieldData.(
        create_multi_select ()
        @ answer_multi_select [ contact ] answer_index
        @ publish_fields ())
      |> Lwt_list.iter_s
           (Pool_event.handle_event Test_utils.Data.database_label)
    in
    let%lwt experiment =
      Experiment.find_all Test_utils.Data.database_label () ||> CCList.hd
    in
    test_list_filter
      answer_index
      Filter.Operator.ContainsAll
      contact
      experiment
      true
  in
  Lwt.return_unit
;;

let filter_by_list_contains_none _ () =
  let%lwt () =
    let open Utils.Lwt_result.Infix in
    let%lwt contact = TestContacts.get_contact 0 in
    let answer_index = [ 1; 2 ] in
    let%lwt experiment =
      Experiment.find_all Test_utils.Data.database_label () ||> CCList.hd
    in
    test_list_filter
      answer_index
      Filter.Operator.ContainsNone
      contact
      experiment
      false
  in
  Lwt.return_unit
;;

let filter_by_list_contains_some _ () =
  let%lwt () =
    let open Utils.Lwt_result.Infix in
    let%lwt contact = TestContacts.get_contact 0 in
    let answer_index = [ 1; 2 ] in
    let%lwt experiment =
      Experiment.find_all Test_utils.Data.database_label () ||> CCList.hd
    in
    test_list_filter
      answer_index
      Filter.Operator.ContainsSome
      contact
      experiment
      true
  in
  Lwt.return_unit
;;

let retrieve_fitleterd_and_ordered_contacts _ () =
  let open Test_utils in
  let pool = Data.database_label in
  let%lwt () =
    let%lwt () =
      Seed.Contacts.(
        [ 11; 12 ]
        |> CCList.map create_contact
        |> fun contact_data -> create ~contact_data Data.database_label)
    in
    let find_contact id =
      id
      |> Format.asprintf "contact-%i@econ.uzh.ch"
      |> Pool_user.EmailAddress.of_string
      |> Contact.find_by_email pool
      |> Lwt.map CCResult.get_exn
    in
    let%lwt contact_one = find_contact 11 in
    let%lwt contact_two = find_contact 12 in
    let%lwt experiment =
      Experiment.find_all Test_utils.Data.database_label () |> Lwt.map CCList.hd
    in
    let filter =
      let open Filter in
      Pred
        (Predicate.create
           Key.(Hardcoded ContactLanguage)
           Operator.Equal
           (Single (Language Pool_common.Language.En)))
      |> create None
    in
    let%lwt () =
      Contact.
        [ NumInvitationsIncreased
            { contact_one with num_invitations = NumberOfInvitations.of_int 3 }
          |> Pool_event.contact
        ]
      |> Lwt_list.iter_s (Pool_event.handle_event Data.database_label)
    in
    let order_by =
      let open Mailing.Distribution in
      Sorted [ InvitationCount, SortOrder.Ascending ] |> get_order_element
    in
    let%lwt contacts =
      Contact.find_filtered
        ~order_by
        Data.database_label
        Experiment.(experiment.Experiment.id |> Id.to_common)
        (Some filter)
      |> Lwt.map CCResult.get_exn
    in
    let get_index contact =
      CCList.find_idx
        (fun c -> Contact.(Pool_common.Id.equal (id c) (id contact)))
        contacts
      |> CCOption.get_exn_or "Cannot find contact"
      |> fst
    in
    let index_one = get_index contact_one in
    let index_two = get_index contact_two in
    let res = index_two < index_one in
    let expected = true in
    Alcotest.(check bool "succeeds" expected res) |> Lwt.return
  in
  Lwt.return_unit
;;

let create_filter_template_with_template _ () =
  let open Pool_common in
  let%lwt () =
    let open CCResult in
    let open Filter in
    let template_id = Pool_common.Id.create () in
    let template =
      Pred
        Predicate.
          { key = Key.(Hardcoded Name)
          ; operator = Operator.Equal
          ; value = Single (Str "Foo")
          }
      |> create ~id:template_id None
    in
    let filter = Template template_id in
    let events =
      let open Cqrs_command.Filter_command.Create in
      Message.Field.[ show Title, [ "Some title" ] ]
      |> decode
      >>= handle [] [ template ] filter
    in
    let expected = Error Message.FilterMustNotContainTemplate in
    Alcotest.(
      check
        (result (list Test_utils.event) Test_utils.error)
        "succeeds"
        expected
        events)
    |> Lwt.return
  in
  Lwt.return_unit
;;
