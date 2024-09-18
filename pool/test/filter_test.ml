open Test_utils
open Pool_message
module TestSeed = Test_seed
module Operator = Filter.Operator

let equal_operator = FilterHelper.equal
let get_exn = get_or_failwith
let contact_email_address = "jane.doe@email.com"
let lang = Pool_common.Language.En
let tenant = Tenant_test.Data.full_tenant

let current_user () =
  Integration_utils.AdminRepo.create () |> Lwt.map Pool_context.admin
;;

let allowed_email_suffixes =
  [ "mail.com" ]
  |> CCList.map Settings.EmailSuffix.create
  |> CCResult.flatten_l
  |> get_exn
;;

let convert_id = CCFun.(Experiment.Id.value %> Pool_common.Id.of_string)

module TestContacts = struct
  let all () =
    TestSeed.Contacts.contact_ids |> Contact.find_multiple Data.database_label
  ;;

  let get_contact index =
    let open Utils.Lwt_result.Infix in
    index
    |> CCList.nth TestSeed.Contacts.contact_ids
    |> Contact.find Data.database_label
    ||> get_exn
  ;;

  let persist_contact_update current_user contact =
    let open Contact in
    Updated contact
    |> Pool_event.contact
    |> Pool_event.handle_event Data.database_label current_user
    |> Lwt.map (CCFun.const contact)
  ;;
end

module CustomFieldData = struct
  let published = () |> Custom_field.PublishedAt.create_now |> CCOption.pure

  let create_custom_field field_name encoder =
    let open Custom_field_test in
    Custom_field.
      { id = Id.create ()
      ; model = Model.Contact
      ; name = Name.create [ lang ] [ lang, field_name ] |> get_exn
      ; hint = [] |> Hint.create |> get_exn
      ; validation = Validation.pure
      ; required = false |> Required.create
      ; disabled = false |> Disabled.create
      ; custom_field_group_id = None
      ; admin_hint = Data.admin_hint
      ; admin_override = Data.admin_override
      ; admin_view_only = Data.admin_view_only
      ; admin_input_only = Data.admin_input_only
      ; published_at = published
      ; prompt_on_registration = false |> PromptOnRegistration.create
      ; show_on_session_close_page = false
      ; show_on_session_detail_page = false
      }
    |> encoder
  ;;

  let save_custom_field t = Custom_field.Created t |> Pool_event.custom_field

  let save_options field =
    let field_id = Custom_field.id field in
    CCList.map (fun option ->
      Custom_field.OptionCreated (field_id, option) |> Pool_event.custom_field)
  ;;

  module NrOfSiblings = struct
    let answer_value = 3

    let field =
      create_custom_field "Nr of siblings" (fun a -> Custom_field.Number a)
    ;;

    let public ?(entity_uuid = Pool_common.Id.create ()) is_admin answer_value =
      let open Custom_field in
      let open Custom_field_test in
      let answer =
        match is_admin with
        | true -> Answer.create ?admin_value:answer_value entity_uuid None
        | false -> Answer.create entity_uuid answer_value
      in
      let version = 0 |> Pool_common.Version.of_int in
      Public.Number
        ( { Public.id = id field
          ; name = name field
          ; hint = hint field
          ; validation = Validation.pure
          ; required = required field
          ; admin_override = Data.admin_override
          ; admin_input_only = Data.admin_input_only
          ; prompt_on_registration = Data.prompt_on_registration
          ; version
          }
        , Some answer )
    ;;

    let save () = save_custom_field field

    let save_answers ~answer_value ?admin contacts =
      CCList.map
        (fun contact ->
          let user =
            admin |> CCOption.value ~default:(Pool_context.Contact contact)
          in
          Custom_field.AnswerUpserted
            ( public (CCOption.is_some admin) answer_value
            , Contact.id contact
            , user )
          |> Pool_event.custom_field)
        contacts
    ;;
  end

  module Birthday = struct
    let answer_value =
      "1990-01-01" |> Pool_model.Base.Ptime.date_of_string |> get_exn
    ;;

    let field = create_custom_field "Birthday" (fun a -> Custom_field.Date a)

    let public ?(entity_uuid = Pool_common.Id.create ()) is_admin answer_value =
      let open Custom_field in
      let open Custom_field_test in
      let answer =
        match is_admin with
        | true -> Answer.create ?admin_value:answer_value entity_uuid None
        | false -> Answer.create entity_uuid answer_value
      in
      let version = 0 |> Pool_common.Version.of_int in
      Public.Date
        ( { Public.id = id field
          ; name = name field
          ; hint = hint field
          ; validation = Validation.pure
          ; required = required field
          ; admin_override = Data.admin_override
          ; admin_input_only = Data.admin_input_only
          ; prompt_on_registration = Data.prompt_on_registration
          ; version
          }
        , Some answer )
    ;;

    let save () = save_custom_field field

    let save_answers ~answer_value ?admin contacts =
      CCList.map
        (fun contact ->
          let user =
            admin |> CCOption.value ~default:(Pool_context.Contact contact)
          in
          Custom_field.AnswerUpserted
            ( public (CCOption.is_some admin) answer_value
            , Contact.id contact
            , user )
          |> Pool_event.custom_field)
        contacts
    ;;

    let filter ?date operator () =
      let open Filter in
      let value = date |> CCOption.value ~default:answer_value in
      let query =
        Pred
          (Predicate.create
             Key.(CustomField (field |> Custom_field.id))
             operator
             (Single (Date value)))
      in
      create None query
    ;;
  end

  module SelectField = struct
    let option_to_public =
      let open Custom_field in
      fun { SelectOption.id; name; _ } -> SelectOption.Public.create ~id name
    ;;

    let options =
      [ "1"; "2" ]
      |> CCList.map
           Custom_field.(
             fun label ->
               [ lang, label ]
               |> Name.create [ lang ]
               |> get_or_failwith
               |> SelectOption.create)
    ;;

    let public_options = options |> CCList.map option_to_public
    let default_answer = CCList.hd options

    let field =
      create_custom_field "Select" (fun a -> Custom_field.Select (a, options))
    ;;

    let public ?(entity_uuid = Pool_common.Id.create ()) is_admin answer =
      let open Custom_field in
      let open Custom_field_test in
      let answer =
        match is_admin with
        | true -> Answer.create ?admin_value:answer entity_uuid None
        | false -> Answer.create entity_uuid answer
      in
      let version = 0 |> Pool_common.Version.of_int in
      Public.Select
        ( { Public.id = id field
          ; name = name field
          ; hint = hint field
          ; validation = Validation.pure
          ; required = required field
          ; admin_override = Data.admin_override
          ; admin_input_only = Data.admin_input_only
          ; prompt_on_registration = Data.prompt_on_registration
          ; version
          }
        , public_options
        , Some answer )
    ;;

    let save () = save_custom_field field

    let save_answer answer ?admin contacts =
      CCList.map
        (fun contact ->
          let user =
            admin |> CCOption.value ~default:(Pool_context.Contact contact)
          in
          Custom_field.AnswerUpserted
            (public (CCOption.is_some admin) answer, Contact.id contact, user)
          |> Pool_event.custom_field)
        contacts
    ;;

    let filter answers operator () =
      let open Filter in
      let value =
        answers
        |> CCList.map (fun opt -> Option opt.Custom_field.SelectOption.id)
      in
      let query =
        Pred
          (Predicate.create
             Key.(CustomField (field |> Custom_field.id))
             operator
             (Lst value))
      in
      create None query
    ;;

    let init current_user =
      save () :: save_options field options
      |> Pool_event.handle_events Test_utils.Data.database_label current_user
    ;;
  end

  let admin_override_nr_field =
    let open Custom_field_test in
    Custom_field.(
      Number
        { id = Id.create ()
        ; model = Model.Contact
        ; name =
            Name.create [ lang ] [ lang, "admin_override_nr_field" ] |> get_exn
        ; hint = [] |> Hint.create |> get_exn
        ; validation = Validation.pure
        ; required = false |> Required.create
        ; disabled = false |> Disabled.create
        ; custom_field_group_id = None
        ; admin_hint = Data.admin_hint
        ; admin_override = true |> AdminOverride.create
        ; admin_view_only = Data.admin_view_only
        ; admin_input_only = Data.admin_input_only
        ; published_at = published
        ; prompt_on_registration = false |> PromptOnRegistration.create
        ; show_on_session_close_page = false
        ; show_on_session_detail_page = false
        })
  ;;

  let admin_override_nr_field_public
    ?(entity_uuid = Pool_common.Id.create ())
    is_admin
    answer_value
    =
    let open Custom_field in
    let answer =
      match is_admin with
      | true -> Answer.create ~admin_value:answer_value entity_uuid None
      | false -> Answer.create entity_uuid (Some answer_value)
    in
    let version = 0 |> Pool_common.Version.of_int in
    Public.Number
      ( { Public.id = id admin_override_nr_field
        ; name = name admin_override_nr_field
        ; hint = hint admin_override_nr_field
        ; validation = Validation.pure
        ; required = required admin_override_nr_field
        ; admin_override = admin_override admin_override_nr_field
        ; admin_input_only = admin_input_only admin_override_nr_field
        ; prompt_on_registration =
            prompt_on_registration admin_override_nr_field
        ; version
        }
      , Some answer )
  ;;

  let create_admin_override_nr_field () =
    Custom_field.Created admin_override_nr_field |> Pool_event.custom_field
  ;;

  let answer_admin_override_nr_field ?admin ~answer_value contacts =
    CCList.map
      (fun contact ->
        let user =
          admin |> CCOption.value ~default:(Pool_context.Contact contact)
        in
        Custom_field.AnswerUpserted
          ( admin_override_nr_field_public (CCOption.is_some admin) answer_value
          , Contact.id contact
          , user )
        |> Pool_event.custom_field)
      contacts
  ;;

  let multi_select_option_data =
    let open Custom_field in
    let open CCList in
    range 0 5
    |> map (fun i -> [ lang, CCInt.to_string i ])
    |> map (Name.create [ lang ])
    |> CCList.all_ok
    |> get_exn
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
    let open Custom_field_test in
    MultiSelect
      ( { id = Id.create ()
        ; model = Model.Contact
        ; name = Name.create [ lang ] [ lang, "Multi select" ] |> get_exn
        ; hint = [] |> Hint.create |> get_exn
        ; validation = Validation.pure
        ; required = false |> Required.create
        ; disabled = false |> Disabled.create
        ; custom_field_group_id = None
        ; admin_hint = Data.admin_hint
        ; admin_override = Data.admin_override
        ; admin_view_only = Data.admin_view_only
        ; admin_input_only = Data.admin_input_only
        ; published_at = published
        ; prompt_on_registration = false |> PromptOnRegistration.create
        ; show_on_session_close_page = false
        ; show_on_session_detail_page = false
        }
      , multi_select_options )
  ;;

  let multi_select_custom_field_public
    ?(entity_uuid = Pool_common.Id.create ())
    answer_index
    =
    let open Custom_field in
    let open Custom_field_test in
    let answer =
      multi_select_options_public_by_index answer_index
      |> CCOption.pure
      |> Answer.create entity_uuid
      |> CCOption.pure
    in
    let version = 0 |> Pool_common.Version.of_int in
    Public.MultiSelect
      ( { Public.id = id multi_select_custom_field
        ; name = name multi_select_custom_field
        ; hint = hint multi_select_custom_field
        ; validation = Validation.pure
        ; required = required multi_select_custom_field
        ; admin_override = Data.admin_override
        ; admin_input_only = Data.admin_input_only
        ; prompt_on_registration = Data.prompt_on_registration
        ; version
        }
      , multi_select_options_public
      , answer )
  ;;

  let create_multi_select () =
    let open Custom_field in
    CCList.cons
      (Created multi_select_custom_field)
      (multi_select_options
       |> CCList.map (fun o -> OptionCreated (id multi_select_custom_field, o))
      )
    |> CCList.map Pool_event.custom_field
  ;;

  let answer_multi_select contacts answer_index =
    CCList.map
      (fun contact ->
        Custom_field.AnswerUpserted
          ( multi_select_custom_field_public answer_index
          , Contact.id contact
          , Pool_context.Contact contact )
        |> Pool_event.custom_field)
      contacts
  ;;

  let publish_fields () =
    [ multi_select_custom_field; NrOfSiblings.field ]
    |> CCList.map (fun field ->
      Custom_field.Published field |> Pool_event.custom_field)
  ;;
end

let nr_of_siblings_filter ?nr () =
  let open Filter in
  let value =
    nr |> CCOption.value ~default:CustomFieldData.NrOfSiblings.answer_value
  in
  Pred
    (Predicate.create
       Key.(CustomField (CustomFieldData.NrOfSiblings.field |> Custom_field.id))
       equal_operator
       (Single (Nr (value |> CCFloat.of_int))))
;;

let admin_override_nr_field_filter ~nr () =
  let open Filter in
  Pred
    (Predicate.create
       Key.(
         CustomField (CustomFieldData.admin_override_nr_field |> Custom_field.id))
       equal_operator
       (Single (Nr (nr |> CCFloat.of_int))))
;;

let participation_filter experiment_ids operator () =
  let open Filter in
  Pred
    (Predicate.create
       Key.(Hardcoded Participation)
       operator
       (Lst
          (experiment_ids
           |> CCList.map (fun id -> Str (id |> Experiment.Id.value)))))
;;

let tag_filter tag_ids operator () =
  let open Filter in
  Pred
    (Predicate.create
       Key.(Hardcoded Tag)
       operator
       (Lst (tag_ids |> CCList.map (fun id -> Str (id |> Tags.Id.value)))))
;;

let firstname firstname =
  let open Filter in
  Pred
    (Predicate.create
       Key.(Hardcoded Firstname)
       equal_operator
       (Single (Str firstname)))
;;

let find_contact_in_filtered_list contact experiment_id filter =
  let open Utils.Lwt_result.Infix in
  let find = Filter.find_filtered_contacts Data.database_label experiment_id in
  filter
  |> CCOption.pure
  |> find
  ||> get_exn
  ||> CCList.find_opt (Contact.equal contact)
  ||> CCOption.is_some
;;

let test_filter expected contact filter { Experiment.id; _ } =
  let%lwt res =
    find_contact_in_filtered_list
      contact
      (Filter.Matcher (id |> convert_id))
      filter
  in
  Alcotest.(check bool "succeeds" expected res) |> Lwt.return
;;

let save_filter current_user filter experiment =
  [ Filter.Created filter |> Pool_event.filter
  ; Experiment.Updated
      (experiment, { experiment with Experiment.filter = Some filter })
    |> Pool_event.experiment
  ]
  |> Pool_event.handle_events Data.database_label current_user
;;

let update_filter _ () =
  let open Test_utils in
  let query = firstname "Foo" in
  let filter = Filter.create None query in
  let experiment =
    { (Model.create_experiment ()) with Experiment.filter = Some filter }
  in
  let events =
    Cqrs_command.Experiment_command.UpdateFilter.handle
      experiment
      ([], [])
      filter
  in
  let expected =
    let updated_experiment =
      { experiment with
        Experiment.filter = Some filter
      ; matcher_notification_sent =
          Experiment.MatcherNotificationSent.create false
      }
    in
    Ok
      [ Experiment.updated experiment updated_experiment
        |> Pool_event.experiment
      ; Filter.Updated filter |> Pool_event.filter
      ; Email.BulkSent [] |> Pool_event.email
      ]
  in
  check_result expected events |> Lwt.return
;;

let create_and_update_filter_template _ () =
  let open Cqrs_command.Filter_command in
  let%lwt current_user = current_user () in
  let%lwt () =
    CustomFieldData.NrOfSiblings.save ()
    |> Pool_event.handle_event Data.database_label current_user
  in
  let%lwt key_list = Filter.all_keys Data.database_label in
  let title = "has siblings" |> Filter.Title.of_string in
  let id = Filter.Id.create () in
  let query = nr_of_siblings_filter () in
  let events = Create.handle ~id key_list [] query title in
  let filter = Filter.create ~id (Some title) query in
  let expected = Ok [ Filter.Created filter |> Pool_event.filter ] in
  check_result expected events;
  let events = Update.handle key_list [] filter query title in
  let expected =
    Ok
      [ Filter.(Updated filter) |> Pool_event.filter
      ; Assignment_job.Dispatched |> Pool_event.assignmentjob
      ]
  in
  check_result expected events |> Lwt.return
;;

let filter_contacts _ () =
  let%lwt current_user = current_user () in
  let%lwt () =
    let open Utils.Lwt_result.Infix in
    let%lwt contacts = TestContacts.all () in
    let%lwt experiment = Repo.first_experiment () in
    let%lwt () =
      let open CustomFieldData in
      (* Save field and answer with 3 *)
      NrOfSiblings.(save_answers ~answer_value:(Some answer_value) contacts)
      |> Pool_event.handle_events Data.database_label current_user
    in
    let filter = Filter.create None (nr_of_siblings_filter ()) in
    let%lwt () = save_filter current_user filter experiment in
    let expected = true in
    let%lwt filtered_contacts =
      Filter.(
        find_filtered_contacts
          Data.database_label
          (Matcher (experiment.Experiment.id |> convert_id))
          (Some filter))
      ||> get_exn
    in
    let res =
      filtered_contacts
      |> (CCList.subset ~eq:(fun filtered contact ->
            Contact.(Id.equal (id contact) (id filtered))))
           contacts
    in
    Alcotest.(check bool "succeeds" expected res) |> Lwt.return
  in
  Lwt.return_unit
;;

let filter_by_email _ () =
  let%lwt current_user = current_user () in
  let%lwt contact = TestContacts.get_contact 0 in
  let%lwt experiment = Repo.first_experiment () in
  let filter =
    Filter.(
      create
        None
        (And
           [ nr_of_siblings_filter ()
           ; firstname (Contact.firstname contact |> Pool_user.Firstname.value)
           ]))
  in
  let%lwt () = save_filter current_user filter experiment in
  test_filter true contact filter experiment
;;

let filter_exclude_inactive _ () =
  let%lwt current_user = current_user () in
  let%lwt contact = TestContacts.get_contact 9 in
  let%lwt experiment = Repo.first_experiment () in
  let filter =
    let open Filter in
    Pred
      (Predicate.create
         Key.(Hardcoded Name)
         equal_operator
         (Single (Str (Contact.lastname contact |> Pool_user.Lastname.value))))
    |> create None
  in
  let%lwt () = save_filter current_user filter experiment in
  (* Expect contact to be included *)
  let%lwt () = test_filter true contact filter experiment in
  let%lwt (_ : Pool_user.t) =
    let open Pool_user in
    update
      Test_utils.Data.database_label
      ~status:Status.Inactive
      contact.Contact.user
  in
  (* Expect inactive contact to be excluded *)
  test_filter false contact filter experiment
;;

let validate_filter_with_unknown_field _ () =
  let open Test_utils in
  let open CCResult in
  let%lwt key_list = Filter.all_keys Data.database_label in
  let query =
    let open Filter in
    Pred
      (Predicate.create
         Key.(CustomField ("Unknown field id" |> Custom_field.Id.of_string))
         equal_operator
         (Single (Nr 1.2)))
  in
  let filter = Filter.create None query in
  let title = Filter.Title.of_string "Title" in
  let events =
    Cqrs_command.Filter_command.Update.handle key_list [] filter query title
  in
  let expected = Error (Error.Invalid Field.Key) in
  check_result expected events |> Lwt.return
;;

let validate_filter_with_invalid_value _ () =
  let open Test_utils in
  let open CCResult in
  let%lwt key_list = Filter.all_keys Data.database_label in
  let query =
    let open Filter in
    Pred
      (Predicate.create
         Key.(
           CustomField (CustomFieldData.NrOfSiblings.field |> Custom_field.id))
         equal_operator
         (Single (Str "Not a number")))
  in
  let res = Filter.validate_query key_list [] query >|= Filter.create None in
  let expected = Error (Error.QueryNotCompatible (Field.Value, Field.Key)) in
  Alcotest.(check (result filter error) "succeeds" res expected) |> Lwt.return
;;

let test_list_filter answer_index operator contact experiment expected =
  let%lwt () =
    let%lwt current_user = current_user () in
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
    let%lwt () = save_filter current_user filter experiment in
    test_filter expected contact filter experiment
  in
  Lwt.return_unit
;;

let filter_by_list_contains_all _ () =
  let%lwt current_user = current_user () in
  let%lwt () =
    let%lwt contact = TestContacts.get_contact 0 in
    let answer_index = [ 0; 1 ] in
    let%lwt () =
      (* Save field and answer *)
      CustomFieldData.(
        create_multi_select ()
        @ answer_multi_select [ contact ] answer_index
        @ publish_fields ())
      |> Pool_event.handle_events Data.database_label current_user
    in
    let%lwt experiment = Repo.first_experiment () in
    test_list_filter
      answer_index
      Operator.(ListM.ContainsAll |> list)
      contact
      experiment
      true
  in
  Lwt.return_unit
;;

let filter_by_list_contains_none _ () =
  let%lwt () =
    let%lwt contact = TestContacts.get_contact 0 in
    let answer_index = [ 1; 2 ] in
    let%lwt experiment = Repo.first_experiment () in
    test_list_filter
      answer_index
      Operator.(ListM.ContainsNone |> list)
      contact
      experiment
      false
  in
  Lwt.return_unit
;;

let filter_by_list_contains_some _ () =
  let%lwt () =
    let%lwt contact = TestContacts.get_contact 0 in
    let answer_index = [ 1; 2 ] in
    let%lwt experiment = Repo.first_experiment () in
    test_list_filter
      answer_index
      Operator.(ListM.ContainsSome |> list)
      contact
      experiment
      true
  in
  Lwt.return_unit
;;

let filter_by_select_field _ () =
  let open CustomFieldData in
  let%lwt current_user = current_user () in
  let%lwt () = SelectField.init current_user in
  let%lwt contact = TestContacts.get_contact 0 in
  let%lwt experiment = Repo.first_experiment () in
  let%lwt () =
    SelectField.(
      save_answer (Some (option_to_public default_answer)) [ contact ])
    |> Pool_event.handle_events Data.database_label current_user
  in
  let test_select_filter operator expected =
    let filter = SelectField.(filter [ default_answer ] operator) () in
    let%lwt () = save_filter current_user filter experiment in
    test_filter expected contact filter experiment
  in
  let open Filter.Operator in
  let%lwt () = test_select_filter (List ListM.ContainsAll) true in
  let%lwt () = test_select_filter (List ListM.ContainsNone) false in
  Lwt.return_unit
;;

let retrieve_fitleterd_and_ordered_contacts _ () =
  let open CCFun in
  let open Test_utils in
  let open Utils.Lwt_result.Infix in
  let pool = Data.database_label in
  let%lwt current_user = current_user () in
  let%lwt () =
    let%lwt () =
      TestSeed.Contacts.(
        [ 11; 12 ]
        |> CCList.map create_contact
        |> fun contact_data -> create ~contact_data Data.database_label)
    in
    let find_contact = TestSeed.Contacts.find_contact_by_id pool in
    let%lwt contact_one = find_contact 11 in
    let%lwt contact_two = find_contact 12 in
    let%lwt id = Repo.first_experiment () ||> Experiment.(id %> Id.to_common) in
    let filter =
      let open Filter in
      Pred
        (Predicate.create
           Key.(Hardcoded ContactLanguage)
           equal_operator
           (Single (Language Pool_common.Language.En)))
      |> create None
    in
    let%lwt () =
      Contact.
        [ { contact_one with num_invitations = NumberOfInvitations.of_int 3 }
          |> update_num_invitations ~step:1
          |> updated
          |> Pool_event.contact
        ]
      |> Pool_event.handle_events Data.database_label current_user
    in
    let order_by =
      let open Mailing.Distribution in
      Sorted [ SortableField.InvitationCount, SortOrder.Ascending ]
      |> get_order_element
    in
    let%lwt contacts =
      Filter.(
        find_filtered_contacts
          ~order_by
          Data.database_label
          (Matcher id)
          (Some filter))
      |> Lwt.map get_exn
    in
    let get_index contact =
      CCList.find_idx (fun c -> Contact.(Id.equal (id c) (id contact))) contacts
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
  let open CCResult in
  let open Filter in
  let template_id = Pool_common.Id.create () in
  let template =
    Pred
      Predicate.
        { key = Key.(Hardcoded Name)
        ; operator = equal_operator
        ; value = Single (Str "Foo")
        }
    |> create ~id:template_id None
  in
  let filter = Template template_id in
  let events =
    let open Cqrs_command.Filter_command in
    Field.[ show Title, [ "Some title" ] ]
    |> default_decode
    >>= Create.handle [] [ template ] filter
  in
  let expected = Error Error.FilterMustNotContainTemplate in
  Alcotest.(check (result (list event) error) "succeeds" expected events)
  |> Lwt.return
;;

let filter_with_admin_value _ () =
  let%lwt () =
    let%lwt current_user = current_user () in
    let open Utils.Lwt_result.Infix in
    let%lwt id =
      Repo.first_experiment ()
      ||> fun { Experiment.id; _ } -> id |> Experiment.Id.to_common
    in
    let%lwt contact = TestContacts.get_contact 0 in
    let admin = Model.create_admin () in
    let%lwt () =
      CustomFieldData.(
        create_admin_override_nr_field ()
        :: (answer_admin_override_nr_field ~answer_value:3 [ contact ]
            @ answer_admin_override_nr_field ~answer_value:1 ~admin [ contact ]
           ))
      |> Pool_event.handle_events Data.database_label current_user
    in
    let search = find_contact_in_filtered_list contact (Filter.Matcher id) in
    let%lwt should_not_contain =
      Filter.create None (admin_override_nr_field_filter ~nr:3 ()) |> search
    in
    let%lwt should_contain =
      Filter.create None (admin_override_nr_field_filter ~nr:1 ()) |> search
    in
    let res = should_contain && not should_not_contain in
    Alcotest.(check bool "succeeds" true res) |> Lwt.return
  in
  Lwt.return_unit
;;

let no_admin_values_shown_to_contacts _ () =
  let%lwt () =
    let open Utils.Lwt_result.Infix in
    let%lwt contact = TestContacts.get_contact 0 in
    let open Custom_field in
    let%lwt custom_fields =
      find_all_by_contact
        Data.database_label
        (Pool_context.Contact contact)
        (Contact.id contact)
      ||> fun (grouped, ungrouped) ->
      ungrouped
      @ CCList.flat_map Group.Public.(fun group -> group.fields) grouped
    in
    let res =
      let open Custom_field.Answer in
      let open CCOption in
      custom_fields
      |> CCList.filter (function
        | Public.Boolean (_, answer) -> answer >>= admin_value |> is_some
        | Public.Date (_, answer) -> answer >>= admin_value |> is_some
        | Public.MultiSelect (_, _, answer) -> answer >>= admin_value |> is_some
        | Public.Number (_, answer) -> answer >>= admin_value |> is_some
        | Public.Select (_, _, answer) -> answer >>= admin_value |> is_some
        | Public.Text (_, answer) -> answer >>= admin_value |> is_some)
      |> CCList.is_empty
    in
    Alcotest.(check bool "succeeds" true res) |> Lwt.return
  in
  Lwt.return_unit
;;

let filter_ignore_admin_value _ () =
  let%lwt current_user = current_user () in
  let%lwt () =
    let open Utils.Lwt_result.Infix in
    let open CustomFieldData in
    let answer_value = 3 in
    let%lwt id =
      Repo.first_experiment ()
      ||> fun { Experiment.id; _ } -> id |> Experiment.Id.to_common
    in
    let%lwt contact = TestContacts.get_contact 0 in
    let%lwt () =
      let open Custom_field in
      let override_field =
        match[@warning "-4"] admin_override_nr_field with
        | Number field ->
          Number { field with admin_override = AdminOverride.create false }
        | _ -> failwith "Invalid field type"
      in
      (Updated override_field |> Pool_event.custom_field)
      :: answer_admin_override_nr_field ~answer_value [ contact ]
      |> Pool_event.handle_events Data.database_label current_user
    in
    let search = find_contact_in_filtered_list contact (Filter.Matcher id) in
    let%lwt res =
      Filter.create None (admin_override_nr_field_filter ~nr:answer_value ())
      |> search
    in
    Alcotest.(check bool "succeeds" true res) |> Lwt.return
  in
  Lwt.return_unit
;;

let filter_by_experiment_participation _ () =
  let open Assignment in
  let open Utils.Lwt_result.Infix in
  let hd = CCList.hd in
  let database_label = Data.database_label in
  let%lwt current_user = current_user () in
  let%lwt all_experiments = Repo.all_experiments () in
  let first_experiment =
    CCList.nth all_experiments 0 |> Experiment.(fun exp -> exp.id)
  in
  let second_experiment =
    CCList.nth all_experiments 2 |> Experiment.(fun exp -> exp.id)
  in
  let%lwt first_session =
    Session.find_all_for_experiment database_label first_experiment ||> hd
  in
  let%lwt second_session =
    Session.find_all_for_experiment database_label second_experiment ||> hd
  in
  let%lwt contact = TestContacts.get_contact 2 in
  let handle_events =
    Pool_event.handle_events Data.database_label current_user
  in
  let%lwt () =
    let%lwt () =
      [ Created (create contact, first_session.Session.id)
        |> Pool_event.assignment
      ]
      |> handle_events
    in
    let%lwt assignment =
      find_not_deleted_by_session database_label first_session.Session.id
      ||> CCList.find (fun (assignment : t) ->
        Contact.equal assignment.contact contact)
    in
    let assignment =
      { assignment with
        no_show = Some (NoShow.create false)
      ; participated = Some (Participated.create true)
      }
    in
    [ Updated assignment |> Pool_event.assignment
    ; Session.Closed first_session |> Pool_event.session
    ]
    |> handle_events
  in
  let id =
    CCList.nth all_experiments 1
    |> Experiment.(fun exp -> exp.id |> Id.to_common)
  in
  let search = find_contact_in_filtered_list contact (Filter.Matcher id) in
  let%lwt res =
    Filter.(
      create
        None
        (participation_filter
           [ first_experiment ]
           Operator.(ListM.ContainsNone |> list)
           ()))
    |> search
  in
  let () =
    Alcotest.(
      check bool "filtering 'ContainsNone' should not contain contact" false res)
  in
  let%lwt res =
    Filter.(
      create
        None
        (participation_filter
           [ first_experiment ]
           Operator.(ListM.ContainsAll |> list)
           ()))
    |> search
  in
  let () =
    Alcotest.(
      check bool "filtering 'ContainsAll' should contain contact" true res)
  in
  let%lwt () =
    let assignment = create contact in
    let assignment =
      { assignment with
        no_show = Some (NoShow.create true)
      ; participated = Some (Participated.create true)
      }
    in
    [ Created (assignment, second_session.Session.id) |> Pool_event.assignment
    ; Session.Closed second_session |> Pool_event.session
    ]
    |> handle_events
  in
  let%lwt res =
    Filter.(
      create
        None
        (participation_filter
           [ first_experiment; second_experiment ]
           Operator.(ListM.ContainsSome |> list)
           ()))
    |> search
  in
  let () =
    Alcotest.(
      check
        bool
        "filtering 'ContainsSome' with multiple experiments should contain \
         contact"
        true
        res)
  in
  Lwt.return_unit
;;

let filter_by_empty_hardcoded_value _ () =
  let%lwt current_user = current_user () in
  let%lwt contact =
    let open Contact in
    let%lwt contact =
      Integration_utils.ContactRepo.create ~with_terms_accepted:true ()
    in
    let user =
      Pool_user.{ contact.user with confirmed = Confirmed.create true }
    in
    { contact with language = None; user }
    |> TestContacts.persist_contact_update current_user
  in
  let%lwt experiment = Repo.first_experiment () in
  let filter operator =
    let open Filter in
    let query =
      Pred (Predicate.create Key.(Hardcoded ContactLanguage) operator NoValue)
    in
    create None query
  in
  let empty_filter = filter Operator.(Existence.Empty |> existence) in
  let%lwt () = test_filter true contact empty_filter experiment in
  let non_empty_filter = filter Operator.(Existence.NotEmpty |> existence) in
  test_filter false contact non_empty_filter experiment
;;

let filter_by_non_empty_hardcoded_value _ () =
  let%lwt contact =
    Integration_utils.ContactRepo.create
      ~with_terms_accepted:true
      ~language:Pool_common.Language.En
      ()
  in
  let%lwt experiment = Repo.first_experiment () in
  let filter operator =
    let open Filter in
    let query =
      Pred (Predicate.create Key.(Hardcoded ContactLanguage) operator NoValue)
    in
    create None query
  in
  let non_empty_filter = filter Operator.(Existence.NotEmpty |> existence) in
  let%lwt () = test_filter true contact non_empty_filter experiment in
  let empty_filter = filter Operator.(Existence.Empty |> existence) in
  test_filter false contact empty_filter experiment
;;

let filter_by_empty_custom_field _ () =
  let%lwt contact =
    Integration_utils.ContactRepo.create ~with_terms_accepted:true ()
  in
  let%lwt experiment = Repo.first_experiment () in
  let filter operator =
    let open Filter in
    let query =
      Pred
        (Predicate.create
           Key.(
             CustomField (CustomFieldData.NrOfSiblings.field |> Custom_field.id))
           operator
           NoValue)
    in
    create None query
  in
  let empty_filter = filter Operator.(Existence.Empty |> existence) in
  let%lwt () = test_filter true contact empty_filter experiment in
  let non_empty_filter = filter Operator.(Existence.NotEmpty |> existence) in
  test_filter false contact non_empty_filter experiment
;;

let filter_by_non_empty_custom_field _ () =
  let%lwt current_user = current_user () in
  let%lwt contact =
    Integration_utils.ContactRepo.create ~with_terms_accepted:true ()
  in
  let%lwt () =
    let open CustomFieldData in
    NrOfSiblings.(save_answers ~answer_value:(Some answer_value) [ contact ])
    |> Pool_event.handle_events Data.database_label current_user
  in
  let%lwt experiment = Repo.first_experiment () in
  let filter operator =
    let open Filter in
    let query =
      Pred
        (Predicate.create
           Key.(
             CustomField (CustomFieldData.NrOfSiblings.field |> Custom_field.id))
           operator
           NoValue)
    in
    create None query
  in
  let non_empty_filter = filter Operator.(Existence.NotEmpty |> existence) in
  let%lwt () = test_filter true contact non_empty_filter experiment in
  let empty_filter = filter Operator.(Existence.Empty |> existence) in
  test_filter false contact empty_filter experiment
;;

let filter_by_empty_custom_field_with_deleted_value _ () =
  let%lwt current_user = current_user () in
  let%lwt contact =
    Integration_utils.ContactRepo.create ~with_terms_accepted:true ()
  in
  let%lwt experiment = Repo.first_experiment () in
  let%lwt () =
    CustomFieldData.NrOfSiblings.(save_answers ~answer_value:None [ contact ])
    |> Pool_event.handle_events Data.database_label current_user
  in
  let filter operator =
    let open Filter in
    let query =
      Pred
        (Predicate.create
           Key.(
             CustomField (CustomFieldData.NrOfSiblings.field |> Custom_field.id))
           operator
           NoValue)
    in
    create None query
  in
  let empty_filter = filter Operator.(Existence.Empty |> existence) in
  let%lwt () = test_filter true contact empty_filter experiment in
  let non_empty_filter = filter Operator.(Existence.NotEmpty |> existence) in
  test_filter false contact non_empty_filter experiment
;;

let filter_by_date_custom_field _ () =
  let open CustomFieldData in
  let%lwt current_user = current_user () in
  let%lwt contact =
    Integration_utils.ContactRepo.create ~with_terms_accepted:true ()
  in
  let%lwt experiment = Repo.first_experiment () in
  let%lwt () =
    Birthday.(
      save () :: save_answers ~answer_value:(Some answer_value) [ contact ])
    |> Pool_event.handle_events Data.database_label current_user
  in
  let date = "1985-01-01" |> Pool_model.Base.Ptime.date_of_string |> get_exn in
  let greater_filter =
    Birthday.filter ~date Operator.(Size.Greater |> size) ()
  in
  let%lwt () = test_filter true contact greater_filter experiment in
  let less_filter = Birthday.filter ~date Operator.(Size.Less |> size) () in
  let%lwt () = test_filter false contact less_filter experiment in
  let equal_filter =
    Birthday.filter
      ~date:Birthday.answer_value
      Operator.(Equality.Equal |> equality)
      ()
  in
  test_filter true contact equal_filter experiment
;;

let filter_by_tags _ () =
  let open Utils.Lwt_result.Infix in
  let open Tags in
  let open Operator.ListM in
  let open Alcotest in
  let%lwt current_user = current_user () in
  let database_label = Data.database_label in
  let contact_testable = Contact.(testable pp equal) in
  let create_tag title =
    let id = Id.create () in
    let tag =
      create ~id (Title.of_string title) Tags.Model.Contact |> get_exn
    in
    let%lwt () = Created tag |> Tags.handle_event database_label in
    find database_label id ||> get_exn
  in
  let%lwt tag_one = create_tag "A Testing Tag" in
  let%lwt tag_two = create_tag "Another Testing Tag" in
  let%lwt tag_three = create_tag "Some Testing Tag" in
  let%lwt contact_one = TestContacts.get_contact 2 in
  let%lwt contact_two = TestContacts.get_contact 3 in
  let%lwt contact_three = TestContacts.get_contact 4 in
  let handle_events =
    Pool_event.handle_events Data.database_label current_user
  in
  let%lwt () =
    let create_tagged_event contact tag =
      let open Tagged in
      { model_uuid = Contact.(id contact |> Id.to_common)
      ; tag_uuid = tag.Tags.id
      }
      |> tagged
      |> Pool_event.tags
    in
    [ create_tagged_event contact_one tag_one
    ; create_tagged_event contact_two tag_two
    ; create_tagged_event contact_three tag_one
    ; create_tagged_event contact_three tag_two
    ]
    |> handle_events
  in
  let%lwt { Experiment.id; _ } = Integration_utils.ExperimentRepo.create () in
  let search filter =
    let find =
      Filter.Matcher (id |> Experiment.Id.to_common)
      |> Filter.find_filtered_contacts Data.database_label
    in
    find (Some filter)
    ||> get_exn
    ||> CCList.filter
          (CCFun.flip
             (CCList.mem ~eq:Contact.equal)
             [ contact_one; contact_two; contact_three ])
    ||> CCList.stable_sort Contact.compare
  in
  let create_filter ?(not = false) operator tags =
    let query =
      tag_filter
        (CCList.map (fun tag -> tag.Tags.id) tags)
        Operator.(operator |> list)
        ()
    in
    Filter.create None (if not then Filter.Not query else query)
  in
  let%lwt () =
    let%lwt res = create_filter ContainsNone [ tag_one ] |> search in
    let msg =
      "filtering 'ContainsNone' should not contain contact one or three"
    in
    check (list contact_testable) msg [ contact_two ] res |> Lwt.return
  in
  let%lwt () =
    let%lwt res = create_filter ContainsAll [ tag_one; tag_two ] |> search in
    let msg = "filtering 'ContainsAll' should contain contact three" in
    check (list contact_testable) msg [ contact_three ] res |> Lwt.return
  in
  let%lwt () =
    let%lwt res = create_filter ContainsAll [ tag_one ] |> search in
    let msg = "filtering 'ContainsAll' should contain contact one and three" in
    check
      (list contact_testable)
      msg
      ([ contact_one; contact_three ] |> CCList.stable_sort Contact.compare)
      res
    |> Lwt.return
  in
  let%lwt () =
    let%lwt res = create_filter ~not:true ContainsAll [ tag_one ] |> search in
    let msg = "filtering excluding 'ContainsAll' should contain contact two" in
    check (list contact_testable) msg [ contact_two ] res |> Lwt.return
  in
  let%lwt () =
    let%lwt res =
      create_filter ContainsSome [ tag_one; tag_two; tag_three ] |> search
    in
    let msg =
      "filtering 'ContainsSome' with multiple tags should contain all three \
       contacts"
    in
    check
      (list contact_testable)
      msg
      ([ contact_one; contact_two; contact_three ]
       |> CCList.stable_sort Contact.compare)
      res
    |> Lwt.return
  in
  let%lwt () =
    let%lwt res = create_filter ContainsSome [ tag_three ] |> search in
    let msg =
      "filtering 'ContainsSome' with unassigned tag should contain no one"
    in
    check (list contact_testable) msg [] res |> Lwt.return
  in
  Lwt.return_unit
;;
