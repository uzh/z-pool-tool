open Entity

type event =
  | AdminAnswerCleared of Public.t * Pool_common.Id.t
  | AnswerOverridden of Public.t * Contact.t
  | AnswerUpserted of Public.t * Contact.Id.t * Pool_context.user
  | AnsweredOnSignup of Public.t * Pool_common.Id.t
  | Created of t
  | Deleted of t
  | FieldsSorted of t list
  | GroupCreated of Group.t
  | GroupDestroyed of Group.t
  | GroupsSorted of Group.t list
  | GroupUpdated of (Group.t * Group.t)
  | OptionCreated of (Id.t * SelectOption.t)
  | OptionDestroyed of SelectOption.t
  | OptionPublished of SelectOption.t
  | OptionsSorted of SelectOption.t list
  | OptionUpdated of (SelectOption.t * SelectOption.t)
  | PartialUpdate of PartialUpdate.t * Contact.t * Pool_context.user
  | Published of t
  | Updated of t * t
[@@deriving eq, show, variants]

let handle_event ?user_uuid pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let create_changelog before after =
    let open Version_history in
    insert pool ?user_uuid ~entity_uuid:(id before) ~before ~after ()
  in
  let create_option_changelog before after =
    let open Version_history.OptionVersionHistory in
    insert pool ?user_uuid ~entity_uuid:before.SelectOption.id ~before ~after ()
  in
  let create_group_changelog before after =
    let open Version_history.GroupVersionHistory in
    insert pool ?user_uuid ~entity_uuid:before.Group.id ~before ~after ()
  in
  let create_custom_field_answer_changelog contact public =
    let open Version_history in
    let contact_id = Contact.id contact in
    let field_id = Public.id public in
    let after = AnswerRecord.from_public public in
    let%lwt before =
      Repo_version_history.find_answer_opt pool contact_id field_id
      ||> CCOption.value ~default:(AnswerRecord.default_record after)
    in
    AnswerVersionHistory.insert
      pool
      ?user_uuid
      ~entity_uuid:(Contact.Id.to_common contact_id)
      ~before
      ~after
      ()
  in
  let create_contact_changelog before after =
    let open Contact in
    let contact_id = id before in
    VersionHistory.insert
      pool
      ?user_uuid
      ~entity_uuid:(Id.to_common contact_id)
      ~before
      ~after
      ()
  in
  function
  | AdminAnswerCleared (m, entity_uuid) ->
    Repo_partial_update.clear_answer
      pool
      ~is_admin:true
      ~field_id:(Public.id m)
      ~entity_uuid
      ()
  | AnswerOverridden (m, contact) ->
    let%lwt () = create_custom_field_answer_changelog contact m in
    let entity_uuid = Contact.(id contact |> Id.to_common) in
    Repo_partial_update.override_answer pool entity_uuid m
  | AnswerUpserted (m, entity_uuid, user) ->
    let is_admin = Pool_context.user_is_admin user in
    Repo_partial_update.upsert_answer
      pool
      is_admin
      (Contact.Id.to_common entity_uuid)
      m
  | AnsweredOnSignup (m, entity_uuid) ->
    Repo_partial_update.upsert_answer pool false entity_uuid m
  | Created m ->
    let%lwt () = Repo.insert pool m in
    Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) m
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Deleted m -> Repo.delete pool m
  | FieldsSorted m -> CCList.map (fun m -> id m) m |> Repo.sort_fields pool
  | GroupCreated m ->
    let%lwt () = Repo_group.insert pool m in
    Entity_guard.Group.Target.to_authorizable ~ctx:(Database.to_ctx pool) m
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | GroupDestroyed m -> Repo_group.destroy pool m
  | GroupsSorted m ->
    CCList.map (fun m -> m.Group.id) m |> Repo_group.sort_groups pool
  | GroupUpdated (m, updated) ->
    let%lwt () = create_group_changelog m updated in
    Repo_group.update pool updated
  | OptionCreated (field_id, m) -> Repo_option.insert pool field_id m
  | OptionDestroyed m -> Repo_option.destroy pool m
  | OptionPublished m -> Repo_option.publish pool m
  | OptionsSorted m ->
    CCList.map (fun m -> m.SelectOption.id) m |> Repo_option.sort_options pool
  | OptionUpdated (m, updated) ->
    let%lwt () = create_option_changelog m updated in
    Repo_option.update pool updated
  | PartialUpdate (update, contact, current_user) ->
    let open Contact in
    let open PartialUpdate in
    let%lwt () =
      match update with
      | Custom public -> create_custom_field_answer_changelog contact public
      | Firstname (_, firstname) ->
        create_contact_changelog contact (set_firstname contact firstname)
      | Lastname (_, lastname) ->
        create_contact_changelog contact (set_lastname contact lastname)
      | Language (_, language) ->
        create_contact_changelog contact (set_language contact language)
    in
    Repo_partial_update.update pool current_user update contact
  | Published m ->
    let%lwt () = create_changelog m (set_published_at m) in
    Repo.publish pool m
  | Updated (t, updated) ->
    let%lwt () = create_changelog t updated in
    Repo.update pool updated
;;
