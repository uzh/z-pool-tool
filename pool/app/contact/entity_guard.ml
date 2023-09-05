open CCFun.Infix
open Utils.Lwt_result.Infix
open Guard

module Target = struct
  type t = Entity.t [@@deriving eq, show]

  let to_authorizable ?ctx t =
    Persistence.Target.decorate
      ?ctx
      (fun user ->
        Target.create
          `Contact
          (user |> Entity.id |> Uuid.target_of Pool_common.Id.value))
      t
    >|- Format.asprintf "Failed to convert Contact to authorizable: %s"
    >|- Pool_common.Message.authorization
  ;;
end

module Actor = struct
  type t = Entity.t [@@deriving eq, show]

  let decorate ?ctx encode id =
    Persistence.Actor.decorate ?ctx (encode %> Actor.create `Contact) id
    >|- Format.asprintf "Failed to convert Contact to authorizable: %s"
    >|- Pool_common.Message.authorization
  ;;

  let to_authorizable ?ctx =
    let encode = Entity.id %> Uuid.actor_of Pool_common.Id.value in
    decorate ?ctx encode
  ;;
end

module Access = struct
  open Guard
  open ValidationSet
  open Permission

  let contact action uuid =
    one_of_tuple
      (action, `Contact, Some (uuid |> Uuid.target_of Pool_common.Id.value))
  ;;

  let index_permission = Read
  let index = one_of_tuple (index_permission, `Contact, None)
  let create = one_of_tuple (Create, `Contact, None)
  let read = contact Read
  let update = contact Update

  let read_additionals model ?(verify_on_ids = []) () =
    let open PermissionOnTarget in
    let create ?target_uuid = create ?target_uuid Permission.Read in
    CCList.flat_map
      (fun target_uuid ->
        [ create ~target_uuid `Contact; create ~target_uuid model ])
      verify_on_ids
    @ [ create `Contact; create model ]
  ;;

  let read_name = read_additionals `ContactName
  let read_info = read_additionals `ContactInfo

  let read_of_target target_uuid =
    let open PermissionOnTarget in
    [ create index_permission `Contact
    ; create ~target_uuid index_permission `Contact
    ]
  ;;
end
