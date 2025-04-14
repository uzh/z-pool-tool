open Entity

type event =
  | Created of t
  | Updated of t * t
[@@deriving eq, show]

let[@warning "-27"] handle_event ?user_uuid pool
  =
  (* let create_changelog before after =
     let open Version_history in
     insert pool ?user_uuid ~entity_uuid:before.id ~before ~after ()
     in *)
  function
  | Created t -> Repo.insert pool t
  | Updated (_, after) -> Repo.update pool after
;;
