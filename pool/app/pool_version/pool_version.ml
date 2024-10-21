include Entity
include Event
include Entity_guard
include Repo

let announcement ?id (version : t) =
  let open CCResult in
  let open Announcement in
  let text =
    let open Tyxml.Html in
    let url =
      Format.asprintf
        "/admin/%s/%s"
        Pool_message.Field.(human_url Version)
        (Entity.Id.value version.id)
      |> Sihl.Web.externalize_path
    in
    p
      [ txt
          (Format.asprintf
             "A new version has been released: %s"
             (Tag.value version.tag))
      ; br ()
      ; a ~a:[ a_href url ] [ txt "Read more" ]
      ]
    |> Format.asprintf "%a" (Tyxml.Html.pp_elt ~indent:true ())
  in
  let start_at = StartAt.create_now () |> CCOption.return in
  let* text = [ Pool_common.Language.En, text ] |> Text.create in
  let show_to_admins = ShowToAdmins.create true in
  let show_to_contacts = ShowToContacts.create false in
  create ?id text start_at None show_to_admins show_to_contacts |> return
;;
