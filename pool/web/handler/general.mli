val src : Logs.src

val user_from_session
  :  ?cookie_key:string
  -> ?secret:string
  -> ?key:string
  -> Database.Label.t
  -> Rock.Request.t
  -> Pool_user.t option Lwt.t

val admin_from_session
  :  Database.Label.t
  -> Rock.Request.t
  -> (Admin.t, Pool_message.Error.t) Lwt_result.t

val create_tenant_layout
  :  Rock.Request.t
  -> ?active_navigation:CCString.t
  -> Pool_context.t
  -> [< Html_types.div_content_fun > `Div `PCDATA ] Tyxml_html.elt
  -> ([> Html_types.html ] Tyxml_html.elt, Pool_message.Error.t) Lwt_result.t

val create_root_layout
  :  ?active_navigation:CCString.t
  -> Pool_context.t
  -> [< Html_types.div_content_fun > `Div `PCDATA ] Tyxml_html.elt
  -> [> Html_types.html ] Tyxml_html.elt Lwt.t

val note
  :  title:Pool_common.I18n.t
  -> body:Pool_common.I18n.t
  -> Rock.Request.t
  -> Rock.Response.t Lwt.t
