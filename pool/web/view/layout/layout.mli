module Navigation : sig
  type validation =
    | AlwaysOn
    | OnChildren
    | Set of Guard.ValidationSet.t

  module NavElement : sig
    type t =
      { url : string option
      ; label : Pool_common.I18n.nav_link
      ; icon : Component.Icon.t option
      ; validation : validation
      ; children : t list
      }

    val equal : t -> t -> bool
    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val logout : ?prefix:string -> unit -> t
  end

  module NavUtils : sig
    val filter_items
      :  ?validate:bool
      -> ?actor:Guard.Actor.t
      -> ?guardian:Guard.PermissionOnTarget.t list
      -> NavElement.t list
      -> NavElement.t list
  end

  module NavElements : sig
    module Profile : sig
      val nav : ?contact:bool -> ?prefix:string -> unit -> NavElement.t
    end

    module AdminTenantItems : sig
      val dashboard : NavElement.t
      val settings : NavElement.t
      val user : NavElement.t
      val experiments : NavElement.t
      val all : NavElement.t list
    end
  end
end

module CustomField : sig
  val create
    :  ?buttons:[< Html_types.div_content_fun ] Tyxml_html.elt
    -> ?hint:Pool_common.I18n.hint
    -> Pool_context.t
    -> Custom_field.Model.t
    -> [< Html_types.div_content_fun ] Tyxml_html.elt list
    -> [> Html_types.div ] Tyxml_html.elt Lwt.t
end

module Error : sig
  val create
    :  [< Html_types.div_content_fun ] Tyxml_html.elt
    -> [> Html_types.html ] Tyxml_html.elt
end

module Experiment : sig
  type title =
    | Control of Pool_message.Control.t
    | NavLink of Pool_common.I18n.nav_link
    | I18n of Pool_common.I18n.t
    | Text of string

  val create
    :  ?active_navigation:string
    -> ?buttons:[< Html_types.div_content_fun ] Tyxml_html.elt
    -> ?hint:Pool_common.I18n.hint
    -> Pool_context.t
    -> title
    -> Experiment.t
    -> [< Html_types.div_content_fun ] Tyxml_html.elt list
    -> [> Html_types.div ] Tyxml_html.elt Lwt.t
end

module Print : sig
  val create
    :  document_title:string
    -> [< Html_types.flow5 ] Tyxml_html.elt list
    -> [> Html_types.html ] Tyxml_html.elt
end

module Root : sig
  val create
    :  ?active_navigation:string
    -> Pool_context.t
    -> [< Html_types.div_content_fun > `Div `PCDATA ] Tyxml_html.elt
    -> [> Html_types.html ] Tyxml_html.elt Lwt.t
end

module Tenant : sig
  val create
    :  ?active_navigation:string
    -> Pool_context.t
    -> Pool_context.Tenant.t
    -> [< Html_types.div_content_fun ] Tyxml_html.elt
    -> [> Html_types.html ] Tyxml_html.elt Lwt.t
end
