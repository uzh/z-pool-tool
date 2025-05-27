val list
  :  Pool_context.t
  -> ?contact:Contact.t
  -> Duplicate_contacts.t list * Query.t
  -> [> Html_types.div ] Tyxml_html.elt

val index
  :  Pool_context.t
  -> ?contact:Contact.t
  -> Duplicate_contacts.t list * Query.t
  -> [> Html_types.div ] Tyxml_html.elt

val show
  :  Pool_context.t
  -> Custom_field.t list
  -> Contact.t * Custom_field.Public.t list
  -> Contact.t * Custom_field.Public.t list
  -> Duplicate_contacts.t
  -> [> Html_types.div ] Tyxml_html.elt
