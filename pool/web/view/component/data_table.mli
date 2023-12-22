(** {1 A Sortable Table}

    This table takes in a series of [cols] and [rows] and will wrap the column
    headers with the according sorting behavior.

    By default, it uses a HTMX Get request that swaps out the entire table.

    The Get request will be sent to [sort.url] with the right search query
    string, using [sort.query] as a default and updating the
    [sort.query.sort.order] and [sort.query.sort.column] accordingly.

    The table headers will be presented in the same order as given, and if you
    need to pass in custom headers (such as a button group) you can use the
    [`custom] tag instead.

    It is expected that [id] will be unique and only correspond to the current
    table. *)

(** The sort configuration of the table. *)
type data_table =
  { url : Uri.t (** the URL to which to make the sort requests *)
  ; query : Query.t (** the current URL query string *)
  ; language : Pool_common.Language.t
  ; search : Query.Column.t list option (** the columns that can be searched for  *)
  (** the language in which to show the table *)
  }

(** A column in the table. Use [`column] for actual database columns, and use [`custom] for arbitrary elements. *)
type col =
  [ `column of Query.Column.t
  | `custom of [ | Html_types.flow5 ] Tyxml_html.elt
  | `field of Pool_common.Message.Field.t * Query.Column.t
  | `empty
  ]

val make
  : ?align_last_end:bool
  -> ?align_top:bool
  ->  ?layout:[ `Striped | `Simple ]
  -> target_id:string
  -> cols:col list
  -> row:('a -> [< Html_types.table_content_fun ] Tyxml_html.elt)
  -> data_table
  -> 'a list
  -> [> Html_types.div ] Tyxml_html.elt
