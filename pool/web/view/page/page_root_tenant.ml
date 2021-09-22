open Tyxml

let of_string value = Html.txt value

let build_tenant_rows tenant_list =
  let open Tenant in
  CCList.map
    (fun (tenant : Tenant.t) ->
      [%html
        {|
          <div>
          <h2>|}
          [ of_string (Title.value tenant.title) ]
          {|</h2>
          <div>
          |}
          [ of_string
              (Format.asprintf
                 "Language: %s"
                 (Settings.Language.code tenant.default_language))
          ]
          {|
          </div>
          </div>
          |}])
    tenant_list
;;

let list csrf ~tenant_list ~message () =
  let tenant_list = build_tenant_rows tenant_list in
  let html =
    [%html
      {|
          <h1>Tenants</h1>
          |}
        tenant_list
        {|
          <form method="Post" action=|}
        (Sihl.Web.externalize_path "/root/tenant/create")
        {|
        >
        |}
        [ Component.csrf_element csrf ]
        {|
          <input name="title" type="text" placeholder="title"/>
          <input type="submit" value="Create new" />
          </form>
          |}]
  in
  Page_layout.create ~children:html ~message ()
;;
