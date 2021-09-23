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
          <hr />
          <div>
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
          <input name="title" type="text" placeholder="title"  value="Econ uzh" />
          <input name="description" type="text" placeholder="description" value="asdfasdf"/>
          <input name="url" type="text" placeholder="url" value="pool.econ.uzh.ch"/>
          <input name="database_url" type="text" placeholder="database_url" value="database@econ.ch"/>
          <input name="smtp_auth_server" type="text" placeholder="smtp_auth_server" value="smtp.uzh.ch" />
          <input name="smtp_auth_port" type="text" placeholder="smtp_auth_port" value="587" />
          <input name="smtp_auth_username" placeholder="smtp_auth_username" value="engineering@econ.uzh.ch" />
          <input name="smtp_auth_authentication_method" placeholder="smtp_auth_authentication_method" value="LOGIN" />
          <input name="smtp_auth_protocol" placeholder="smtp_auth_protocol" value="SSL/TLS" />
          <input name="styles" placeholder="styles" value="custom.css" />
          <input name="icon" placeholder="icon" value="icon" />
          <input name="logos" placeholder="logos" value="logos" />
          <input name="partner_logos" placeholder="partner_logo" value="partner logo" />
          <input name="default_language" placeholder="default language" value="EN" />
          <input type="submit" value="Create new" />
          </form>
          |}]
  in
  Page_layout.create ~children:html ~message ()
;;
