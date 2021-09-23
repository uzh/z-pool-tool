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
          <input name="smtp_auth_username" type="text" placeholder="smtp_auth_username" value="engineering@econ.uzh.ch" />
          <input name="smtp_auth_authentication_method" type="text" placeholder="smtp_auth_authentication_method" value="LOGIN" />
          <input name="smtp_auth_protocol" type="text" placeholder="smtp_auth_protocol" value="SSL/TLS" />
          <input name="styles" type="text" placeholder="styles" value="custom.css" />
          <input name="icon" type="text" placeholder="icon" value="icon" />
          <input name="logos" type="text" placeholder="logos" value="logos" />
          <input name="partner_logos" type="text" placeholder="partner_logo" value="partner logo" />
          <input name="default_language" type="text" placeholder="default language" value="EN" />

          <br />
          <h3>Operator</h3>
          <input name="email" type="text" placeholder="operator_email_address" value="operator@mail.com" />
          <input name="password" type="text" placeholder="operator_password" value="adminadmin" />
          <input name="firstname" type="text" placeholder="operator_firstname" value="timo" />
          <input name="lastname" type="text" placeholder="operator_lastname" value="huber" />

          <input type="submit" value="Create new" />
          </form>
          |}]
  in
  Page_layout.create ~children:html ~message ()
;;
