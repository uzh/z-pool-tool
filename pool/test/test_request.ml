let mock_context
      ?(database_label = Test_utils.Data.database_label)
      ?(user = Pool_context.Guest)
      ?(language = Pool_common.Language.En)
      ()
  =
  let open Pool_context in
  { query_parameters = []
  ; language
  ; database_label
  ; message = None
  ; csrf = "csrf"
  ; user
  ; guardian = []
  ; notifications = []
  ; flash_fetcher = None
  }
;;

let mock_request
      ?(headers = [])
      ?(meth = `GET)
      ?(body = Rock.Body.empty)
      ?(context = mock_context ())
      ?context_tenant
      ()
  =
  let open Rock.Request in
  let headers = Httpaf.Headers.of_list headers in
  let req =
    { version = Httpaf.Version.of_string "HTTP/1.1"
    ; target = "http://localhost:8080/login"
    ; headers
    ; meth
    ; body
    ; env = Rock.Context.empty
    }
  in
  let req = Pool_context.set req context in
  match context_tenant with
  | None -> req
  | Some tenant ->
    let open Pool_context.Tenant in
    { tenant; tenant_languages = Pool_common.Language.all } |> set req
;;

let data_to_urlencoded data =
  let open CCList in
  data >|= CCPair.map_snd return
;;

let mock_post_request ?context_tenant form =
  let url_encode_form data =
    data
    |> List.map (fun (k, v) ->
      let v = CCList.head_opt v |> CCOption.get_or ~default:"" in
      Printf.sprintf "%s=%s" (Uri.pct_encode k) (Uri.pct_encode v))
    |> String.concat "&"
  in
  let headers = [ "Content-Type", "application/x-www-form-urlencoded" ] in
  let body = form |> url_encode_form |> Rock.Body.of_string in
  mock_request ~headers ~body ~meth:`POST ?context_tenant ()
;;
