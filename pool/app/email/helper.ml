open Entity
module User = Pool_user
module Database = Pool_database

let string_to_html =
  let open CCFun in
  CCString.split ~by:"\n\n"
  %> CCList.map (Utils.Html.handle_line_breaks Tyxml.Html.p)
;;

let create_public_url pool_url path =
  path
  |> Sihl.Web.externalize_path
  |> Format.asprintf "http://%s%s" (Pool_tenant.Url.value pool_url)
;;

let prepend_root_directory pool url =
  match Database.Label.equal pool Database.root with
  | true -> Format.asprintf "/root%s" url
  | false -> url
;;

let layout_from_tenant (tenant : Pool_tenant.t) =
  let open Pool_tenant in
  let logo_src =
    tenant.logos
    |> Logos.value
    |> CCList.head_opt
    |> CCOption.map_or
         ~default:""
         CCFun.(Pool_common.File.path %> create_public_url tenant.url)
  in
  let logo_alt = tenant.title |> Title.value |> Format.asprintf "Logo %s" in
  let link = tenant.url |> Url.value |> Format.asprintf "http://%s" in
  { link; logo_src; logo_alt }
;;

let root_layout () =
  let open CCOption in
  let root_url =
    Sihl.Configuration.read_string "PUBLIC_URL"
    >>= CCFun.(Pool_tenant.Url.create %> CCOption.of_result)
  in
  let logo_src =
    root_url
    >|= (fun url -> create_public_url url "assets/images/root_logo.svg")
    |> value ~default:""
  in
  let logo_alt = "Logo Pool Tool" in
  let link = root_url >|= Pool_tenant.Url.value |> value ~default:"" in
  { link; logo_alt; logo_src }
;;

let layout_params layout =
  [ "logoSrc", layout.logo_src
  ; "logoAlt", layout.logo_alt
  ; "logoHref", layout.link
  ]
;;

let prepare_email pool language label subject email layout params =
  let%lwt template =
    Service.EmailTemplate.get_by_label
      ~ctx:(Pool_tenant.to_ctx pool)
      ~language:(Pool_common.Language.show language)
      (TemplateLabel.show label)
  in
  match template, Sihl.Configuration.read_string "SMTP_SENDER" with
  | _, None -> failwith "SMTP_SENDER not found in configuration"
  | None, _ -> failwith "Email template not found!"
  | Some template, Some sender ->
    let mail =
      Sihl_email.
        { sender
        ; recipient = email
        ; subject
        ; text = ""
        ; html = None
        ; cc = []
        ; bcc = []
        }
    in
    let params = params @ layout_params layout in
    Sihl_email.Template.email_of_template ~template mail params
;;

let prepare_boilerplate_email template email params =
  match Sihl.Configuration.read_string "SMTP_SENDER" with
  | None -> failwith "SMTP_SENDER not found in configuration"
  | Some sender ->
    let CustomTemplate.{ subject; content; layout } = template in
    let subject = subject |> CustomTemplate.Subject.value in
    let text = content |> CustomTemplate.Content.value in
    let html =
      Default_utils.(
        combine_html
          Pool_common.Language.En
          (Some subject)
          (text |> string_to_html)
        |> html_to_string)
    in
    let params = params @ layout_params layout in
    let mail =
      Sihl_email.
        { sender
        ; recipient = email
        ; subject
        ; text
        ; html = Some html
        ; cc = []
        ; bcc = []
        }
    in
    Sihl_email.Template.render_email_with_data params mail
;;
