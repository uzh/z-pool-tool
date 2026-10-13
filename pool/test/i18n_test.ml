module I18nCommand = Cqrs_command.I18n_command
module SettingsCommand = Cqrs_command.Settings_command
module Common = Pool_common

let database_label = Test_utils.Data.database_label

let update_terms_and_conditions () =
  let languages = Pool_common.Language.[ En; De ] in
  let content = "Terms and Conditions" in
  let terms =
    CCList.map
      (fun lang ->
         let system_event_id = System_event.Id.create () in
         ( I18n.create
             I18n.Key.TermsAndConditions
             lang
             (Some (I18n.Content.of_string content))
         , system_event_id ))
      languages
  in
  let events =
    let open CCResult in
    let open I18nCommand.Update in
    terms
    |> CCList.map (fun (i18n, system_event_id) ->
      [ Pool_message.Field.(show Translation), [ content ] ]
      |> decode
      >>= handle ~system_event_id i18n)
    |> CCResult.flatten_l
    |> CCResult.map CCList.flatten
  in
  let expected =
    terms
    |> CCList.map (fun (i18n, system_event_id) ->
      let content =
        I18n.Content.create_opt content |> Pool_message.Error.get_or_failwith
      in
      [ I18n.Updated (i18n, content) |> Pool_event.i18n
      ; System_event.(
          Job.I18nPageUpdated
          |> create ~id:system_event_id
          |> created
          |> Pool_event.system_event)
      ])
    |> CCList.flatten
    |> CCResult.return
  in
  Alcotest.(
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
;;

let update_terms_and_conditions_accepts_empty_content () =
  let system_event_id = System_event.Id.create () in
  let i18n =
    I18n.create
      I18n.Key.TermsAndConditions
      Pool_common.Language.En
      (Some (I18n.Content.of_string "Terms and Conditions"))
  in
  let events =
    let open CCResult in
    let open I18nCommand.Update in
    [ Pool_message.Field.(show Translation), [ "" ] ]
    |> decode
    >>= handle ~system_event_id i18n
  in
  let expected =
    Ok
      [ I18n.Updated (i18n, None) |> Pool_event.i18n
      ; System_event.(
          Job.I18nPageUpdated
          |> create ~id:system_event_id
          |> created
          |> Pool_event.system_event)
      ]
  in
  Alcotest.(
    check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
;;

let extract_by_key_exn_filters_by_language () =
  let key = I18n.Key.TermsAndConditions in
  let de =
    I18n.create key Pool_common.Language.De (Some (I18n.Content.of_string "Bedingungen"))
  in
  let en =
    I18n.create key Pool_common.Language.En (Some (I18n.Content.of_string "Terms"))
  in
  let extracted = I18n.extract_by_key_exn [ de; en ] key Pool_common.Language.En in
  Alcotest.(
    check bool "extracts entry for requested language" true (I18n.equal extracted en))
;;

let sanitize_rich_text () =
  let check msg expected input =
    Alcotest.(check string msg expected (Utils.Html.sanitize input))
  in
  check
    "keeps allowed markup"
    {|<h2>Title</h2><p><strong>bold</strong> <a href="https://example.com">link</a></p>|}
    {|<h2>Title</h2><p><strong>bold</strong> <a href="https://example.com">link</a></p>|};
  check
    "keeps relative links"
    {|<a href="/terms#top">x</a>|}
    {|<a href="/terms#top">x</a>|};
  check "escapes text" "a &lt; b &amp;" "a < b &";
  check "removes script" "<p>hi</p>" "<p>hi<script>alert(1)</script></p>";
  check "removes uppercase script" "" "<SCRIPT>alert(1)</SCRIPT>";
  check "removes style" "" "<style>body{display:none}</style>";
  check "removes iframe" "" {|<iframe src="https://evil.example"></iframe>|};
  check "removes svg" "" {|<svg onload="alert(1)"><script>alert(1)</script></svg>|};
  check "removes math" "" {|<math><mtext><img src=x onerror=alert(1)></mtext></math>|};
  check "removes comments" "<p>a</p>" "<p>a<!-- <script>alert(1)</script> --></p>";
  check "removes event attributes" "<img>" {|<img src=x:y onerror="alert(1)">|};
  check "removes onclick" "<p>x</p>" {|<p onclick="alert(1)">x</p>|};
  check "removes style attribute" "<p>x</p>" {|<p style="background:url(x)">x</p>|};
  check "unwraps unknown elements" "<p>text</p>" "<p><blink>text</blink></p>";
  check "removes javascript urls" "<a>x</a>" {|<a href="javascript:alert(1)">x</a>|};
  check
    "removes obfuscated javascript urls"
    "<a>x</a><a>y</a><a>z</a>"
    ({|<a href=" JaVaScRiPt:alert(1)">x</a>|}
     ^ {|<a href="java&#x09;script:alert(1)">y</a>|}
     ^ {|<a href="javascript&colon;alert(1)">z</a>|});
  check "removes vbscript urls" "<a>x</a>" {|<a href="vbscript:msgbox(1)">x</a>|};
  check
    "removes data urls on links"
    "<a>x</a>"
    {|<a href="data:text/html;base64,PHNjcmlwdD4=">x</a>|};
  check
    "removes non image data urls on images"
    "<img>"
    {|<img src="data:image/svg+xml;base64,PHN2Zz4=">|};
  check
    "keeps raster data urls on images"
    {|<img src="data:image/png;base64,AAAA" alt="a">|}
    {|<img src="data:image/png;base64,AAAA" alt="a">|};
  check
    "keeps mailto links"
    {|<a href="mailto:a@b.ch">x</a>|}
    {|<a href="mailto:a@b.ch">x</a>|};
  check
    "adds rel to links with target"
    {|<a href="https://a.ch" target="_blank" rel="noopener noreferrer">x</a>|}
    {|<a href="https://a.ch" target="_blank" rel="opener">x</a>|};
  check "removes unknown targets" {|<a>x</a>|} {|<a target="evil">x</a>|};
  check
    "keeps otp div"
    {|<div class="otp apple-otp">123</div>|}
    {|<div class="otp apple-otp">123</div>|}
;;

(* Only the I18n event is compared, the system event carries a creation timestamp *)
let handle_update i18n input =
  let open CCResult in
  let open I18nCommand.Update in
  [ Pool_message.Field.(show Translation), [ input ] ]
  |> decode
  >>= handle i18n
  |> map (CCList.take 1)
;;

let update_rich_text_sanitizes_content () =
  let i18n = I18n.create I18n.Key.WelcomeText Pool_common.Language.En None in
  let check msg content input =
    Alcotest.(
      check
        (result (list Test_utils.event) Test_utils.error)
        msg
        (Ok [ I18n.Updated (i18n, content) |> Pool_event.i18n ])
        (handle_update i18n input))
  in
  check
    "strips script and event handler"
    (Some (I18n.Content.of_string {|<p>Welcome</p><img src="x">|}))
    {|<p>Welcome<script>alert(1)</script></p><img src=x onerror="alert(1)">|};
  check "drops content that is empty after sanitizing" None "<script>alert(1)</script>"
;;

let update_plain_text_keeps_content () =
  let i18n = I18n.create I18n.Key.UnsubscribeTitle Pool_common.Language.En None in
  let content = "Tom & Jerry <3" in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "unchanged"
      (Ok
         [ I18n.Updated (i18n, Some (I18n.Content.of_string content)) |> Pool_event.i18n ])
      (handle_update i18n content))
;;

let content_to_html_sanitizes_stored_content () =
  let i18n =
    I18n.create
      I18n.Key.TermsAndConditions
      Pool_common.Language.En
      (Some
         (I18n.Content.of_string
            {|<p onmouseover="alert(1)">Terms</p><script>x</script>|}))
  in
  Alcotest.(check string "sanitized" "<p>Terms</p>" (I18n.content_to_html i18n))
;;
