open Tyxml.Html

let create ?(active = false) ?subtitle language title id html =
  let title =
    div
      ~a:[ a_class [ "modal-heading" ] ]
      [ txt (title language)
      ; span ~a:[ a_class [ "modal-close" ] ] Component_icon.[ to_html Close ]
      ]
  in
  let subtitle =
    subtitle
    |> CCOption.map_or ~default:(txt "") (fun sub ->
      sub language |> txt |> CCList.pure |> p ~a:[ a_class [ "gap-xs" ] ])
  in
  let attrs =
    let base_classnames = [ "fullscreen-overlay"; "modal" ] in
    let dataset = a_user_data "modal" id in
    match active with
    | true ->
      dataset
      :: [ a_class ("active" :: base_classnames); a_aria "hidden" [ "false" ] ]
    | false ->
      dataset :: [ a_class base_classnames; a_aria "hidden" [ "true" ] ]
  in
  div
    ~a:(a_id id :: attrs)
    [ div
        ~a:[ a_class [ "modal-body" ] ]
        [ div ~a:[ a_class [ "modal-header" ] ] [ title; subtitle ]
        ; div ~a:[ a_class [ "modal-content" ] ] [ html ]
        ]
    ]
;;

let js_modal_add_spinner =
  Format.asprintf
    {js|
      document.addEventListener("htmx:beforeSend", (e) => {
        const { target } = e.detail;
        if(target && target.id === "%s" ) {
          const content = document.createElement("div");
          const body = document.createElement("div");
          body.classList.add("modal-body");
          content.classList.add("modal-content", "flexrow", "justify-center")
          const icon = document.createElement("i");
          icon.classList.add("icon-spinner-outline", "rotate");
          content.appendChild(icon);
          body.appendChild(content);
          target.innerHTML = '';
          target.appendChild(body);
          target.classList.add("active");
        }
      })
    |js}
;;

let js_add_modal_close_listener =
  {js|
    document.addEventListener("htmx:afterSwap", (e) => {
      const modal = e.detail.elt;
      if(modal.classList.contains("modal")) {
        modal.querySelector(".modal-close").addEventListener("click", (e) => {
          modal.classList.remove("active");
          modal.setAttribute("aria-hidden", "true");
        })
      }
    })
  |js}
;;
