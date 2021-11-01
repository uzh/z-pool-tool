open Entity

type event =
  | LanguagesUpdated of TenantLanguages.Values.t
  | EmailSuffixesUpdated of TenantEmailSuffixes.Values.t
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | LanguagesUpdated languages ->
    let%lwt _ = Repo.update_languages pool languages in
    Lwt.return_unit
  | EmailSuffixesUpdated _ -> failwith "Todo"
;;

let pp_event formatter event =
  match event with
  | LanguagesUpdated m -> TenantLanguages.Values.pp formatter m
  | EmailSuffixesUpdated m -> TenantEmailSuffixes.Values.pp formatter m
;;
