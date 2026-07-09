(* Extracted from the Sihl web framework (https://github.com/oxidizing/sihl), MIT license.
   Source:
   https://github.com/uzh/sihl/blob/6c3c4040413294155cda3a363edf0ff3c7e638b8/sihl/src/core_random.ml
   The unused [Uuid] module has been removed. *)

let () = Stdlib.Random.self_init ()

let rec chars result n =
  if n > 0
  then chars (List.cons (Char.chr (Stdlib.Random.int 255)) result) (n - 1)
  else result |> List.to_seq |> String.of_seq
;;

let bytes nr = chars [] nr
let base64 nr = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet (bytes nr)

let random_cmd =
  Command.make
    ~name:"random"
    ~help:"<number of bytes>"
    ~description:
      "Generates a random string with the given length in bytes. The string is base64 \
       encoded. Use the generated value for SIHL_SECRET."
    (function
    | [ n ] ->
      (match int_of_string_opt n with
       | Some n ->
         print_endline @@ base64 n;
         Lwt.return @@ Some ()
       | None -> failwith "Invalid number of bytes provided")
    | _ -> Lwt.return None)
;;
