(* Extracted from the Sihl web framework (https://github.com/oxidizing/sihl), MIT license.
   These are the file types of the former [Sihl.Contract.Storage] module.
   Source:
   https://github.com/uzh/sihl/blob/6c3c4040413294155cda3a363edf0ff3c7e638b8/sihl/src/contract_storage.ml
   The service signature has been removed. *)

type file =
  { id : string
  ; filename : string
  ; filesize : int
  ; mime : string
  }

type stored =
  { file : file
  ; blob : string
  }

let name = "storage"

exception Exception of string

let file_to_sexp { id; filename; filesize; mime } =
  let open Sexplib0.Sexp_conv in
  let open Sexplib0.Sexp in
  List
    [ List [ Atom "id"; sexp_of_string id ]
    ; List [ Atom "filename"; sexp_of_string filename ]
    ; List [ Atom "filesize"; sexp_of_int filesize ]
    ; List [ Atom "mime"; sexp_of_string mime ]
    ]
;;

let pp_file fmt t = Sexplib0.Sexp.pp_hum fmt (file_to_sexp t)
let set_mime mime file = { file with mime }
let set_filesize filesize file = { file with filesize }
let set_filename filename file = { file with filename }

let set_mime_stored mime stored_file =
  { stored_file with file = set_mime mime stored_file.file }
;;

let set_filesize_stored size stored_file =
  { stored_file with file = set_filesize size stored_file.file }
;;

let set_filename_stored name stored_file =
  { stored_file with file = set_filename name stored_file.file }
;;

let stored_to_sexp { file; _ } =
  let open Sexplib0.Sexp_conv in
  let open Sexplib0.Sexp in
  List
    [ List [ Atom "file"; file_to_sexp file ]
    ; List [ Atom "blob"; sexp_of_string "<binary>" ]
    ]
;;

let pp_stored fmt t = Sexplib0.Sexp.pp_hum fmt (stored_to_sexp t)
