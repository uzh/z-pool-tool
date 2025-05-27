open Ppx_yojson_conv_lib.Yojson_conv
open CCFun
open Entity

type multi_select_answer = SelectOption.Id.t list [@@deriving yojson]

let encode_yojson of_t t = t |> of_t |> Yojson.Safe.to_string |> CCResult.return

let decode_yojson t_of_yojson field t =
  let read s = s |> Yojson.Safe.from_string |> t_of_yojson in
  try Ok (read t) with
  | _ ->
    Error
      (Pool_message.(Error.Invalid field) |> Pool_common.Utils.error_to_string Language.En)
;;

module Id = struct
  include Id

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.string
      (of_string %> CCResult.return)
      value
  ;;
end

module FieldType = struct
  include Pool_common.Repo.Model.SelectorType (FieldType)
  include FieldType
end

module Model = Pool_common.Repo.Model.SelectorType (Model)

module Name = struct
  include Name

  let t =
    let encode = encode_yojson yojson_of_t in
    let decode = decode_yojson t_of_yojson Pool_message.Field.Name in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Hint = struct
  include Hint

  let t =
    let encode = encode_yojson yojson_of_t in
    let decode = decode_yojson t_of_yojson Pool_message.Field.Hint in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Validation = struct
  include Validation

  let t =
    let encode = encode_yojson CCFun.id in
    let decode = decode_yojson CCFun.id Pool_message.Field.Validation in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Required = struct
  include Required

  let t =
    Pool_common.Repo.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
  ;;
end

module Disabled = struct
  include Disabled

  let t =
    Pool_common.Repo.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
  ;;
end

module PublishedAt = struct
  include PublishedAt

  let t = Pool_common.Repo.make_caqti_type Caqti_type.ptime create value
end

module AdminHint = struct
  include AdminHint

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module AdminOverride = struct
  include AdminOverride

  let t =
    Pool_common.Repo.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
  ;;
end

module AdminViewOnly = struct
  include AdminViewOnly

  let t =
    Pool_common.Repo.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
  ;;
end

module AdminInputOnly = struct
  include AdminInputOnly

  let t =
    Pool_common.Repo.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
  ;;
end

module PromptOnRegistration = struct
  include PromptOnRegistration

  let t =
    Pool_common.Repo.make_caqti_type Caqti_type.bool (create %> CCResult.return) value
  ;;
end
