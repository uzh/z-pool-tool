module Common = Pool_common
module PoolError = Common.Message

module Key = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create key =
    if CCString.is_empty key then Error PoolError.(Invalid Key) else Ok key
  ;;

  let schema () =
    Conformist.custom
      (Pool_common.Utils.schema_decoder create PoolError.Key)
      CCList.pure
      "key"
  ;;
end

module Content = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create content =
    if CCString.is_empty content
    then Error PoolError.(Invalid Translation)
    else Ok content
  ;;

  let schema () =
    Conformist.custom
      (Pool_common.Utils.schema_decoder create PoolError.Translation)
      CCList.pure
      "content"
  ;;
end

type t =
  { id : Common.Id.t
  ; key : Key.t
  ; language : Pool_common.Language.t
  ; content : Content.t
  }
[@@deriving eq, show]

let create key language content =
  { id = Common.Id.create (); key; language; content }
;;

let id m = m.id
let key m = m.key
let language m = m.language
let content m = m.content
