(** Helper functions for creating Caqti types with custom encoder/decoders. *)

module Data = struct
  type _ t =
    | [] : unit t
    | ( :: ) : ('a * 'b t) -> ('a * 'b) t

  let rec make_value : type a. a t -> a =
    fun xs ->
    match xs with
    | [] -> ()
    | x :: xs -> x, make_value xs
  ;;
end

module Schema = struct
  type _ t =
    | [] : unit t
    | ( :: ) : ('a Caqti_type.t * 'b t) -> ('a * 'b) t

  let rec make_type : type a. a t -> a Caqti_type.t =
    fun xs ->
    match xs with
    | [] -> failwith "Schema shouldn't be empty"
    | x :: [] -> Caqti_type.(t2 x unit)
    | x :: xs -> Caqti_type.(t2 x (make_type xs))
  ;;
end

let custom
  : type a b.
    encode:(b -> (a Data.t, string) result)
    -> decode:(a -> (b, string) result)
    -> a Schema.t
    -> b Caqti_type.t
  =
  fun ~encode ~decode schema ->
  let typ = Schema.make_type schema in
  let encode data = encode data |> CCResult.map Data.make_value in
  Caqti_type.custom ~encode ~decode typ
;;

let custom_ok
  : type a b.
    encode:(b -> a Data.t) -> decode:(a -> b) -> a Schema.t -> b Caqti_type.t
  =
  let open CCFun in
  fun ~encode ~decode schema ->
    let typ = Schema.make_type schema in
    let encode = CCResult.(encode %> return %> map Data.make_value) in
    Caqti_type.custom ~encode ~decode:(decode %> CCResult.return) typ
;;
