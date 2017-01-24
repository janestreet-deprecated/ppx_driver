include Ppx_core

let with_output fn ~binary ~f =
  match fn with
  | None | Some "-" -> f stdout
  | Some fn -> Out_channel.with_file fn ~binary ~f
;;

module Kind = struct
  type t = Intf | Impl

  let of_filename fn : t option =
    if Caml.Filename.check_suffix fn ".ml" then
      Some Impl
    else if Caml.Filename.check_suffix fn ".mli" then
      Some Intf
    else
      None
  ;;

  let describe = function
    | Impl -> "implementation"
    | Intf -> "interface"
  ;;

  let equal : t -> t -> bool = Polymorphic_compare.equal
end

module Intf_or_impl = struct
  type ('a, 'b) t = ('a, 'b) Migrate_parsetree.intf_or_impl =
    | Intf of 'a
    | Impl of 'b

  let map t (map : Ast_traverse.map) =
    match t with
    | Impl x -> Impl (map#structure x)
    | Intf x -> Intf (map#signature x)
  ;;

  let map_with_context t (map : _ Ast_traverse.map_with_context) ctx =
    match t with
    | Impl x -> Impl (map#structure ctx x)
    | Intf x -> Intf (map#signature ctx x)
  ;;

  let kind : _ -> Kind.t = function
    | Intf _ -> Intf
    | Impl _ -> Impl
end

let map_impl x ~(f : _ Intf_or_impl.t -> _ Intf_or_impl.t) =
  match f (Impl x) with
  | Impl x -> x
  | Intf _ -> assert false

let map_intf x ~(f : _ Intf_or_impl.t -> _ Intf_or_impl.t) =
  match f (Intf x) with
  | Intf x -> x
  | Impl _ -> assert false
