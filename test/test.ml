open Ppx_compare_lib.Builtin
open Sexplib.Std

type 'a t =
  | A of 'a
  | B of string list
  | C of 'a t
  | Plop
[@@deriving_inline sexp, compare]
let rec t_of_sexp : type a.(Sexplib.Sexp.t -> a) -> Sexplib.Sexp.t -> a t =
  let _tp_loc = "test.ml.t"  in
  fun _of_a  ->
    function
    | Sexplib.Sexp.List ((Sexplib.Sexp.Atom ("a"|"A" as _tag))::sexp_args) as
      _sexp ->
      (match sexp_args with
       | v0::[] -> let v0 = _of_a v0  in A v0
       | _ -> Sexplib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
    | Sexplib.Sexp.List ((Sexplib.Sexp.Atom ("b"|"B" as _tag))::sexp_args) as
      _sexp ->
      (match sexp_args with
       | v0::[] -> let v0 = list_of_sexp string_of_sexp v0  in B v0
       | _ -> Sexplib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
    | Sexplib.Sexp.List ((Sexplib.Sexp.Atom ("c"|"C" as _tag))::sexp_args) as
      _sexp ->
      (match sexp_args with
       | v0::[] -> let v0 = t_of_sexp _of_a v0  in C v0
       | _ -> Sexplib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
    | Sexplib.Sexp.Atom ("plop"|"Plop") -> Plop
    | Sexplib.Sexp.Atom ("a"|"A") as sexp ->
      Sexplib.Conv_error.stag_takes_args _tp_loc sexp
    | Sexplib.Sexp.Atom ("b"|"B") as sexp ->
      Sexplib.Conv_error.stag_takes_args _tp_loc sexp
    | Sexplib.Sexp.Atom ("c"|"C") as sexp ->
      Sexplib.Conv_error.stag_takes_args _tp_loc sexp
    | Sexplib.Sexp.List ((Sexplib.Sexp.Atom ("plop"|"Plop"))::_) as sexp ->
      Sexplib.Conv_error.stag_no_args _tp_loc sexp
    | Sexplib.Sexp.List ((Sexplib.Sexp.List _)::_) as sexp ->
      Sexplib.Conv_error.nested_list_invalid_sum _tp_loc sexp
    | Sexplib.Sexp.List [] as sexp ->
      Sexplib.Conv_error.empty_list_invalid_sum _tp_loc sexp
    | sexp -> Sexplib.Conv_error.unexpected_stag _tp_loc sexp

let rec sexp_of_t : type a.(a -> Sexplib.Sexp.t) -> a t -> Sexplib.Sexp.t =
  fun _of_a  ->
    function
    | A v0 ->
      let v0 = _of_a v0  in Sexplib.Sexp.List [Sexplib.Sexp.Atom "A"; v0]
    | B v0 ->
      let v0 = sexp_of_list sexp_of_string v0  in
      Sexplib.Sexp.List [Sexplib.Sexp.Atom "B"; v0]
    | C v0 ->
      let v0 = sexp_of_t _of_a v0  in
      Sexplib.Sexp.List [Sexplib.Sexp.Atom "C"; v0]
    | Plop  -> Sexplib.Sexp.Atom "Plop"

let rec compare : 'a . ('a -> 'a -> int) -> 'a t -> 'a t -> int =
  fun _cmp__a  ->
  fun a__001_  ->
  fun b__002_  ->
    if Ppx_compare_lib.phys_equal a__001_ b__002_
    then 0
    else
      (match (a__001_, b__002_) with
       | (A _a__003_,A _b__004_) -> _cmp__a _a__003_ _b__004_
       | (A _,_) -> (-1)
       | (_,A _) -> 1
       | (B _a__005_,B _b__006_) ->
         compare_list compare_string _a__005_ _b__006_
       | (B _,_) -> (-1)
       | (_,B _) -> 1
       | (C _a__009_,C _b__010_) -> compare _cmp__a _a__009_ _b__010_
       | (C _,_) -> (-1)
       | (_,C _) -> 1
       | (Plop ,Plop ) -> 0)

[@@@end]
