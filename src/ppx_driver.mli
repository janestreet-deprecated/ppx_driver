open Ppx_core.Std
open Parsetree

(** Add one argument to the command line *)
val add_arg : Arg.key -> Arg.spec -> doc:string -> unit

(** Register a code transformation. [impl] is the function that is applied on
    implementation files and [intf] is the one applied on interface files.

    [name] is a logical name for the transformation (such as [sexp_conv] or
    [bin_prot]). It is not currently used but we might use it in the future for debugging
    purposes. *)
val register_code_transformation
  :  name:string
  -> impl:(structure -> structure)
  -> intf:(signature -> signature)
  -> unit

(** Suitable for -pp and also usable as a standalone command line tool.

    If the first command line argument is [-as-ppx] then it will run as a ppx rewriter. *)
val standalone : unit -> unit

(** Suitable for -ppx. Used only for the public release. *)
val run_as_ppx_rewriter : unit -> unit
