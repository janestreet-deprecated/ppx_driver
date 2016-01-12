open Ppx_core.Std
open Parsetree

(** Add one argument to the command line *)
val add_arg : Arg.key -> Arg.spec -> doc:string -> unit

(** [register_transformation name] registers a code transformation.

    [name] is a logical name for the transformation (such as [sexp_conv] or
    [bin_prot]). It is mostly used for debugging purposes.

    [extensions] is a list of context independant extension point expanders. This is what
    most code transformation should use. Extension expanders from all registered
    transformations are applied at the same time, before any other
    transformations. Moreover they are applied in a top-down manner, for the following
    reason:

    - some extensions capture a pretty-print of the payload in their expansion and using
      top-down ensures that the payload is as close as possible to the original code
    - some extensions process other extension in a special way inside their payload. For
      instance [%here] (from ppx_here) will normally expand to a record of type
      [Lexing.position]. However when used inside [%sexp] (from ppx_sexp_value) it will
      expand to the human-readable sexp representation of a source code position.

    [impl] is an optional function that is applied on implementation files and [intf] is
    an optional function that is applied on interface files. These two functions are
    applied on the AST of the whole file. They should only be used in specific cases:

    1. the transformation needs to add an header and/or footer
    2. the transformation expands extension points that depend on the context
*)
val register_transformation
  :  ?extensions : Extension.V2.t list
  -> ?impl       : (structure -> structure)
  -> ?intf       : (signature -> signature)
  -> string
  -> unit

(** Same as:

    {[
      register_transformation
        ~name
        ~impl
        ~intf
        ()
    ]}
*)
val register_code_transformation
  :  name:string
  -> impl:(structure -> structure)
  -> intf:(signature -> signature)
  -> unit
  [@@deprecated "[since 2015-11] use register_transformation instead"]

(** Suitable for -pp and also usable as a standalone command line tool.

    If the first command line argument is [-as-ppx] then it will run as a ppx rewriter. *)
val standalone : unit -> unit

(** Suitable for -ppx. Used only for the public release. *)
val run_as_ppx_rewriter : unit -> unit
