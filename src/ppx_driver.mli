open Import

(** Add one argument to the command line *)
val add_arg : Caml.Arg.key -> Caml.Arg.spec -> doc:string -> unit

(** [register_transformation name] registers a code transformation.

    [name] is a logical name for the transformation (such as [sexp_conv] or
    [bin_prot]). It is mostly used for debugging purposes.

    [rules] is a list of context independent rewriting rules, such as extension point
    expanders. This is what most code transformation should use. Rules from all registered
    transformations are all applied at the same time, before any other
    transformations. Moreover they are applied in a top-down manner, giving more control
    to extensions on how they interpret their payload.

    For instance:

    - some extensions capture a pretty-print of the payload in their expansion and using
      top-down ensures that the payload is as close as possible to the original code
    - some extensions process other extension in a special way inside their payload. For
      instance [%here] (from ppx_here) will normally expand to a record of type
      [Lexing.position]. However when used inside [%sexp] (from ppx_sexp_value) it will
      expand to the human-readable sexp representation of a source code position.

    [extensions] is a special cases of [rules] and is deprecated. It is only kept for
    backward compatibility.

    [enclose_impl] and [enclose_intf] produces a header and footer for
    implementation/interface files. They are a special case of [impl] and [intf]. Both
    functions receive a location that denotes the whole file, or [None] if the file
    contains nothing.

    [impl] is an optional function that is applied on implementation files and [intf] is
    an optional function that is applied on interface files. These two functions are
    applied on the AST of the whole file. They should only be used when the other
    mechanism are not enough. For instance if the transformation expands extension points
    that depend on the context.

    If no rewriter is using [impl] and [intf], then the whole transformation is completely
    independent of the order in which the various rewriter are specified. Moreover the
    resulting driver will be faster as it will do only one pass (excluding safety checks)
    on the whole AST.
*)
val register_transformation
  :  ?extensions   : Extension.t list (* deprecated, use ~rules instead *)
  -> ?rules        : Context_free.Rule.t list
  -> ?enclose_impl : (Location.t option -> structure * structure)
  -> ?enclose_intf : (Location.t option -> signature * signature)
  -> ?impl         : (structure -> structure)
  -> ?intf         : (signature -> signature)
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

(** If [true], code transformations should avoid generating code that is not strictly
    necessary, such as extra type annotations. *)
val pretty : unit -> bool

(**/**)
val map_structure : structure -> structure
