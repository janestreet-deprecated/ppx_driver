open StdLabels
open Ppx_core.Std

module Caller_id = Ppx_core.Caller_id

module List = struct
  include List

  let rec filter_map l ~f =
    match l with
    | [] -> []
    | x :: l ->
      match f x with
      | None -> filter_map l ~f
      | Some x -> x :: filter_map l ~f
end

let exe_name = Filename.basename Sys.executable_name

let args = ref []

let add_arg key spec ~doc = args := (key, spec, doc) :: !args

let perform_checks = ref true
let debug_attribute_drop = ref false
let apply_list = ref None
let preprocessor = ref None
let no_merge = ref false
let request_print_passes = ref false
let request_print_transformations = ref false

module Transform = struct
  type t =
    { name          : string
    ; impl          : (Parsetree.structure -> Parsetree.structure) option
    ; intf          : (Parsetree.signature -> Parsetree.signature) option
    ; enclose_impl  : (Location.t option -> Parsetree.structure * Parsetree.structure) option
    ; enclose_intf  : (Location.t option -> Parsetree.signature * Parsetree.signature) option
    ; rules         : Context_free.Rule.t list
    ; registered_at : Caller_id.t
    }

  let all : t list ref = ref []

  let print_caller_id oc (caller_id : Caller_id.t) =
    match caller_id with
    | None -> output_string oc "<unknown location>"
    | Some loc -> Printf.fprintf oc "%s:%d" loc.filename loc.line_number
  ;;

  let register ?(extensions=[]) ?(rules=[]) ?enclose_impl ?enclose_intf
        ?impl ?intf name =
    let rules =
      List.map extensions ~f:Context_free.Rule.extension @ rules
    in
    let caller_id = Caller_id.get ~skip:[__FILE__] in
    begin match List.filter !all ~f:(fun ct -> ct.name = name) with
    | [] -> ()
    | ct :: _ ->
      Printf.eprintf "Warning: code transformation %s registered twice.\n" name;
      Printf.eprintf "  - first time was at %a\n" print_caller_id ct.registered_at;
      Printf.eprintf "  - second time is at %a\n" print_caller_id caller_id;
    end;
    let ct =
      { name
      ; rules
      ; enclose_impl
      ; enclose_intf
      ; impl
      ; intf
      ; registered_at = caller_id
      }
    in
    all := ct :: !all
  ;;

  let rec last prev l =
    match l with
    | [] -> prev
    | x :: l -> last x l
  ;;

  let loc_of_list ~get_loc l =
    match l with
    | [] -> None
    | x :: l ->
      let first : Location.t = get_loc x in
      let last = get_loc (last x l) in
      Some { first with loc_end = last.loc_end }
  ;;

  let merge_into_generic_mappers t =
    let { rules; enclose_impl; enclose_intf; impl; intf; _ } = t in
    let map = new Context_free.map_top_down rules in
    let map_impl st =
      let st =
        let header, footer =
          match enclose_impl with
          | None   -> ([], [])
          | Some f ->
            let whole_loc = loc_of_list st ~get_loc:(fun st -> st.Parsetree.pstr_loc) in
            f whole_loc
        in
        let st = map#structure (File_path.get_default_path_str st) st in
        match header, footer with
        | [], [] -> st
        | _      -> List.concat [ header; st; footer ]
      in
      match impl with
      | None -> st
      | Some f -> f st
    in
    let map_intf sg =
      let sg =
        let header, footer =
          match enclose_intf with
          | None   -> ([], [])
          | Some f ->
            let whole_loc = loc_of_list sg ~get_loc:(fun sg -> sg.Parsetree.psig_loc) in
            f whole_loc
        in
        let sg = map#signature (File_path.get_default_path_sig sg) sg in
        match header, footer with
        | [], [] -> sg
        | _      -> List.concat [ header; sg; footer ]
      in
      match intf with
      | None -> sg
      | Some f -> f sg
    in
    { t with
      impl = Some map_impl
    ; intf = Some map_intf
    }

  let builtin_of_context_free_rewriters ~rules ~enclose_impl ~enclose_intf =
    merge_into_generic_mappers
      { name = "<bultin:context-free>"
      ; impl = None
      ; intf = None
      ; enclose_impl
      ; enclose_intf
      ; rules
      ; registered_at = Caller_id.get ~skip:[]
      }
end

let register_transformation = Transform.register

let register_code_transformation ~name ~impl ~intf =
  register_transformation name ~impl ~intf
;;

let debug_dropped_attribute name ~old_dropped ~new_dropped =
  let print_diff what a b =
    let diff =
      List.filter a ~f:(fun (name : _ Location.loc) ->
        not (List.exists b ~f:(fun (name' : _ Location.loc) -> name.txt == name'.txt)))
    in
    if diff <> [] then begin
      Format.eprintf "The following attributes %s after applying %s:\n"
        what name;
      List.iter diff ~f:(fun { Location. txt; loc } ->
        Format.eprintf "- %a: %s\n" Location.print_loc loc txt);
      Format.eprintf "@."
    end
  in
  print_diff "disappeared" new_dropped old_dropped;
  print_diff "reappeared"  old_dropped new_dropped
;;

let get_whole_ast_passes () =
  let cts =
    match !apply_list with
    | None -> List.rev !Transform.all
    | Some names ->
      List.map names ~f:(fun name ->
        List.find !Transform.all ~f:(fun (ct : Transform.t) ->
          ct.name = name))
  in
  let cts =
    if !no_merge then
      List.map cts ~f:Transform.merge_into_generic_mappers
    else begin
      let get_enclosers ~f =
        List.filter_map cts ~f:(fun (ct : Transform.t) ->
          match f ct with
          | None -> None
          | Some x -> Some (ct.name, x))
        (* Sort them to ensure deterministic ordering *)
        |> List.sort ~cmp:(fun (a, _) (b, _) -> String.compare a b)
        |> List.map ~f:snd
      in

      let rules =
        List.map cts ~f:(fun (ct : Transform.t) -> ct.rules) |> List.concat
      and impl_enclosers =
        get_enclosers ~f:(fun ct -> ct.enclose_impl)
      and intf_enclosers =
        get_enclosers ~f:(fun ct -> ct.enclose_intf)
      in
      match rules, impl_enclosers, intf_enclosers with
      | [], [], [] -> cts
      | _              ->
        let merge_encloser = function
          | [] -> None
          | enclosers -> Some (fun loc ->
            let headers, footers =
              List.map enclosers ~f:(fun f -> f loc)
              |> List.split
            in
            let headers = List.concat headers in
            let footers = List.concat (List.rev footers) in
            (headers, footers))
        in
        Transform.builtin_of_context_free_rewriters ~rules
          ~enclose_impl:(merge_encloser impl_enclosers)
          ~enclose_intf:(merge_encloser intf_enclosers)
        :: cts
    end
  in
  List.filter cts ~f:(fun (ct : Transform.t) ->
    match ct.impl, ct.intf with
    | None, None -> false
    | _          -> true)
;;

let print_passes () =
  let cts = get_whole_ast_passes () in
  if !perform_checks then
    Printf.printf "<builtin:freshen-and-collect-attributes>\n";
  List.iter cts ~f:(fun ct -> Printf.printf "%s\n" ct.Transform.name);
  if !perform_checks then
    Printf.printf "<builtin:check-unused-attributes>\n\
                   <builtin:check-unused-extensions>\n";
;;

let apply_transforms ~field ~dropped_so_far x =
  let cts = get_whole_ast_passes () in
  List.fold_left cts ~init:(x, []) ~f:(fun (x, dropped) (ct : Transform.t) ->
    match field ct with
    | None -> (x, dropped)
    | Some f ->
      let x = f x in
      let dropped =
        if !debug_attribute_drop then begin
          let new_dropped = dropped_so_far x in
          debug_dropped_attribute ct.name ~old_dropped:dropped ~new_dropped;
          new_dropped
        end else
          []
      in
      (x, dropped))
  |> fst
;;

let map_structure st =
  let st =
    if !perform_checks then begin
      Attribute.reset_checks ();
      Attribute.freshen_and_collect#structure st
    end else
      st
  in
  let st =
    apply_transforms st ~field:(fun (ct : Transform.t) -> ct.impl)
      ~dropped_so_far:Attribute.dropped_so_far_structure
  in
  if !perform_checks then begin
    Attribute.check_unused#structure st;
    Extension.check_unused#structure st;
    Attribute.check_all_seen ();
  end;
  st
;;

let map_signature sg =
  let sg =
    if !perform_checks then begin
      Attribute.reset_checks ();
      Attribute.freshen_and_collect#signature sg
    end else
      sg
  in
  let sg =
    apply_transforms sg ~field:(fun ct -> ct.intf)
      ~dropped_so_far:Attribute.dropped_so_far_signature
  in
  if !perform_checks then begin
    Attribute.check_unused#signature sg;
    Extension.check_unused#signature sg;
    Attribute.check_all_seen ();
  end;
  sg
;;

let mapper =
  let structure _ st = map_structure st in
  let signature _ sg = map_signature sg in
  { Ast_mapper.default_mapper with structure; signature }
;;

let as_ppx_rewriter_main argv =
  let argv = Sys.executable_name :: argv in
  let usage =
    Printf.sprintf "%s [extra_args] <infile> <outfile>" exe_name
  in
  match
    Arg.parse_argv (Array.of_list argv) (Arg.align (List.rev !args))
      (fun _ -> raise @@ Arg.Bad "anonymous arguments not accepted")
      usage
  with
  | exception Arg.Bad  msg -> prerr_string msg; exit 2
  | exception Arg.Help msg -> prerr_string msg; exit 0
  | () -> mapper

let run_as_ppx_rewriter () =
  perform_checks := false;
  Ast_mapper.run_main as_ppx_rewriter_main

module Kind = struct
  type _ t =
    | Intf : Parsetree.signature t
    | Impl : Parsetree.structure t
  ;;

  type packed = T : _ list t -> packed

  let of_filename fn =
    if Filename.check_suffix fn ".ml" then
      Some (T Impl)
    else if Filename.check_suffix fn ".mli" then
      Some (T Intf)
    else
      None
  ;;

  let ast_magic : type a. a t -> string = function
    | Impl -> Config.ast_impl_magic_number
    | Intf -> Config.ast_intf_magic_number
  ;;
end

let is_ast_file kind fn ic =
  let ast_magic = Kind.ast_magic kind in
  try
    let buffer = really_input_string ic (String.length ast_magic) in
    if buffer = ast_magic then
      true
    else if String.sub buffer ~pos:0 ~len:9 = String.sub ast_magic ~pos:0 ~len:9 then
      Location.raise_errorf ~loc:(Location.in_file fn)
        "File is a binary ast for a different version of OCaml"
    else
      false
  with _ ->
    false
;;

let output_ast oc kind ast =
  output_string oc (Kind.ast_magic kind);
  output_value oc !Location.input_name;
  output_value oc ast;
;;

let with_output fn ~binary ~f =
  match fn with
  | None | Some "-" -> f stdout
  | Some fn ->
    let oc = if binary then open_out_bin fn else open_out fn in
    match f oc with
    | v -> close_out oc; v
    | exception e -> close_out oc; raise e
;;

exception Pp_error of string

let report_pp_error ppf cmd =
  Format.fprintf ppf "Error while running external preprocessor@.\
                      Command line: %s@." cmd

let () =
  Location.register_error_of_exn
    (function
      | Pp_error cmd -> Some (Location.error_of_printer_file report_pp_error cmd)
      | _ -> None)

let remove_no_error fn =
  try Sys.remove fn with Sys_error _ -> ()

let protectx x ~f ~finally =
  match f x with
  | v -> finally x; v
  | exception e -> finally x; raise e
;;

let with_preprocessed_file fn ~f =
  match !preprocessor with
  | None -> f fn
  | Some pp ->
    protectx (Filename.temp_file "ocamlpp" "")
      ~finally:remove_no_error
      ~f:(fun tmpfile ->
        let comm =
          Printf.sprintf "%s %s > %s"
            pp (if fn = "-" then "" else Filename.quote fn) (Filename.quote tmpfile)
        in
        if Sys.command comm <> 0 then raise (Pp_error comm);
        f tmpfile)

let with_preprocessed_input fn ~f =
  with_preprocessed_file fn ~f:(fun fn ->
    if fn = "-" then
      f stdin
    else
      protectx (open_in_bin fn) ~f ~finally:close_in)
;;

let relocate = object
  inherit [string * string] Ast_traverse.map_with_context

  method! lexing_position (old_fn, new_fn) pos =
    if pos.pos_fname = old_fn then
      { pos with pos_fname = new_fn }
    else
      pos
end

let load_input : type a. a Kind.t -> string -> string -> in_channel -> a =
  fun kind fn input_name ic ->
    Location.input_name := input_name;
    if is_ast_file kind fn ic then begin
      let ast_input_name : string = input_value ic in
      let ast : a = input_value ic in
      if ast_input_name = input_name then
        ast
      else
        let relocate : _ -> a -> a =
          match kind with
          | Kind.Intf -> relocate#signature
          | Kind.Impl -> relocate#structure
        in
        relocate (ast_input_name, input_name) ast
    end else begin
      seek_in ic 0;
      let lexbuf = Lexing.from_channel ic in
      Location.init lexbuf input_name;
      match kind with
      | Kind.Intf -> Parse.interface      lexbuf
      | Kind.Impl -> Parse.implementation lexbuf
    end
;;

type output_mode =
  | Pretty_print
  | Dump_ast
  | Dparsetree

let process_file : type a. a list Kind.t -> _ = fun kind fn ~input_name ~output_mode ~output ->
  let ast : a list =
    try
      let ast = with_preprocessed_input fn ~f:(load_input kind fn input_name) in
      match kind with
      | Kind.Intf -> map_signature ast
      | Kind.Impl -> map_structure ast
    with exn when output_mode = Dump_ast ->
    match Location.error_of_exn exn with
    | None -> raise exn
    | Some error ->
      let mk_ext : loc:_ -> _ -> _ -> a =
        let open Ast_builder.Default in
        match kind with
        | Kind.Intf -> psig_extension
        | Kind.Impl -> pstr_extension
      in
      [ mk_ext ~loc:Location.none (Ast_mapper.extension_of_error error) [] ]
  in
  let null_ast =
    match kind with
    | Kind.Intf -> (match ast with [] -> true | _::_ -> false)
    | Kind.Impl -> (match ast with [] -> true | _::_ -> false)
  in
  match output_mode with
  | Pretty_print ->
    with_output output ~binary:false ~f:(fun oc ->
      let ppf = Format.formatter_of_out_channel oc in
      (match kind with
       | Kind.Intf -> Pprintast.signature ppf ast
       | Kind.Impl -> Pprintast.structure ppf ast);
      if not null_ast then Format.pp_print_newline ppf ())
  | Dump_ast ->
    with_output output ~binary:true ~f:(fun oc -> output_ast oc kind ast)
  | Dparsetree ->
    with_output output ~binary:false ~f:(fun oc ->
      let ppf = Format.formatter_of_out_channel oc in
      (match kind with
       | Kind.Intf -> Printast.interface      ppf ast
       | Kind.Impl -> Printast.implementation ppf ast);
      Format.pp_print_newline ppf ())
;;

let loc_fname = ref None
let output_mode = ref Pretty_print
let output = ref None
let kind = ref None

let set_kind k =
  let k = Kind.T k in
  match !kind with
  | Some k' when k <> k' -> raise (Arg.Bad "must specify at most one of -impl or -intf")
  | _ -> kind := Some k
;;

let set_output_mode mode =
  match !output_mode, mode with
  | Pretty_print, _ -> output_mode := mode
  | _, Pretty_print -> assert false
  | Dump_ast  , Dump_ast
  | Dparsetree, Dparsetree -> ()
  | Dump_ast, Dparsetree
  | Dparsetree, Dump_ast -> raise (Arg.Bad "-dump-ast and -dparsetree are incompatible")
;;

let print_transformations () =
  List.iter !Transform.all ~f:(fun (ct : Transform.t) ->
    Printf.printf "%s\n" ct.name);
;;

let set_apply_list s =
  let rec split i j =
    if j = String.length s then
      [String.sub s ~pos:i ~len:(j - i)]
    else if s.[j] = ',' then
      String.sub s ~pos:i ~len:(j - i) :: split (j + 1) (j + 1)
    else
      split i (j + 1)
  in
  let names = if s = "" then [] else split 0 0 in
  List.iter names ~f:(fun name ->
    if not (List.exists !Transform.all ~f:(fun (ct : Transform.t) ->
      ct.name = name)) then
      raise (Arg.Bad (Printf.sprintf "code transformation '%s' does not exist" name)));
  apply_list := Some names
;;

let use_optcomp = ref true

let standalone_args =
  [ "-as-ppx", Arg.Unit (fun () -> raise (Arg.Bad "-as-ppx must be the first argument")),
    " Run as a -ppx rewriter (must be the first argument)"
  ; "-o", Arg.String (fun s -> output := Some s),
    "<filename> Output file (use '-' for stdout)"
  ; "-reserve-namespace", Arg.String Attribute.reserve_namespace,
    "<string> Mark the given namespace as reserved"
  ; "-loc-filename", Arg.String (fun s -> loc_fname := Some s),
    "<string> File name to use in locations"
  ; "-no-optcomp", Arg.Clear use_optcomp,
    " Do not use optcomp (default if the input or output of -pp is a binary AST)"
  ; "-dump-ast", Arg.Unit (fun () -> set_output_mode Dump_ast),
    " Dump the marshaled ast to the output file instead of pretty-printing it"
  ; "-dparsetree", Arg.Unit (fun () -> set_output_mode Dparsetree),
    " Print the parsetree (same as ocamlc -dparsetree)"
  ; "-impl", Arg.Unit (fun () -> set_kind Impl),
    "<file> Treat the input as a .ml file"
  ; "-intf", Arg.Unit (fun () -> set_kind Intf),
    "<file> Treat the input as a .mli file"
  ; "-no-check", Arg.Clear perform_checks,
    " Disable checks (unsafe)"
  ; "-debug-attribute-drop", Arg.Set debug_attribute_drop,
    " Debug attribute dropping"
  ; "-print-transformations", Arg.Set request_print_transformations,
    " Print linked-in code transformations, in the order they are applied"
  ; "-print-passes", Arg.Set request_print_passes,
    " Print the actual passes over the whole AST in the order they are applied"
  ; "-apply", Arg.String set_apply_list,
    "<names> Apply these transformations in order (comma-separated list)"
  ; "-no-merge", Arg.Set no_merge,
    " Do not merge context free transformations (better for debugging rewriters)"
  ; "-pp", Arg.String (fun s -> preprocessor := Some s),
    "<command>  Pipe sources through preprocessor <command> (incompatible with -as-ppx)"
  ]
;;

module Optcomp = Ppx_optcomp.Make(struct
    let lexer = Lexer.token
    let env = Ppx_optcomp.Env.init
  end)

let standalone_main () =
  let usage =
    Printf.sprintf "%s [extra_args] [<files>]" exe_name
  in
  let args = List.rev_append !args standalone_args in
  let input = ref None in
  let anon fn =
    match !input with
    | None -> input := Some fn
    | Some _ -> raise (Arg.Bad "too many input files")
  in
  Arg.parse (Arg.align args) anon usage;
  if !request_print_transformations then begin
    print_transformations ();
    exit 0;
  end;
  if !request_print_passes then begin
    print_passes ();
    exit 0;
  end;
  if !use_optcomp then Lexer.set_preprocessor Optcomp.init Optcomp.map_lexer;
  match !input with
  | None    -> Printf.eprintf "%s: no input file given\n%!" exe_name
  | Some fn ->
    let Kind.T kind =
      match !kind with
      | Some k -> k
      | None ->
        match Kind.of_filename fn with
        | Some k -> k
        | None ->
          Printf.eprintf "%s: don't know what to do with '%s', use -impl or -intf.\n"
            exe_name fn;
          exit 2
    in
    let input_name =
      match !loc_fname with
      | None    -> fn
      | Some fn -> fn
    in
    process_file kind fn ~input_name ~output_mode:!output_mode ~output:!output
;;

let standalone_run_as_ppx_rewriter () =
  let n = Array.length Sys.argv in
  let usage = Printf.sprintf "%s -as-ppx [extra_args] <infile> <outfile>" exe_name in
  if n < 4 then begin
    Printf.eprintf "Usage: %s\n%!" usage;
    exit 2
  end;
  let argv = Array.make (n - 3) "" in
  argv.(0) <- Sys.argv.(0);
  for i = 1 to (n - 4) do
    argv.(i) <- Sys.argv.(i + 1)
  done;
  match
    Arg.parse_argv argv (Arg.align (List.rev !args))
      (fun _ -> raise @@ Arg.Bad "anonymous arguments not accepted")
      usage
  with
  | exception Arg.Bad  msg -> prerr_string msg; exit 2
  | exception Arg.Help msg -> prerr_string msg; exit 0
  | () ->
    Ast_mapper.apply ~source:Sys.argv.(n - 2) ~target:Sys.argv.(n - 1) mapper
;;

let standalone () =
  try
    if Array.length Sys.argv >= 2 && Sys.argv.(1) = "-as-ppx" then begin
      standalone_run_as_ppx_rewriter ()
    end else
      standalone_main ()
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
;;
