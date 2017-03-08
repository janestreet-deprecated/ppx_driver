open Import

module Arg = Caml.Arg

let exe_name = Caml.Filename.basename Caml.Sys.executable_name

let args = ref []

let add_arg key spec ~doc = args := (key, spec, doc) :: !args

let perform_checks = ref true
let debug_attribute_drop = ref false
let apply_list = ref None
let preprocessor = ref None
let no_merge = ref false
let request_print_passes = ref false
let request_print_transformations = ref false
let use_color = ref true
let diff_command = ref None
let pretty = ref false
let styler = ref None

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
    | None -> Out_channel.output_string oc "<unknown location>"
    | Some loc -> Out_channel.fprintf oc "%s:%d" loc.filename loc.line_number
  ;;

  let register ?(extensions=[]) ?(rules=[]) ?enclose_impl ?enclose_intf
        ?impl ?intf name =
    let rules =
      List.map extensions ~f:Context_free.Rule.extension @ rules
    in
    let caller_id = Caller_id.get ~skip:[Caml.__FILE__] in
    begin match List.filter !all ~f:(fun ct -> String.equal ct.name name) with
    | [] -> ()
    | ct :: _ ->
      eprintf "Warning: code transformation %s registered twice.\n" name;
      eprintf "  - first time was at %a\n" print_caller_id ct.registered_at;
      eprintf "  - second time is at %a\n" print_caller_id caller_id;
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

  let merge_into_generic_mappers t ~hook ~expect_mismatch_handler =
    let { rules; enclose_impl; enclose_intf; impl; intf; _ } = t in
    let map =
      new Context_free.map_top_down rules
        ~generated_code_hook:hook
        ~expect_mismatch_handler
    in
    let gen_header_and_footer context whole_loc f =
      let header, footer = f whole_loc in
      (match whole_loc with
      | Some (loc : Location.t) ->
        let loc_header = { loc with loc_end   = loc.loc_start } in
        let loc_footer = { loc with loc_start = loc.loc_end   } in
        (match header with [] -> () | _ -> hook.f context loc_header (Many header));
        (match footer with [] -> () | _ -> hook.f context loc_footer (Many footer))
      | None ->
        match header @ footer with
        | [] -> ()
        | l ->
          let pos =
            { Lexing.
              pos_fname = ""
            ; pos_lnum  = 1
            ; pos_bol   = 0
            ; pos_cnum  = 0
            }
          in
          let loc = { Location. loc_start = pos; loc_end = pos; loc_ghost = false } in
          hook.f context loc (Many l));
      (header, footer)
    in
    let map_impl st =
      let st =
        let header, footer =
          match enclose_impl with
          | None   -> ([], [])
          | Some f ->
            let whole_loc = loc_of_list st ~get_loc:(fun st -> st.Parsetree.pstr_loc) in
            gen_header_and_footer Structure_item whole_loc f
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
            gen_header_and_footer Signature_item whole_loc f
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

  let builtin_of_context_free_rewriters ~hook ~rules ~enclose_impl ~enclose_intf =
    merge_into_generic_mappers ~hook
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

let register_transformation_using_ocaml_current_ast  ?impl ?intf name =
  let impl = Option.map impl ~f:(Ppx_ast.Selected_ast.of_ocaml_mapper Structure) in
  let intf = Option.map intf ~f:(Ppx_ast.Selected_ast.of_ocaml_mapper Signature) in
  register_transformation ?impl ?intf name

let debug_dropped_attribute name ~old_dropped ~new_dropped =
  let print_diff what a b =
    let diff =
      List.filter a ~f:(fun (name : _ Loc.t) ->
        not (List.exists b ~f:(fun (name' : _ Location.loc) -> phys_equal name.txt name'.txt)))
    in
    if not (List.is_empty diff) then begin
      eprintf "The following attributes %s after applying %s:\n"
        what name;
      List.iter diff ~f:(fun { Location. txt; loc } ->
        Caml.Format.eprintf "- %a: %s\n" Location.print loc txt);
      Caml.Format.eprintf "@."
    end
  in
  print_diff "disappeared" new_dropped old_dropped;
  print_diff "reappeared"  old_dropped new_dropped
;;

let get_whole_ast_passes ~hook ~expect_mismatch_handler =
  let cts =
    match !apply_list with
    | None -> List.rev !Transform.all
    | Some names ->
      List.map names ~f:(fun name ->
        List.find_exn !Transform.all ~f:(fun (ct : Transform.t) ->
          String.equal ct.name name))
  in
  let cts =
    if !no_merge then
      List.map cts ~f:(Transform.merge_into_generic_mappers ~hook
                         ~expect_mismatch_handler)
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
              |> List.unzip
            in
            let headers = List.concat headers in
            let footers = List.concat (List.rev footers) in
            (headers, footers))
        in
        Transform.builtin_of_context_free_rewriters ~rules ~hook ~expect_mismatch_handler
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
  let cts =
    get_whole_ast_passes ~hook:Context_free.Generated_code_hook.nop
      ~expect_mismatch_handler:Context_free.Expect_mismatch_handler.nop
  in
  if !perform_checks then
    printf "<builtin:freshen-and-collect-attributes>\n";
  List.iter cts ~f:(fun ct -> printf "%s\n" ct.Transform.name);
  if !perform_checks then
    printf "<builtin:check-unused-attributes>\n\
            <builtin:check-unused-extensions>\n";
;;

let apply_transforms ~field ~dropped_so_far ~hook ~expect_mismatch_handler x =
  let cts = get_whole_ast_passes ~hook ~expect_mismatch_handler in
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

let map_structure_gen st ~hook ~expect_mismatch_handler =
  let st =
    if !perform_checks then begin
      Attribute.reset_checks ();
      Attribute.freshen_and_collect#structure st
    end else
      st
  in
  let st =
    apply_transforms st ~field:(fun (ct : Transform.t) -> ct.impl)
      ~dropped_so_far:Attribute.dropped_so_far_structure ~hook ~expect_mismatch_handler
  in
  if !perform_checks then begin
    Attribute.check_unused#structure st;
    Extension.check_unused#structure st;
    Attribute.check_all_seen ();
  end;
  st
;;

let map_structure st =
  map_structure_gen st ~hook:Context_free.Generated_code_hook.nop
    ~expect_mismatch_handler:Context_free.Expect_mismatch_handler.nop

let map_signature_gen sg ~hook ~expect_mismatch_handler =
  let sg =
    if !perform_checks then begin
      Attribute.reset_checks ();
      Attribute.freshen_and_collect#signature sg
    end else
      sg
  in
  let sg =
    apply_transforms sg ~field:(fun ct -> ct.intf)
      ~dropped_so_far:Attribute.dropped_so_far_signature ~hook ~expect_mismatch_handler
  in
  if !perform_checks then begin
    Attribute.check_unused#signature sg;
    Extension.check_unused#signature sg;
    Attribute.check_all_seen ();
  end;
  sg
;;

let map_signature sg =
  map_signature_gen sg ~hook:Context_free.Generated_code_hook.nop
    ~expect_mismatch_handler:Context_free.Expect_mismatch_handler.nop

let mapper =
  let module Js = Ppx_ast.Selected_ast in
  let structure _ st = Js.to_ocaml_mapper Structure map_structure st in
  let signature _ sg = Js.to_ocaml_mapper Signature map_signature sg in
  { Ocaml_common.Ast_mapper.default_mapper with structure; signature }
;;

let as_ppx_rewriter_main argv =
  let argv = Caml.Sys.executable_name :: argv in
  let usage =
    Printf.sprintf "%s [extra_args] <infile> <outfile>" exe_name
  in
  match
    Arg.parse_argv (Array.of_list argv) (Arg.align (List.rev !args))
      (fun _ -> raise (Arg.Bad "anonymous arguments not accepted"))
      usage
  with
  | exception Arg.Bad  msg -> eprintf "%s" msg; Caml.exit 2
  | exception Arg.Help msg -> eprintf "%s" msg; Caml.exit 0
  | () -> mapper

let run_as_ppx_rewriter () =
  perform_checks := false;
  Ocaml_common.Ast_mapper.run_main as_ppx_rewriter_main;
  Caml.exit 0

let string_contains_binary_ast s =
  let test magic_number =
    String.is_prefix s ~prefix:(String.sub magic_number ~pos:0 ~len:9)
  in
  test Ast_magic.ast_intf_magic_number ||
  test Ast_magic.ast_impl_magic_number

type pp_error = { filename : string; command_line : string }
exception Pp_error of pp_error

let report_pp_error ppf e =
  Caml.Format.fprintf ppf "Error while running external preprocessor@.\
                           Command line: %s@." e.command_line

let () =
  Location.Error.register_error_of_exn
    (function
      | Pp_error e ->
        Some (Location.Error.createf ~loc:(Location.in_file e.filename) "%a"
                report_pp_error e)
      | _ -> None)

let remove_no_error fn =
  try Caml.Sys.remove fn with Sys_error _ -> ()

let protectx x ~f ~finally =
  match f x with
  | v -> finally x; v
  | exception e -> finally x; raise e
;;

let with_preprocessed_file fn ~f =
  match !preprocessor with
  | None -> f fn
  | Some pp ->
    protectx (Caml.Filename.temp_file "ocamlpp" "")
      ~finally:remove_no_error
      ~f:(fun tmpfile ->
        let comm =
          Printf.sprintf "%s %s > %s"
            pp (if String.equal fn "-" then "" else Caml.Filename.quote fn)
            (Caml.Filename.quote tmpfile)
        in
        if Caml.Sys.command comm <> 0 then
          raise (Pp_error { filename = fn
                          ; command_line = comm
                          });
        f tmpfile)

let with_preprocessed_input fn ~f =
  with_preprocessed_file fn ~f:(fun fn ->
    if String.equal fn "-" then
      f stdin
    else
      In_channel.with_file fn ~f)
;;

let relocate = object
  inherit [string * string] Ast_traverse.map_with_context

  method! position (old_fn, new_fn) pos =
    if String.equal pos.pos_fname old_fn then
      { pos with pos_fname = new_fn }
    else
      pos
end

let load_input (kind : Kind.t) fn input_name ic =
  Ocaml_common.Location.input_name := input_name;
  match Migrate_parsetree.Ast_io.from_channel ic with
  | Ok (ast_input_name, ast) ->
    let ast = Intf_or_impl.of_ast_io ast in
    if not (Kind.equal kind (Intf_or_impl.kind ast)) then
      Location.raise_errorf ~loc:(Location.in_file fn)
        "File contains a binary %s AST but an %s was expected"
        (Kind.describe (Intf_or_impl.kind ast))
        (Kind.describe kind);
    if String.equal ast_input_name input_name then
      ast
    else
      Intf_or_impl.map_with_context ast relocate (ast_input_name, input_name)
  | Error (Unknown_version _) ->
    Location.raise_errorf ~loc:(Location.in_file fn)
      "File is a binary ast for an unknown version of OCaml"
  | Error (Not_a_binary_ast prefix_read_from_file) ->
    (* To test if a file is an AST file, we have to read the first few bytes of the
       file. If it is not, we have to parse these bytes and the rest of the file as
       source code.

       The compiler just does [seek_on 0] in this case, however this doesn't work when
       the input is a pipe.

       What we do instead is create a lexing buffer from the input channel and pre-fill
       it with what we read to do the test. *)
    let lexbuf = Lexing.from_channel ic in
    let len = String.length prefix_read_from_file in
    String.blit ~src:prefix_read_from_file ~src_pos:0 ~dst:lexbuf.lex_buffer ~dst_pos:0
      ~len;
    lexbuf.lex_buffer_len <- len;
    lexbuf.lex_curr_p <-
      { pos_fname = input_name
      ; pos_lnum  = 1
      ; pos_bol   = 0
      ; pos_cnum  = 0
      };
    match kind with
    | Intf -> Intf (Parse.interface      lexbuf)
    | Impl -> Impl (Parse.implementation lexbuf)
;;

let load_source_file fn =
  let s = In_channel.read_all fn in
  if string_contains_binary_ast s then
    Location.raise_errorf ~loc:(Location.in_file fn)
      "ppx_driver: cannot use -reconcile with binary AST files";
  s
;;

type output_mode =
  | Pretty_print
  | Dump_ast
  | Dparsetree
  | Reconcile of Reconcile.mode
  | Null

let process_file (kind : Kind.t) fn ~input_name ~output_mode ~embed_errors ~output =
    let replacements = ref [] in
    let expect_mismatches = ref [] in
    let add_to_list r x = r := x :: !r in
    let hook : Context_free.Generated_code_hook.t =
      match output_mode with
      | Reconcile (Using_line_directives | Delimiting_generated_blocks) ->
        { f = fun context (loc : Location.t) generated ->
            add_to_list replacements
              (Reconcile.Replacement.make ()
                 ~context:(Extension context)
                 ~start:loc.loc_start
                 ~stop:loc.loc_end
                 ~repl:generated)
        }
      | _ ->
        Context_free.Generated_code_hook.nop
    in
    let expect_mismatch_handler : Context_free.Expect_mismatch_handler.t =
      { f = fun context (loc : Location.t) generated ->
          add_to_list expect_mismatches
            (Reconcile.Replacement.make ()
               ~context:(Floating_attribute context)
               ~start:loc.loc_start
               ~stop:loc.loc_end
               ~repl:(Many generated)
               ~is_expectation:true)
      }
    in

    let ast : Intf_or_impl.t =
      try
        let ast = with_preprocessed_input fn ~f:(load_input kind fn input_name) in
        match ast with
        | Intf x -> Intf (map_signature_gen x ~hook ~expect_mismatch_handler)
        | Impl x -> Impl (map_structure_gen x ~hook ~expect_mismatch_handler)
      with exn when embed_errors ->
      match Location.Error.of_exn exn with
      | None -> raise exn
      | Some error ->
        let loc = Location.none in
        let ext = Location.Error.to_extension error in
        let open Ast_builder.Default in
        match kind with
        | Intf -> Intf [ psig_extension ~loc ext [] ]
        | Impl -> Impl [ pstr_extension ~loc ext [] ]
    in

    let null_ast =
      match ast with
      | Intf [] | Impl [] -> true
      | _ -> false
    in

    let input_contents = lazy (load_source_file fn) in
    let corrected = fn ^ ".corrected" in
    let mismatches_found =
      match !expect_mismatches with
      | [] ->
        if Caml.Sys.file_exists corrected then Caml.Sys.remove corrected;
        false
      | mismatches ->
        Reconcile.reconcile mismatches ~contents:(Lazy.force input_contents)
          ~output:(Some corrected) ~input_filename:fn ~input_name ~target:Corrected
          ?styler:!styler ~kind;
        true
    in

    (match output_mode with
     | Null -> ()
     | Pretty_print ->
       with_output output ~binary:false ~f:(fun oc ->
         let ppf = Caml.Format.formatter_of_out_channel oc in
         (match ast with
          | Intf ast -> Pprintast.signature ppf ast
          | Impl ast -> Pprintast.structure ppf ast);
         if not null_ast then Caml.Format.pp_print_newline ppf ())
     | Dump_ast ->
       with_output output ~binary:true ~f:(fun oc ->
         let ast = Intf_or_impl.to_ast_io ast in
         Migrate_parsetree.Ast_io.to_channel oc input_name ast)
     | Dparsetree ->
       with_output output ~binary:false ~f:(fun oc ->
         let ppf = Caml.Format.formatter_of_out_channel oc in
         (match ast with
          | Intf ast -> Sexp.pp_hum ppf (Ast_traverse.sexp_of#signature ast)
          | Impl ast -> Sexp.pp_hum ppf (Ast_traverse.sexp_of#structure ast));
         Caml.Format.pp_print_newline ppf ())
     | Reconcile mode ->
       Reconcile.reconcile !replacements ~contents:(Lazy.force input_contents) ~output
         ~input_filename:fn ~input_name ~target:(Output mode) ?styler:!styler
         ~kind);

    if mismatches_found then begin
      Print_diff.print () ~file1:fn ~file2:corrected ~use_color:!use_color
        ?diff_command:!diff_command;
      Caml.exit 1
    end
;;

let loc_fname = ref None
let output_mode = ref Pretty_print
let output = ref None
let kind = ref None
let input = ref None
let embed_errors = ref None
let set_input fn =
  match !input with
  | None -> input := Some fn
  | Some _ -> raise (Arg.Bad "too many input files")

let set_kind k =
  match !kind with
  | Some k' when not (Kind.equal k k') ->
    raise (Arg.Bad "must specify at most one of -impl or -intf")
  | _ -> kind := Some k
;;

let set_output_mode mode =
  match !output_mode, mode with
  | Pretty_print, _ -> output_mode := mode
  | _, Pretty_print -> assert false
  | Dump_ast   , Dump_ast
  | Dparsetree , Dparsetree -> ()
  | Reconcile a, Reconcile b when Polymorphic_compare.equal a b -> ()
  | x, y ->
    let arg_of_output_mode = function
      | Pretty_print -> assert false
      | Dump_ast                              -> "-dump-ast"
      | Dparsetree                            -> "-dparsetree"
      | Reconcile Using_line_directives       -> "-reconcile"
      | Reconcile Delimiting_generated_blocks -> "-reconcile-with-comments"
      | Null                                  -> "-null"
    in
    raise (Arg.Bad (Printf.sprintf
                      "%s and %s are incompatible"
                      (arg_of_output_mode x) (arg_of_output_mode y)))
;;

let print_transformations () =
  List.iter !Transform.all ~f:(fun (ct : Transform.t) ->
    printf "%s\n" ct.name);
;;

let parse_apply_list s =
  let names = if String.equal s "" then [] else String.split s ~on:',' in
  List.iter names ~f:(fun name ->
    if not (List.exists !Transform.all ~f:(fun (ct : Transform.t) ->
      String.equal ct.name name)) then
      raise (Caml.Arg.Bad (Printf.sprintf "code transformation '%s' does not exist" name)));
  names

type mask =
  { mutable apply      : string list option
  ; mutable dont_apply : string list option
  }

let mask =
  { apply      = None
  ; dont_apply = None
  }

let handle_apply s =
  if Option.is_some mask.apply then
    raise (Arg.Bad "-apply called too many times");
  (* This is not strictly necessary but it's more intuitive *)
  if Option.is_some mask.dont_apply then
    raise (Arg.Bad "-apply must be called before -dont-apply");
  mask.apply <- Some (parse_apply_list s)

let handle_dont_apply s =
  if Option.is_some mask.dont_apply then
    raise (Arg.Bad "-apply called too many times");
  mask.dont_apply <- Some (parse_apply_list s)

let interpret_mask () =
  if Option.is_some mask.apply || Option.is_some mask.dont_apply then begin
    let names =
      match mask.apply with
      | None -> List.map !Transform.all ~f:(fun (ct : Transform.t) -> ct.name)
      | Some names -> names
    in
    let forbidden =
      match mask.dont_apply with
      | None -> Set.empty (module String)
      | Some names -> Set.of_list (module String) names
    in
    apply_list :=
      Some (List.filter names ~f:(fun name ->
        not (Set.mem forbidden name)))
  end

let use_optcomp = ref true

let shared_args =
  [ "-loc-filename", Arg.String (fun s -> loc_fname := Some s),
    "<string> File name to use in locations"
  ; "-reserve-namespace", Arg.String Reserved_namespaces.reserve,
    "<string> Mark the given namespace as reserved"
  ; "-no-check", Arg.Clear perform_checks,
    " Disable checks (unsafe)"
  ; "-apply", Arg.String handle_apply,
    "<names> Apply these transformations in order (comma-separated list)"
  ; "-dont-apply", Arg.String handle_dont_apply,
    "<names> Exclude these transformations"
  ; "-no-merge", Arg.Set no_merge,
    " Do not merge context free transformations (better for debugging rewriters)"
  ]

let () =
  List.iter shared_args ~f:(fun (key, spec, doc) -> add_arg key spec ~doc)

let standalone_args =
  [ "-as-ppx", Arg.Unit (fun () -> raise (Arg.Bad "-as-ppx must be the first argument")),
    " Run as a -ppx rewriter (must be the first argument)"
  ; "--as-ppx", Arg.Unit (fun () -> raise (Arg.Bad "--as-ppx must be the first argument")),
    " Same as -as-ppx"
  ; "-o", Arg.String (fun s -> output := Some s),
    "<filename> Output file (use '-' for stdout)"
  ; "-", Arg.Unit (fun () -> set_input "-"),
    " Read input from stdin"
  ; "-no-optcomp", Arg.Clear use_optcomp,
    " Do not use optcomp (default if the input or output of -pp is a binary AST)"
  ; "-dump-ast", Arg.Unit (fun () -> set_output_mode Dump_ast),
    " Dump the marshaled ast to the output file instead of pretty-printing it"
  ; "--dump-ast", Arg.Unit (fun () -> set_output_mode Dump_ast),
    " Same as -dump-ast"
  ; "-dparsetree", Arg.Unit (fun () -> set_output_mode Dparsetree),
    " Print the parsetree (same as ocamlc -dparsetree)"
  ; "-embed-errors", Arg.Bool (fun x -> embed_errors := Some x),
    " Embed errors in the output AST (default: true when -dump-ast, false otherwise)"
  ; "-null", Arg.Unit (fun () -> set_output_mode Null),
    " Produce no output, except for errors"
  ; "-impl", Arg.Unit (fun () -> set_kind Impl),
    "<file> Treat the input as a .ml file"
  ; "--impl", Arg.Unit (fun () -> set_kind Impl),
    "<file> Same as -impl"
  ; "-intf", Arg.Unit (fun () -> set_kind Intf),
    "<file> Treat the input as a .mli file"
  ; "--intf", Arg.Unit (fun () -> set_kind Intf),
    "<file> Same as -intf"
  ; "-debug-attribute-drop", Arg.Set debug_attribute_drop,
    " Debug attribute dropping"
  ; "-print-transformations", Arg.Set request_print_transformations,
    " Print linked-in code transformations, in the order they are applied"
  ; "-print-passes", Arg.Set request_print_passes,
    " Print the actual passes over the whole AST in the order they are applied"
  ; "-ite-check", Arg.Set Extra_warnings.care_about_ite_branch,
    " Enforce that \"complex\" if branches are delimited (disabled if -pp is given)"
  ; "-pp", Arg.String (fun s -> preprocessor := Some s),
    "<command>  Pipe sources through preprocessor <command> (incompatible with -as-ppx)"
  ; "-reconcile", Arg.Unit (fun () -> set_output_mode (Reconcile Using_line_directives)),
    " (WIP) Pretty print the output using a mix of the input source \
     and the generated code"
  ; "-reconcile-with-comments",
    Arg.Unit (fun () -> set_output_mode (Reconcile Delimiting_generated_blocks)),
    " (WIP) same as -reconcile but uses comments to enclose the generated code"
  ; "-no-color", Arg.Clear use_color,
    " Don't use colors when printing errors"
  ; "-diff-cmd", Arg.String (fun s -> diff_command := Some s),
    " Diff command when using code expectations"
  ; "-pretty", Arg.Set pretty,
    " Instruct code generators to improve the prettiness of the generated code"
  ; "-styler", Arg.String (fun s -> styler := Some s),
    " Code styler"
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
  Arg.parse (Arg.align args) set_input usage;
  interpret_mask ();
  if !request_print_transformations then begin
    print_transformations ();
    Caml.exit 0;
  end;
  if !request_print_passes then begin
    print_passes ();
    Caml.exit 0;
  end;
  if !use_optcomp then Lexer.set_preprocessor Optcomp.init Optcomp.map_lexer;
  match !input with
  | None    ->
    eprintf "%s: no input file given\n%!" exe_name;
    Caml.exit 2
  | Some fn ->
    let kind =
      match !kind with
      | Some k -> k
      | None ->
        match Kind.of_filename fn with
        | Some k -> k
        | None ->
          eprintf "%s: don't know what to do with '%s', use -impl or -intf.\n"
            exe_name fn;
          Caml.exit 2
    in
    let input_name =
      match !loc_fname with
      | None    -> fn
      | Some fn -> fn
    in
    let embed_errors =
      match !embed_errors with
      | Some x -> x
      | None ->
        match !output_mode with
        | Dump_ast -> true
        | _        -> false
    in
    process_file kind fn ~input_name ~output_mode:!output_mode ~output:!output
      ~embed_errors
;;

let standalone_run_as_ppx_rewriter () =
  let n = Array.length Caml.Sys.argv in
  let usage = Printf.sprintf "%s -as-ppx [extra_args] <infile> <outfile>" exe_name in
  if n < 4 then begin
    eprintf "Usage: %s\n%!" usage;
    Caml.exit 2
  end;
  let argv = Array.create ~len:(n - 3) "" in
  argv.(0) <- Caml.Sys.argv.(0);
  for i = 1 to (n - 4) do
    argv.(i) <- Caml.Sys.argv.(i + 1)
  done;
  let standalone_args =
    List.map standalone_args ~f:(fun (arg, spec, _doc) ->
      (arg, spec, " Unused with -as-ppx"))
  in
  let args = List.rev_append !args standalone_args in
  match
    Arg.parse_argv argv (Arg.align args)
      (fun _ -> raise (Arg.Bad "anonymous arguments not accepted"))
      usage
  with
  | exception Arg.Bad  msg -> eprintf "%s" msg; Caml.exit 2
  | exception Arg.Help msg -> eprintf "%s" msg; Caml.exit 0
  | () ->
    interpret_mask ();
    Ocaml_common.Ast_mapper.apply
      ~source:Caml.Sys.argv.(n - 2) ~target:Caml.Sys.argv.(n - 1) mapper
;;

let standalone () =
  try
    if Array.length Caml.Sys.argv >= 2 &&
       match Caml.Sys.argv.(1) with
       | "-as-ppx" | "--as-ppx" -> true
       | _ -> false
    then
      standalone_run_as_ppx_rewriter ()
    else
      standalone_main ();
    Caml.exit 0
  with exn ->
    Location.report_exception Caml.Format.err_formatter exn;
    Caml.exit 1
;;

let pretty () = !pretty

let () =
  (* Register ppx_driver as a single Migrate_driver rewriter so that ppx_driver based
     rewriters can be used with ocaml-migrate-parsetree.driver-main *)
  let mapper =
    let module A = Ppx_ast.Selected_ast.Ast.Ast_mapper in
    let structure _ st = map_structure st in
    let signature _ sg = map_signature sg in
    { A.default_mapper with structure; signature }
  in
  Migrate_parsetree.Driver.register
    ~name:"ppx_driver"
    (* This doesn't take arguments registered by rewriters. It's not worth supporting
       them, since [--cookie] is a much better replacement for passing parameters to
       individual rewriters. *)
    ~args:shared_args
    (module Ppx_ast.Selected_ast)
    (fun _config _cookie -> mapper)
