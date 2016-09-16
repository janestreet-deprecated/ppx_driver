let inside_base = ref false

let () =
  Ppx_driver.add_arg "-inside-base" (Arg.Set inside_base)
    ~doc:" Whether the preprocessors are running inside the Base library or not.";
;;

let get () = !inside_base
