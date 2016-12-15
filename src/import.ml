let protectx x ~finally ~f =
  match f x with
  | y           -> finally x; y
  | exception e -> finally x; raise e
;;

let with_output fn ~binary ~f =
  match fn with
  | None | Some "-" -> f stdout
  | Some fn ->
    protectx (if binary then open_out_bin fn else open_out fn)
      ~finally:close_out ~f
;;
