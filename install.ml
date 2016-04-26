#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"ppx_driver"
  [ oasis_lib "ppx_driver"
  ; oasis_lib "ppx_driver_ocamlbuild"
  ; oasis_lib "ppx_driver_runner"
  ; oasis_lib "ppx_driver_runner_as_ppx"
  ; file "META" ~section:"lib"
  ]
