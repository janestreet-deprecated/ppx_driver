let package_name = "ppx_driver"

let sections =
  [ ("lib",
    [ ("built_lib_ppx_driver", None)
    ; ("built_lib_ppx_driver_ocamlbuild", None)
    ; ("built_lib_ppx_driver_runner", None)
    ; ("built_lib_ppx_driver_runner_as_ppx", None)
    ],
    [ ("META", None)
    ])
  ]
