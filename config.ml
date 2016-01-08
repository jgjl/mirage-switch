open Mirage

let main = foreign "Unikernel.Main" (console @-> network @->  job)

let unix_libs =
  match get_mode () with
  | `Unix -> ["mirage-clock-unix"; "mirage-net-unix"; "mirage-unix"]
  | `MacOSX -> ["mirage-clock-unix"; "mirage-net-macosx"; "mirage-unix"]
  | _ -> ["mirage-net-xen"; "mirage-xen"]


let () =
  add_to_ocamlfind_libraries
    ([ "tcpip.ethif"; "fieldslib"; "fieldslib.syntax"; "mirage" ] @ unix_libs);

  register "network" [
    (*main $ default_console $ (netif "0") $ (netif "1")*)
    main $ default_console $ (netif "tap0")
  ]
