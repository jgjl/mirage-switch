open Mirage

let main = foreign "Unikernel.Main" (console @-> network @->  job)

let unix_libs =
  match get_mode () with
  | `Unix -> ["mirage-clock-unix"; "mirage-net-unix"; "mirage-unix"]
  | `MacOSX -> ["mirage-clock-unix"; "mirage-net-macosx"; "mirage-unix"]
  | _ -> ["mirage-net-xen"; "mirage-xen"]


let () =
  let libs = [ "tcpip.ethif"; "fieldslib"; "fieldslib.syntax"; "mirage" ] @ unix_libs in
  let packs = [ "tcpip"; "fieldslib" ] @ unix_libs in
  add_to_ocamlfind_libraries (libs);
  add_to_opam_packages (packs);

  register "network" [
    main $ default_console $ (netif "tap0")
  ]
