open V1_LWT
open Lwt
open Printf
open Mirage

let packets_in = ref 0l
let packets_waiting = ref 0l

let max_intfs = 3

type eth_frame = {
  in_port: int;
  typ: Wire_structs.ethertype option;
  src: string;
  dst: string;
  payload: Cstruct.t;
}

type interface = {
  port_no : int;
  name : string;
  nic : Netif.t;
  mac : Netif.macaddr;
  in_queue : (Netif.buffer * eth_frame option) Lwt_stream.t;
  in_push : (Netif.buffer * eth_frame option) option -> unit;
  out_queue : Netif.buffer Lwt_stream.t;
  out_push : Netif.buffer option -> unit;
  packets_in : int32 ref;
  packets_out : int32 ref;
}

let parse_ethernet_frame frame in_port =
  if Cstruct.len frame >= 14 then
    (* source + destination + type = 14 *)
    let payload = Cstruct.shift frame Wire_structs.sizeof_ethernet
    and typ = Wire_structs.int_to_ethertype (Wire_structs.get_ethernet_ethertype frame)
    and src = Macaddr.to_string ~sep:':' (Macaddr.of_bytes_exn (Wire_structs.copy_ethernet_src frame))
    and dst = Macaddr.to_string ~sep:':' (Macaddr.of_bytes_exn (Wire_structs.copy_ethernet_dst frame))
    in
    Some {in_port = in_port; typ=typ; src=src; dst=dst; payload=payload}
  else
    None

module Switch = struct

  module MacSet = Set.Make(String)

  type t = {
    mac_intf : (string, int) Hashtbl.t;
    intf_mac : MacSet.t array;
    all_intfs : int list;
    }

  let make intf_list =
    let intf_cnt = List.length intf_list in
    {
      mac_intf = Hashtbl.create intf_cnt;
      intf_mac = Array.make intf_cnt MacSet.empty;
      all_intfs = intf_list;
    }

  let decide state parsed_frame =
    let _ = match Hashtbl.mem state.mac_intf parsed_frame.src with
    | false -> Hashtbl.add state.mac_intf parsed_frame.src parsed_frame.in_port;
               let new_set = MacSet.add parsed_frame.src state.intf_mac.(parsed_frame.in_port) in
               state.intf_mac.(parsed_frame.in_port) <- new_set;
    | true -> () in
    try [ Hashtbl.find state.mac_intf parsed_frame.dst ]
    with Not_found -> state.all_intfs
end

module Main (C: CONSOLE)(NET0: NETWORK) = struct

  let make_intf_queues port_no name nic =
    let (in_queue, in_push) = Lwt_stream.create () in
    let (out_queue, out_push) = Lwt_stream.create () in
    {
      port_no = port_no;
      name = name;
      nic = nic;
      mac = Netif.mac nic;
      in_queue = in_queue;
      in_push = in_push;
      out_queue = out_queue;
      out_push = out_push;
      packets_in = ref 0l;
      packets_out = ref 0l;
    }

  let nic_listen nic =
    let hw_addr = Macaddr.to_string nic.mac in
    let _ = printf "listening on the interface with mac address '%s' \n%!" hw_addr in
    Netif.listen nic.nic (fun frame -> return (nic.in_push (Some (frame, (parse_ethernet_frame frame nic.port_no)))))

  let update_packet_count () =
    let _ = packets_in := Int32.succ !packets_in in
    packets_waiting := Int32.succ !packets_waiting
    (*
    let _ = packets_waiting := Int32.succ !packets_waiting in
    if (Int32.logand !packets_in 0xfl) = 0l then
      let _ = printf "packets (in = %ld) (not forwarded = %ld)" !packets_in !packets_waiting in
      print_endline ""*)

  let forward_packet in_nic frame out_nic =
    (*let _ = printf "in_port: %d, out_port: %d\n" in_nic.port_no out_nic.port_no in*)
    if in_nic.port_no != out_nic.port_no then
      (*let _ = printf "flooding to port %d\n" out_nic.port_no in*)
      return (out_nic.out_push (Some frame))
    else
      (*let _ = printf "not flooding to port %d\n" out_nic.port_no in*)
      return ()

  let start console nic0 =
    let nics = Array.make max_intfs (make_intf_queues 0 "tap0" nic0) in
      let rec detect_nics i = 
        if i < max_intfs then
          let nic_name = "tap" ^ (string_of_int i) in
          (Netif.connect nic_name) >>= function
          | `Error e -> fail (Failure ("net_" ^ (string_of_int i)))
          | `Ok nic -> Array.set nics i (make_intf_queues i nic_name nic);
          detect_nics (i+1)
        else
          return ()
      in
    let make_in_thread nics_list switch nic =
      while_lwt true do
        lwt _ = Lwt_stream.next nic.in_queue >>= fun (frame, _) ->
          let out_ports = Switch.decide switch eth_frame in
          Lwt_list.iter_p (forward_packet nic frame) out_ports in
          return (update_packet_count ())
      done in
    let make_fwd_thread nic =
      while_lwt true do
        lwt frame = Lwt_stream.next nic.out_queue in
        let _ = packets_waiting := Int32.pred !packets_waiting in
        (*let _ = printf "Sending packet out port: %d\n" nic.port_no in*)
        Netif.write nic.nic frame
      done in
    let forward_thread nics_list switch =
      choose [Lwt_list.iter_p (make_in_thread nics_list switch) nics_list;
              Lwt_list.iter_p make_fwd_thread nics_list]
    in
    detect_nics 1 >>
    let nics_list = Array.to_list nics in
    let switch = Switch.make (List.map (fun n -> n.port_no) nics_list) in
    choose [Lwt_list.iter_p nic_listen nics_list;
            forward_thread nics_list switch]
    >> return (print_endline "terminated.")
end
