open V1_LWT
open Lwt
open Printf
open Mirage

let packets_in = ref 0l
let packets_waiting = ref 0l

let max_intfs = 3

type 'a interface = {
  port_no : int;
  name : string;
  nic : Netif.t;
  mac : Netif.macaddr;
  in_queue : 'a Lwt_stream.t;
  in_push : 'a option -> unit;
  out_queue : 'a Lwt_stream.t;
  out_push : 'a option -> unit;
}

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
    }

  let nic_listen nic =
    let hw_addr = Macaddr.to_string nic.mac in
    let _ = printf "listening on the interface with mac address '%s' \n%!" hw_addr in
    Netif.listen nic.nic (fun frame -> return (nic.in_push (Some frame)))

  let update_packet_count () =
    let _ = packets_in := Int32.succ !packets_in in
    packets_waiting := Int32.succ !packets_waiting
    (*
    let _ = packets_waiting := Int32.succ !packets_waiting in
    if (Int32.logand !packets_in 0xfl) = 0l then
      let _ = printf "packets (in = %ld) (not forwarded = %ld)" !packets_in !packets_waiting in
      print_endline ""*)

  let flood_packet in_nic frame out_nic =
    (*let _ = printf "in_port: %d, out_port: %d\n" in_nic.port_no out_nic.port_no in*)
    if in_nic.port_no != out_nic.port_no then
      (*let _ = printf "flooding to port %d\n" out_nic.port_no in*)
      return (out_nic.out_push frame)
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
    let make_in_thread nics_list nic =
      while_lwt true do
        lwt _ = Lwt_stream.next nic.in_queue >>= fun frame ->
          Lwt_list.iter_p (flood_packet nic (Some frame)) nics_list in
          return (update_packet_count ())
      done in
    let make_fwd_thread nic =
      while_lwt true do
        lwt frame = Lwt_stream.next nic.out_queue in
        let _ = packets_waiting := Int32.pred !packets_waiting in
        (*let _ = printf "Sending packet out port: %d\n" nic.port_no in*)
        Netif.write nic.nic frame
      done in
    let forward_thread nics_list =
      choose [Lwt_list.iter_p (make_in_thread nics_list) nics_list;
              Lwt_list.iter_p make_fwd_thread nics_list]
    in
    detect_nics 1 >>
    let nics_list = Array.to_list nics in
    choose [Lwt_list.iter_p nic_listen nics_list;
            forward_thread nics_list]
    >> return (print_endline "terminated.")
end
