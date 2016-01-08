open V1_LWT
open Lwt
open Printf

let packets_in = ref 0l
let packets_waiting = ref 0l

(*
       functor
         (C : V1_LWT.CONSOLE) (N1 : V1_LWT.NETWORK) (N2 : V1_LWT.NETWORK) ->
         sig
           val n1_queues : N1.buffer intf_queues
           val n2_queues : N2.buffer intf_queues
           val queues : '_a intf_queues list
           val n1_listen : N1.t -> unit N1.io
           val n2_listen : N2.t -> unit N2.io
           val update_packet_count : unit -> unit
           val start : 'a -> N1.t -> N2.t -> unit Lwt.t
         end,
 *)


module Main (C: CONSOLE)(NET0: NETWORK) = struct

  type 'a interface = {
    nic : Netif.t;
    mac : Netif.macaddr;
    in_queue : 'a Lwt_stream.t;
    in_push : 'a option -> unit;
    out_queue : 'a Lwt_stream.t;
    out_push : 'a option -> unit;
  }

  let max_intfs = 3

  let make_intf_queues nic =
    let (in_queue, in_push) = Lwt_stream.create () in
    let (out_queue, out_push) = Lwt_stream.create () in
    {
        nic = nic;
        mac = Netif.mac nic;
        in_queue = in_queue;
        in_push = in_push;
        out_queue = out_queue;
        out_push = out_push;
    }

  let nic_listen intf =
    let hw_addr =  Macaddr.to_string intf.mac in
    let _ = printf "N1: listening on the interface with mac address '%s' \n%!" hw_addr in
    Netif.listen intf.nic (fun frame -> return (intf.in_push (Some frame)))

  let update_packet_count () =
    let _ = packets_in := Int32.succ !packets_in in
    let _ = packets_waiting := Int32.succ !packets_waiting in
    if (Int32.logand !packets_in 0xfl) = 0l then
      let _ = printf "packets (in = %ld) (not forwarded = %ld)" !packets_in !packets_waiting in
      print_endline ""

  let flood_packet nics in_port frame =
    let flood frame i nic =
      if i != in_port then
        nic.out_push frame
    in
    Array.iteri (flood frame) nics

  let start console nic0 =
    let nics = Array.make max_intfs (make_intf_queues nic0) in
      let rec detect_nics i = 
        if i < max_intfs then
          (Netif.connect (string_of_int i)) >>= function
          | `Error e -> fail (Failure ("net_" ^ (string_of_int i)))
          | `Ok nic -> Array.set nics i (make_intf_queues nic);
          detect_nics (i+1)
        else
          return ()
      in
      let forward_thread intf0 intf1 =
        choose [
          while_lwt true do
              lwt _ = Lwt_stream.next intf0.in_queue >>= fun frame ->
                  return (flood_packet nics 0 (Some frame)) in
              return (update_packet_count ())
          done
          ;
          while_lwt true do
              lwt _ = Lwt_stream.next intf1.in_queue >>= fun frame ->
                  return (flood_packet nics 1 (Some frame)) in
              return (update_packet_count ())
          done
          ;
          while_lwt true do
              lwt frame = Lwt_stream.next intf0.out_queue in
                  let _ = packets_waiting := Int32.pred !packets_waiting in
                  Netif.write intf0.nic frame
          done
          ;
          while_lwt true do
              lwt frame = Lwt_stream.next intf1.out_queue in
                  let _ = packets_waiting := Int32.pred !packets_waiting in
                  Netif.write intf1.nic frame
          done
        ]
      in
      detect_nics 0 >>
      choose [(nic_listen nics.(0));
              (nic_listen nics.(1));
              (forward_thread nics.(0) nics.(1))]
      >> return (print_endline "terminated.")
end
