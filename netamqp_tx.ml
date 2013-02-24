(* $Id: netamqp_tx.ml 53444 2011-03-10 14:08:13Z gerd $ *)

open Netamqp_types
open Netamqp_rtypes
open Netamqp_endpoint
open Uq_engines.Operators


let select_e ~channel () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    channel
    (`AMQP_0_9 `Tx_select)
    None
  ++ (fun _ -> eps_e (`Done()) esys)	


let select_s ~channel () = sync (select_e ~channel) ()
    

let commit_e ~channel () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    channel
    (`AMQP_0_9 `Tx_commit)
    None
  ++ (fun _ -> eps_e (`Done()) esys)	
    

let commit_s ~channel () = sync (commit_e ~channel) ()


let rollback_e ~channel () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    channel
    (`AMQP_0_9 `Tx_rollback)
    None
  ++ (fun _ -> eps_e (`Done()) esys)	
    

let rollback_s ~channel () = sync (rollback_e ~channel) ()
