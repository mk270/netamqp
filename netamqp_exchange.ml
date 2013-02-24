(* $Id: netamqp_exchange.ml 53444 2011-03-10 14:08:13Z gerd $ *)

open Netamqp_types
open Netamqp_rtypes
open Netamqp_endpoint
open Uq_engines.Operators

type exchange_name = string
type exchange_type = string

let amq_direct = "amq.direct"
let amq_fanout = "amq.fanout"
let amq_topic = "amq.topic"
let amq_match = "amq.match"

let direct = "direct"
let fanout = "fanout"
let topic = "topic"
let headers = "headers"
let system = "system"

let declare_passively_e ~channel ~exchange_name ?(no_wait=false) () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    ?no_wait:(if no_wait then Some (`AMQP_0_9 `Exchange_declare_ok) else None)
    channel
    (`AMQP_0_9
       (`Exchange_declare
	  (0, exchange_name, "", true, false, false, false, no_wait, [])))
    None
  ++ (fun (m, _) ->
	eps_e (`Done()) esys
     )

let declare_passively_s ~channel ~exchange_name ?no_wait () = 
  sync (declare_passively_e ~channel ~exchange_name ?no_wait) ()


let declare_e ~channel ~exchange_name ~exchange_type ?(durable=false)
              ?(no_wait=false) ?(arguments=[]) () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    ?no_wait:(if no_wait then Some (`AMQP_0_9 `Exchange_declare_ok) else None)
    channel
    (`AMQP_0_9
       (`Exchange_declare
	  (0, exchange_name, exchange_type, false, durable, false, false, 
	   no_wait, arguments)))
    None
  ++ (fun (m, _) ->
	eps_e (`Done()) esys
     )


let declare_s ~channel ~exchange_name ~exchange_type ?durable ?no_wait
              ?arguments () =
  sync(declare_e ~channel ~exchange_name ~exchange_type ?durable ?no_wait
	 ?arguments) ()


let delete_e ~channel ~exchange_name ?(if_unused=false) ?(no_wait=false) () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    ?no_wait:(if no_wait then Some (`AMQP_0_9 `Exchange_delete_ok) else None)
    channel
    (`AMQP_0_9
       (`Exchange_delete
	  (0, exchange_name, if_unused, no_wait)))
    None
  ++ (fun (m, _) ->
	eps_e (`Done()) esys
     )


let delete_s ~channel ~exchange_name ?if_unused ?no_wait () =
  sync(delete_e ~channel ~exchange_name ?if_unused ?no_wait) ()
