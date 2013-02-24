(* $Id: netamqp_queue.ml 53444 2011-03-10 14:08:13Z gerd $ *)

open Netamqp_types
open Netamqp_rtypes
open Netamqp_endpoint
open Uq_engines.Operators

type queue_name = string

type 'a declare_result =
    out:( queue_name:queue_name ->
          message_count:Rtypes.uint4 ->
	  consumer_count:Rtypes.uint4 ->
            'a) ->
    unit ->
      'a


let uint4_0 =
  Rtypes.uint4_of_int 0

let declare_passively_e ~channel ~queue ?(no_wait=false) () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    ?no_wait:(if no_wait then
		Some (`AMQP_0_9 (`Queue_declare_ok("",uint4_0, uint4_0 )))
	      else None)
    channel
    (`AMQP_0_9
       (`Queue_declare
	  (0, queue, true, false, false, false, no_wait, [])))
    None
  ++ (fun (m, _) ->
	match m with
	  | `AMQP_0_9 (`Queue_declare_ok(qn, mc, cc)) ->
	      let r ~out () =
		out ~queue_name:qn ~message_count:mc ~consumer_count:cc in
	      eps_e (`Done r) esys
	  | _ ->
	      assert false
     )

let declare_passively_s ~channel ~queue ?no_wait () =
  sync(declare_passively_e ~channel ~queue ?no_wait) ()


let declare_e ~channel ~queue  ?(durable=false) ?(exclusive=false)
              ?(auto_delete=false) ?(no_wait=false) ?(arguments=[]) () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    ?no_wait:(if no_wait then
		Some (`AMQP_0_9 (`Queue_declare_ok("",uint4_0, uint4_0 )))
	      else None)
    channel
    (`AMQP_0_9
       (`Queue_declare
	  (0, queue, false, durable, exclusive, auto_delete, no_wait, [])))
    None
  ++ (fun (m, _) ->
	match m with
	  | `AMQP_0_9 (`Queue_declare_ok(qn, mc, cc)) ->
	      let r ~out () =
		out ~queue_name:qn ~message_count:mc ~consumer_count:cc in
	      eps_e (`Done r) esys
	  | _ ->
	      assert false
     )


let declare_s ~channel ~queue  ?durable ?exclusive ?auto_delete 
              ?no_wait ?arguments () =
  sync (declare_e ~channel ~queue  ?durable ?exclusive ?auto_delete 
          ?no_wait ?arguments) ()


let bind_e ~channel ~queue ~exchange ~routing_key ?(no_wait=false)
           ?(arguments=[]) () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    ?no_wait:(if no_wait then Some (`AMQP_0_9 `Queue_bind_ok) else None)
    channel
    (`AMQP_0_9
       (`Queue_bind
	  (0, queue, exchange, routing_key, no_wait, arguments)))
    None
  ++ (fun (m, _) ->
	match m with
	  | `AMQP_0_9 `Queue_bind_ok ->
	      eps_e (`Done ()) esys
	  | _ ->
	      assert false
     )


let bind_s ~channel ~queue ~exchange ~routing_key ?no_wait ?arguments () =
  sync(bind_e ~channel ~queue ~exchange ~routing_key ?no_wait ?arguments) ()


let unbind_e ~channel ~queue ~exchange ~routing_key ?(arguments=[]) () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    channel
    (`AMQP_0_9
       (`Queue_unbind
	  (0, queue, exchange, routing_key, arguments)))
    None
  ++ (fun (m, _) ->
	match m with
	  | `AMQP_0_9 `Queue_unbind_ok ->
	      eps_e (`Done ()) esys
	  | _ ->
	      assert false
     )


let unbind_s ~channel ~queue ~exchange ~routing_key ?arguments () =
  sync(unbind_e ~channel ~queue ~exchange ~routing_key ?arguments) ()


let purge_e ~channel ~queue ?(no_wait=false) () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    ?no_wait:(if no_wait then Some (`AMQP_0_9 (`Queue_purge_ok uint4_0))
	      else None)
    channel
    (`AMQP_0_9
       (`Queue_purge
	  (0, queue, no_wait)))
    None
  ++ (fun (m, _) ->
	match m with
	  | `AMQP_0_9 (`Queue_purge_ok mc) ->
	      eps_e (`Done mc) esys
	  | _ ->
	      assert false
     )


let purge_s ~channel ~queue ?no_wait () =
  sync(purge_e ~channel ~queue ?no_wait) ()



let delete_e ~channel ~queue ?(if_unused=false) ?(if_empty=false) 
             ?(no_wait=false) () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    ?no_wait:(if no_wait then Some (`AMQP_0_9 (`Queue_delete_ok uint4_0))
	      else None)
    channel
    (`AMQP_0_9
       (`Queue_delete
	  (0, queue, if_unused, if_empty, no_wait)))
    None
  ++ (fun (m, _) ->
	match m with
	  | `AMQP_0_9 (`Queue_delete_ok mc) ->
	      eps_e (`Done (mc : Rtypes.uint4)) esys
	  | _ ->
	      assert false
     )


let delete_s ~channel ~queue ?if_unused ?if_empty ?no_wait () =
  sync (delete_e ~channel ~queue ?if_unused ?if_empty ?no_wait) ()
