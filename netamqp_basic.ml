(* $Id: netamqp_basic.ml 53444 2011-03-10 14:08:13Z gerd $ *)

open Netamqp_types
open Netamqp_rtypes
open Netamqp_endpoint
open Uq_engines.Operators

class type message =
object
  method content_type : string option
  method content_encoding : string option
  method headers : table option
  method delivery_mode : int option
  method priority : int option
  method correlation_id : string option
  method reply_to : string option
  method expiration : string option
  method message_id : string option
  method timestamp : float option
  method typ : string option
  method user_id : string option
  method app_id : string option
  method amqp_header : Netamqp_endpoint.props_t
  method amqp_body : Xdr_mstring.mstring list
end

type 'a get_message =
  out:( delivery_tag : Rtypes.uint8 ->
        redelivered : bool ->
        exchange : Netamqp_exchange.exchange_name ->
        routing_key:string ->
        message_count:Rtypes.uint4 ->
        message ->
         'a) ->
  unit ->
    'a

type 'a get_result =
  [ `Message of 'a get_message | `Empty ]


let create_message 
    ?content_type ?content_encoding ?headers ?delivery_mode ?priority
    ?correlation_id ?reply_to ?expiration ?message_id ?timestamp ?typ
    ?user_id ?app_id amqp_body : message =
  let (amqp_header : props_t) =
    `AMQP_0_9
      (`P_basic
	 (content_type,content_encoding,headers,delivery_mode,priority,
	  correlation_id,reply_to,expiration,message_id,timestamp,typ,
	  user_id,app_id,None)
      ) in
object
  method content_type = content_type
  method content_encoding = content_encoding
  method headers = headers
  method delivery_mode = delivery_mode
  method priority = priority
  method correlation_id = correlation_id
  method reply_to = reply_to
  method expiration = expiration
  method message_id = message_id
  method timestamp = timestamp
  method typ = typ
  method user_id = user_id
  method app_id = app_id
  method amqp_header = amqp_header
  method amqp_body = amqp_body
end


let restore_message amqp_header amqp_body : message =
  match amqp_header with
    | `AMQP_0_9(`P_basic(content_type,content_encoding,headers,delivery_mode,
			 priority,correlation_id,reply_to,expiration,
			 message_id,timestamp,typ,user_id,app_id,_)) ->
	( object
	    method content_type = content_type
	    method content_encoding = content_encoding
	    method headers = headers
	    method delivery_mode = delivery_mode
	    method priority = priority
	    method correlation_id = correlation_id
	    method reply_to = reply_to
	    method expiration = expiration
	    method message_id = message_id
	    method timestamp = timestamp
	    method typ = typ
	    method user_id = user_id
	    method app_id = app_id
	    method amqp_header = amqp_header
	    method amqp_body = amqp_body
	  end
	)

    | _ ->
	failwith "Netamqp_basic.restore_message"



let qos_e ~channel ?(prefetch_size=0) ?(prefetch_count=0) ?(global=false) () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    channel
    (`AMQP_0_9
       (`Basic_qos
	  (Rtypes.uint4_of_int prefetch_size,
	   prefetch_count,
	   global
	  )))
    None
  ++ (fun (m, _) ->
	match m with
	  | `AMQP_0_9 `Basic_qos_ok ->
	      eps_e (`Done ()) esys
	  | _ ->
	      assert false
     )


let qos_s ~channel ?prefetch_size ?prefetch_count ?global () =
  sync (qos_e ~channel ?prefetch_size ?prefetch_count ?global) ()


let consume_e ~channel ~queue ?(consumer_tag="") ?(no_local=false)
              ?(no_ack=false) ?(exclusive=false) ?(no_wait=false)
	      ?(arguments=[]) () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    ?no_wait:(if no_wait then Some (`AMQP_0_9 (`Basic_consume_ok consumer_tag))
	      else None)
    channel
    (`AMQP_0_9
       (`Basic_consume
	  (0, queue, consumer_tag, no_local, no_ack, exclusive, no_wait,
	   arguments)))
    None
  ++ (fun (m, _) ->
	match m with
	  | `AMQP_0_9 (`Basic_consume_ok ctag) ->
	      eps_e (`Done ctag) esys
	  | _ ->
	      assert false
     )


let consume_s ~channel ~queue ?consumer_tag ?no_local ?no_ack ?exclusive
              ?no_wait ?arguments () =
  sync (consume_e
	  ~channel ~queue ?consumer_tag ?no_local ?no_ack ?exclusive
	  ?no_wait ?arguments) ()


let cancel_e ~channel ~consumer_tag ?(no_wait=false) () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    ?no_wait:(if no_wait then Some (`AMQP_0_9 (`Basic_cancel_ok consumer_tag))
	      else None)
    channel
    (`AMQP_0_9
       (`Basic_cancel (consumer_tag, no_wait)))
    None
  ++ (fun (m, _) ->
	match m with
	  | `AMQP_0_9 (`Basic_cancel_ok ctag) ->
	      eps_e (`Done ctag) esys
	  | _ ->
	      assert false
     )


let cancel_s ~channel ~consumer_tag ?no_wait () =
  sync (cancel_e ~channel ~consumer_tag ?no_wait) ()


let publish_e ~channel ~exchange ~routing_key ?(mandatory=false) 
              ?(immediate=false) msg =
  let ep = Netamqp_channel.endpoint channel in
  let ch = Netamqp_channel.number channel in
  async_c2s_e
    ep
    (`AMQP_0_9
       (`Basic_publish
	  (0, exchange, routing_key, mandatory, immediate))) 
    (Some(msg#amqp_header, msg#amqp_body))
    ch


let publish_s ~channel ~exchange ~routing_key ?mandatory ?immediate msg =
  sync (publish_e ~channel ~exchange ~routing_key ?mandatory ?immediate) msg


let on_return ~channel ~cb () =
  let ep = Netamqp_channel.endpoint channel in
  let ch = Netamqp_channel.number channel in
  register_async_s2c
    ep
    (`AMQP_0_9 `Basic_return)
    ch
    (fun m data_opt ->
       match m with
	 | `AMQP_0_9 (`Basic_return(code, text, exchange, routing_key)) ->
	     ( match data_opt with
		 | None -> 
		     failwith "Netamqp_basic.on_return: No data"
		 | Some (hdr,body) ->
		     let msg = restore_message hdr body in
		     cb 
		       ~reply_code:code
		       ~reply_text:text
		       ~exchange
		       ~routing_key
		       msg
	     )
	 | _ ->
	     assert false
    )


let on_deliver ~channel ~cb () =
  let ep = Netamqp_channel.endpoint channel in
  let ch = Netamqp_channel.number channel in
  register_async_s2c
    ep
    (`AMQP_0_9 `Basic_deliver)
    ch
    (fun m data_opt ->
       match m with
	 | `AMQP_0_9 (`Basic_deliver(ctag, dtag, redelivered, exchange, 
				     routing_key)) ->
	     ( match data_opt with
		 | None -> 
		     failwith "Netamqp_basic.on_deliver: No data"
		 | Some (hdr,body) ->
		     let msg = restore_message hdr body in
		     cb
		       ~consumer_tag:ctag
		       ~delivery_tag:dtag
		       ~redelivered
		       ~exchange
		       ~routing_key
		       msg
	     )
	 | _ ->
	     assert false
    )

		     
let get_e ~channel ~queue ?(no_ack=false) () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    channel
    (`AMQP_0_9
       (`Basic_get
	  (0, queue, no_ack)))
    None
  ++ (fun (m, data_opt) ->
	match m with
	  | `AMQP_0_9 (`Basic_get_ok(dtag, redelivered, exchange, routing_key,
				     message_count))->
	      let msg =
		match data_opt with
		  | None -> 
		      failwith "Netamqp_basic.get_e: No data"
		  | Some (hdr,body) ->
		      restore_message hdr body in
	      let f ~out () =
		out 
		  ~delivery_tag:dtag
		  ~redelivered
		  ~exchange
		  ~routing_key
		  ~message_count
		  msg in
	      eps_e (`Done (`Message f)) esys
	  | `AMQP_0_9 (`Basic_get_empty _) ->
	      eps_e (`Done `Empty) esys
	  | _ ->
	      assert false
     )

let get_s ~channel ~queue ?no_ack () =
  sync (get_e ~channel ~queue ?no_ack) ()



let ack_e ~channel ~delivery_tag ?(multiple=false) () =
  let ep = Netamqp_channel.endpoint channel in
  let ch = Netamqp_channel.number channel in
  async_c2s_e
    ep
    (`AMQP_0_9
       (`Basic_ack
	  (delivery_tag, multiple)))
    None
    ch


let ack_s ~channel ~delivery_tag ?multiple () =
  sync (ack_e ~channel ~delivery_tag ?multiple) ()

let reject_e ~channel ~delivery_tag ~requeue () =
  let ep = Netamqp_channel.endpoint channel in
  let ch = Netamqp_channel.number channel in
  async_c2s_e
    ep
    (`AMQP_0_9
       (`Basic_reject
	  (delivery_tag, requeue)))
    None
    ch

let reject_s ~channel ~delivery_tag ~requeue () =
  sync (reject_e ~channel ~delivery_tag ~requeue) ()


let recover_e ~channel ~requeue () =
  let esys = Netamqp_channel.event_system channel in
  Netamqp_channel.sync_c2s_e
    channel
    (`AMQP_0_9
       (`Basic_recover requeue))
    None
  ++ (fun (m, _) ->
	match m with
	  | `AMQP_0_9 `Basic_recover_ok ->
	      eps_e (`Done ()) esys
	  | _ ->
	      assert false
     )


let recover_s ~channel ~requeue () =
  sync(recover_e ~channel ~requeue) ()
