(* $Id: netamqp_channel.ml 53444 2011-03-10 14:08:13Z gerd $ *)

open Netamqp_types
open Netamqp_endpoint
open Uq_engines.Operators
open Printf

exception Not_open
exception Error of string

type channel = int

type channel_obj =
    { c : Netamqp_connection.connection;
      ep : Netamqp_endpoint.endpoint;
      ch : int;
      mutable tmo : float;
    }

let default_tmo = 60.0

let open_e c ch =
  let ep = Netamqp_connection.endpoint c in
  if not (Netamqp_connection.is_open c) then
    raise(Netamqp_connection.Not_open);

  (*
  if is_channel_enabled ep ch then
    failwith "Netamqp_channel.open_e: Channel is already open";
   *)
  
  let ch_obj =
    { c = c; ep = ep; ch = ch; tmo = default_tmo } in
  let esys =
    event_system ep in

  enable_channel ep ch;

  register_sync_s2c ep
    (`AMQP_0_9 `Channel_close)
    ch
    (fun m ->
       match m with
	 | `AMQP_0_9(`Channel_close(reply_code, reply_text,
				    class_id, meth_id)) ->
	     let msg =
	       sprintf "channel-close from server: \
                          code=%d text=%s class-id=%d meth-id=%d"
		 reply_code reply_text class_id meth_id in
	     Netlog.logf `Err "AMQP: %s" msg;
	     let proto = protocol ep in
	     let e =
	       create_method_exception
		 proto ~class_id ~meth_id ~reply_code ~reply_text in
	     propagate_error ep e (Some ch);
	     Some(`AMQP_0_9 `Channel_close_ok)
	 | _ ->
	     assert false
    )
    (fun () -> 
       disable_channel ep ch;
       (* This will not prevent the close-ok from being delivered because
	  sync_s2c puts the output messages onto the priority queue
	*)
    );

  register_sync_s2c ep
    (`AMQP_0_9 `Channel_flow)
    ch
    (fun m ->
       match m with
	 | `AMQP_0_9(`Channel_flow active) ->
	     flow_control ep ch active;
	     Some(`AMQP_0_9 (`Channel_flow active))
	 | _ ->
	     assert false
    )
    (fun () -> ());

  sync_c2s_e ep
    (`AMQP_0_9 (`Channel_open ""))
    None
    ch
    ch_obj.tmo
  ++ (fun (m,_) ->
	match m with
	  | `AMQP_0_9(`Channel_open_ok "") ->
	      eps_e (`Done ch_obj) esys
	  | _ ->
	      assert false
     )

let open_s c ch =
  sync (open_e c) ch
  

let open_next_e c =
  let ep = Netamqp_connection.endpoint c in
  open_e c (suggest_channel ep)


let open_next_s c =
  sync open_next_e c

let close_e co =
  let c = co.c in
  let ep = co.ep in
  if not (Netamqp_connection.is_open c) then
    raise(Netamqp_connection.Not_open);

  if not (is_channel_enabled ep co.ch) then raise Not_open;

  let esys = event_system ep in
  let e =
    sync_c2s_e ep
      ~on_timeout:(fun () ->
		     disable_channel ep co.ch
		  )
      (`AMQP_0_9 (`Channel_close(200, "OK", 0, 0)))
      None
      co.ch
      co.tmo
    ++ (fun (m, _) ->
	  match m with
	    | `AMQP_0_9(`Channel_close_ok) ->
		disable_channel ep co.ch;
		eps_e (`Done ()) esys
	    | _ ->
		assert false
       ) in
  e

let close_s co =
  sync close_e co

let is_open co =
  let c = co.c in
  let ep = co.ep in
  Netamqp_connection.is_open c && is_channel_enabled ep co.ch


let configure_sync_timeout co tmo =
  co.tmo <- tmo

let sync_c2s_e ?no_wait co meth d_opt =
  Netamqp_endpoint.sync_c2s_e
    ~on_timeout:(fun() ->
		   (* trigger channel closure *)
		   ignore(close_e co)
		)
    ?no_wait co.ep meth d_opt co.ch co.tmo

let sync_c2s_s ?no_wait co meth d_opt =
  sync (sync_c2s_e ?no_wait co meth) d_opt


let connection co = co.c
let endpoint co = co.ep
let event_system co = Netamqp_connection.event_system co.c
let number co = co.ch
