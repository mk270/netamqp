(* $Id: netamqp_connection.ml 53444 2011-03-10 14:08:13Z gerd $ *)

open Netamqp_types
open Netamqp_endpoint
open Uq_engines.Operators
open Printf

exception Not_open
exception Error of string


type auth_method = string * string * string
    (* mech, username, password *)

type locale_preference =
    [ `Pref of string
    | `Select of string list -> string
    ]

type state =
    [ `Closed
    | `Start
    | `Start_ok
    | `Secure_ok
    | `Tune_ok
    | `Opened
    | `Close_requested
    ]

type connection =
    { ep : endpoint;
      mutable lp : locale_preference;
      mutable vhost : string;
      mutable state : state;
      mutable auth_l : auth_method list;
      mutable auth_s : string;
    }

let create ep =
  { ep = ep;
    lp = `Pref "";
    vhost = "";
    state = `Closed;
    auth_l = [];
    auth_s = ""
  }

let when_down ep f =
  let rec watch() =
    let e = state_change_e ep in
    Uq_engines.when_state
      ~is_done:(fun st ->
		  match st with
		    | `Disconnected | `Off | `Error _ ->
			f()
		    | _ ->
			watch()
	       )
      e in
  watch()


let null_uint4 =
  Rtypes.uint4_of_int 0

let open_e c auth_l lp vhost =
  if c.state <> `Closed then
    failwith "Netamqp_connection.open_e: can only open closed connections";
  c.lp <- lp;
  c.auth_l <- auth_l;
  c.vhost <- vhost;
  c.state <- `Start;

  connect c.ep;

  let esys = event_system c.ep in
  let tmo_g = Unixqueue.new_group esys in

  let handshake_e, signal_handshake = Uq_engines.signal_engine esys in

  let catch_error f =
    try Some(f())
    with
      | Error _ as err ->
	  signal_handshake (`Error err); None in

  (* Register the handler for the "start" method: *)
  register_sync_s2c
    c.ep (`AMQP_0_9 `Connection_start) 0 
    (fun start_meth ->
       match start_meth with
	 | `AMQP_0_9
	     (`Connection_start(v_maj, v_min, props, mechs_s, locales)) ->
	     catch_error
	       (fun () ->
		  if c.state <> `Start then
		    raise(Error "Not in `Start state");
		  Netlog.logf `Info
		    "AMQP: connection-start mechs=%s locales=%s"
		    mechs_s locales;
		  let mechs_l = Pcre.split mechs_s in
		  let (auth_s,user,pw) =
		    try
		      List.find
			(fun (n, _, _) -> List.mem n mechs_l)
			auth_l
		    with
		      | Not_found ->
			  raise(Error "No suitable authentication methods") in
		  c.auth_s <- auth_s;
		  let loc_l = Pcre.split locales in
		  let loc =
		    match lp with
		      | `Pref l ->
			  if List.mem l loc_l then l else
			    if l = "" then "C" else List.hd loc_l
		      | `Select f ->
			  f loc_l in
		  (* This is for PLAIN auth - we don't have anything else: *)
		  let resp = "\000" ^ user ^ "\000" ^ pw in
		  c.state <- `Start_ok;
		  let client_props = [] in
		  `AMQP_0_9(`Connection_start_ok
			      (client_props, auth_s, resp, loc))
	       )
	 | _ ->
	     assert false
    )
    (fun () ->
       ()
    );

  (* Register the handler for the "tune" method: *)
  register_sync_s2c
    c.ep (`AMQP_0_9 `Connection_tune) 0 
    (fun tune_meth ->
       match tune_meth with
	 | `AMQP_0_9(`Connection_tune(ch_max, frame_max, heartbeat)) ->
	     catch_error
	       (fun () ->
		  if c.state <> `Start_ok then
		    raise(Error "Not in `Start_ok state");
		  Netlog.logf `Info
		    "AMQP: connection-tune \
                     ch_max=%d frame_max=%Ld heartbeat=%d"
		    ch_max (Rtypes.int64_of_uint4 frame_max) heartbeat;
		  c.state <- `Tune_ok;
		  (* Maybe we have to lower the max frame size: *)
		  let mplex_eff_frame_max =
		    Rtypes.uint4_of_int (eff_max_frame_size c.ep) in
		  let eff_frame_max =
		    if frame_max = null_uint4 || 
		       (Rtypes.gt_uint4 frame_max mplex_eff_frame_max)
		    then
		      mplex_eff_frame_max
		    else
		      frame_max in
		  set_max_frame_size c.ep (Rtypes.int_of_uint4 eff_frame_max);
		  Netlog.logf `Info
		    "AMQP: connection-tune-ok frame_max=%Ld"
		    (Rtypes.int64_of_uint4 eff_frame_max);
		  `AMQP_0_9(`Connection_tune_ok(ch_max, eff_frame_max, 
						heartbeat))
	       )
	 | _ ->
	     assert false
    )
    (fun () ->
       (* After tune, we have to open: *)
       let open_e =
	 sync_c2s_e
	   c.ep (`AMQP_0_9 (`Connection_open(c.vhost, "", false))) 
	   None 0 300.0 in
       Uq_engines.when_state
	 ~is_done:(fun (resp_m, _) ->
		     match resp_m with
		       | `AMQP_0_9(`Connection_open_ok _) ->
			   c.state <- `Opened;
			   signal_handshake (`Done ())
		       | _ ->
			   assert false
		  )
	 ~is_error:(fun error -> signal_handshake(`Error error))
	 ~is_aborted:(fun _ -> signal_handshake(`Error(Error "Aborted")))
	 open_e
    );

  (* Handler for "close" *)
  register_sync_s2c
    c.ep (`AMQP_0_9 `Connection_close) 0
    (fun close_meth ->
       match close_meth with
	 | `AMQP_0_9(`Connection_close(reply_code, reply_text,
				       class_id, meth_id)) ->
	     let msg =
	       sprintf "connection-close from server: \
                        code=%d text=%s class-id=%d meth-id=%d"
		 reply_code reply_text class_id meth_id in
	     Netlog.logf `Err "AMQP: %s" msg;
	     (* We arrange now that the close-ok response can still be sent,
		and that the socket is closed after that. Any other activity
		is suppressed. Also, the error is first reported to the user
		when the socket is down.
	      *)
	     drop_frames c.ep;   (* Drop all incoming frames *)
	     clear_output c.ep;  (* Drop all pending output frames *)
	     expect_eof c.ep;    (* Permit EOF from server *)
	     c.state <- `Closed;
	     when_down c.ep
	       (fun () -> (* called back when the socket is down *)
		  let proto = protocol c.ep in
		  let e =
		    create_method_exception
		      proto ~class_id ~meth_id ~reply_code ~reply_text in
		  signal_handshake (`Error e);
		  abort_and_propagate_error c.ep e;
	       );
	     (* Reply this: *)
	     Some(`AMQP_0_9 `Connection_close_ok)
	 | _ ->
	     assert false
    )
    (fun () -> 
       (* Trigger now the disconnect, following directly after sending
	  close-ok
	*)
       quick_disconnect c.ep; 
    );

  (* Watch out for errors: *)
  listen_for_errors
    c.ep None
    (fun e ->
       Netlog.logf `Err
	 "AMQP: exception: %s" (Netexn.to_string e);
       signal_handshake (`Error e);  (* Maybe we are still handshaking *)
       true
    );

  let ann_e =
    announce_e c.ep in

  let all_e =
    Uq_engines.sync_engine ann_e handshake_e
    ++ (fun ((),()) -> 
	  Unixqueue.clear esys tmo_g;
	  eps_e (`Done ()) esys) in

  let timeout = ref false in
  Unixqueue.once esys tmo_g (get_timeout c.ep)
    (fun () ->
       timeout := true;
       all_e # abort();
    );

  all_e
  >> (function
	| `Aborted when !timeout -> `Error Timeout
	| other -> other
     )

let open_s c auth_l lp vhost =
  sync (open_e c auth_l lp) vhost
  

let is_open c =
  c.state = `Opened


let close_e c =
  if c.state <> `Opened then
    raise Not_open;
  
  c.state <- `Close_requested;

  let esys = event_system c.ep in
  let e =
    sync_c2s_e c.ep
      (`AMQP_0_9 (`Connection_close(200, "OK", 0, 0)))
      None
      0
      (get_timeout c.ep)
    ++ (fun (m, _) ->
	  match m with
	    | `AMQP_0_9(`Connection_close_ok) ->
		c.state <- `Closed;
		eps_e (`Done ()) esys
	    | _ ->
		assert false
       ) in

  expect_eof c.ep;
  disconnect c.ep;

  e


let close_s c =
  sync close_e c

let plain_auth user pw =
  ("PLAIN", user, pw)


let endpoint c = c.ep
let event_system c = Netamqp_endpoint.event_system c.ep
