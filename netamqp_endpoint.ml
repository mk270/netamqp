(* $Id: netamqp_endpoint.ml 53446 2011-03-10 14:45:03Z gerd $ *)

(* TODO:
   - heartbeats (also: heartbeat frame length)
   - Move all version-specific stuff to Netamqp_methods

   FIXME:
   - there is no way to unregister handlers other than to close channels
     or connections!
   - unclear what to do if we get incomplete content messages. Right now
     we close the connection.
 *)

open Netamqp_types
open Uq_engines.Operators
open Printf

type connector =
    [ `Sockaddr of Unix.sockaddr
    | `Inet of string * int
    | `Implied
    ]

type transport_layer =
    [ `TCP of connector
    | `Custom of 
	(unit -> Netamqp_transport.amqp_multiplex_controller Uq_engines.engine)
    ]

type state =
    [ `Off
    | `Connected of bool
    | `Disconnecting of bool
    | `Disconnected
    | `Error of exn
    ]

type proto_revision_0_9 =
    [ `One ]

type protocol =
    [ `AMQP_0_9 of proto_revision_0_9 ]

type sync_client_to_server_method_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.sync_client_to_server_method_t ]

type sync_server_to_client_method_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.sync_server_to_client_method_t ]

type sync_client_initiated_method_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.sync_client_initiated_method_t ]

type sync_server_initiated_method_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.sync_server_initiated_method_t ]

type sync_server_initiated_method_type_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.sync_server_initiated_method_type_t ]

type async_client_to_server_method_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.async_client_to_server_method_t ]

type async_server_to_client_method_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.async_server_to_client_method_t ]

type async_server_to_client_method_type_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.async_server_to_client_method_type_t ]

type props_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.props_t ]

type any_server_to_client_method_type_0_9_t =
    [ Netamqp_methods_0_9.sync_server_to_client_method_type_t
    | Netamqp_methods_0_9.async_server_to_client_method_type_t
    ]

type any_server_to_client_method_type_t =
    [ `AMQP_0_9 of any_server_to_client_method_type_0_9_t ]

type any_server_to_client_method_0_9_t =
    [ Netamqp_methods_0_9.sync_server_to_client_method_t
    | Netamqp_methods_0_9.async_server_to_client_method_t
    ]


type any_server_to_client_method_t =
    [ `AMQP_0_9 of any_server_to_client_method_0_9_t ]

type method_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.method_t ]

type method_type_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.method_type_t ]

type data =
    props_t * Xdr_mstring.mstring list

type reg =
    < reg_callback : any_server_to_client_method_t -> data option -> unit;
      reg_error : exn -> unit;
      reg_tmo_group : Unixqueue.group option;
      reg_once : bool;    (* inactivate reg after calling back once *)
      reg_sync : bool;    (* part of sync call -> receives errors *)
    >
    (* registered handler *)

type build =
    { meth : any_server_to_client_method_t;
      mutable props : props_t option;
      mutable exp_size : int64;
      mutable cur_size : int64;
      data : Xdr_mstring.mstring Queue.t
    }
    (* build [data] from frame pieces *)

type err_listener =
    < err_callback : exn -> bool >

type endpoint =
    { esys : Unixqueue.event_system;
      transport_layer : transport_layer;
      protocol : protocol;
      mutable state : state;
      mutable state_notifications : (unit -> unit) Queue.t;
      mutable timeout : float;
      mutable conn_eng : 
	Netamqp_transport.amqp_multiplex_controller Uq_engines.engine option;
      mutable announced : bool;
      mutable announce_notification : 
	(unit Uq_engines.final_state -> unit) option;
      mutable conn_tmo_group : Unixqueue.group option;
      mutable channels : (channel, bool) Hashtbl.t;
        (* if a channel is present in the hashtbl it is enabled.
	   The bool arg is the flow control flag
	 *)
      mutable suggest_cnt : int;
      mutable out_q : 
	(channel, (Netamqp_types.frame * (exn option->unit)) Queue.t) Hashtbl.t;
        (* The unit->unit function is called when the frame is sent *)
      mutable out_prio_q : Netamqp_types.frame Queue.t;
      mutable out_next_ch : channel Queue.t;
      mutable in_build : (channel, build) Hashtbl.t;
      mutable in_tab : (channel * any_server_to_client_method_type_t,
			reg list) Hashtbl.t;
      mutable err_tab : (channel option, err_listener list) Hashtbl.t;
      mutable out_active : bool;
      mutable drop_frames : bool;
      mutable expect_eof : bool;
      mutable quick_disconnect : bool;
    }


(**********************************************************************)

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netamqp_endpoint" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netamqp_endpoint" Debug.enable

let () =
  Netlog.Debug.register_module "Netamqp_endpoint" Debug.enable


(**********************************************************************)

let mplex_opt ep =
  match ep.conn_eng with
    | Some e ->
	( match e#state with
	    | `Done mplex -> Some mplex
	    | _ -> None
	)
    | None -> None

let mplex ep =
  match mplex_opt ep with
    | None -> failwith "Netamqp_endpoint.mplex"
    | Some mplex -> mplex


let string_of_state =
  function
    | `Off -> "off"
    | `Connected flag -> sprintf "connected(%B)" flag
    | `Disconnecting flag -> sprintf "disconnecting(%B)" flag
    | `Disconnected -> "disconnected"
    | `Error e -> sprintf "error(%s)" (Netexn.to_string e)


let protocol ep = ep.protocol


let update_state ep new_state =
  dlogr
    (fun () ->
       sprintf "update_state: %s" (string_of_state new_state)
    );
  ep.state <- new_state;
  (* We need to ensure that users of this notification service have the
     chance to get all state changes. So we have to call [f] immediately
     back, and not from a [once] handler. Also, the users may want to
     add new elements to state_notifications for setting up the next round
     of notifications.
   *)
  let g = Unixqueue.new_group ep.esys in
  let q = Queue.create() in
  Queue.transfer ep.state_notifications q;
  Queue.iter
    (fun f ->
       try f()
       with e ->
	 Unixqueue.once ep.esys g 0.0 (fun () -> raise e)
    )
    q

let fire_regs ep ch mt f =
  let regs =
    try Hashtbl.find ep.in_tab (ch,mt) with Not_found -> [] in
  let regs' =
    List.filter (fun reg -> not reg#reg_once) regs in
  if regs' = [] then
    Hashtbl.remove ep.in_tab (ch, mt)
  else
    Hashtbl.replace ep.in_tab (ch, mt) regs';
  List.iter (fun reg -> f reg) regs


let invoke_regs ep ch mt m d_opt =
  let invoke_reg reg =
    ( match reg # reg_tmo_group with
	| None -> ()
	| Some g -> Unixqueue.clear ep.esys g
    );
    let g = Unixqueue.new_group ep.esys in
    try
      reg # reg_callback m d_opt
    with error ->
      Unixqueue.once ep.esys g 0.0 (fun () -> raise error)
  in
  fire_regs ep ch mt invoke_reg


let error_regs ep ch mt err =
  let error_reg reg =
    ( match reg # reg_tmo_group with
	| None -> ()
	| Some g -> Unixqueue.clear ep.esys g
    );
    let g = Unixqueue.new_group ep.esys in
    try
      reg # reg_error err
    with error ->
      Unixqueue.once ep.esys g 0.0 (fun () -> raise error)
  in
  fire_regs ep ch mt error_reg

let error_regs_to_channel ep chan_opt err =
  let keys =
    Hashtbl.fold
      (fun (ch,mtype) _ acc ->
	 if chan_opt = None || chan_opt = Some ch then (ch,mtype)::acc else acc
      )
      ep.in_tab
      [] in
  List.iter 
    (fun (ch,mtype) -> error_regs ep ch mtype err)
    keys

let rm_channel_from_in_tab ep chan =
  let keys =
    Hashtbl.fold
      (fun (ch,mtype) _ acc -> if ch = chan then (ch,mtype)::acc else acc)
      ep.in_tab
      [] in
  List.iter
    (fun key -> Hashtbl.remove ep.in_tab key)
    keys


let tell_listeners ep ch_opt err =
  let g = Unixqueue.new_group ep.esys in
  let listeners =
    try Hashtbl.find ep.err_tab ch_opt with Not_found -> [] in
  let listeners' =
    List.filter
      (fun listener ->
	 try
	   listener # err_callback err
	 with error ->
	   Unixqueue.once ep.esys g 0.0 (fun () -> raise error);
	   false
      )
      listeners in
  if listeners' = [] then
    Hashtbl.remove ep.err_tab ch_opt
  else
    Hashtbl.replace ep.err_tab ch_opt listeners'
  

let abort ep =
  dlog "abort";
  ( match ep.conn_eng with
      | None -> ()
      | Some eng -> 
	  ( match eng#state with
	      | `Done mplex ->
		  mplex # inactivate()
	      | `Working _ ->
		  eng#abort()
	      | _ ->
		  ()
	  );
	  ep.conn_eng <- None
  );
  ( match ep.conn_tmo_group with
      | None -> ()
      | Some g -> 
	  Unixqueue.clear ep.esys g;
	  ep.conn_tmo_group <- None
  )

let propagate_error ep err chan_opt =
  dlogr
    (fun () -> 
       sprintf "propagate_error chan=%s err=%s"
	 (match chan_opt with
	    | None -> "all"
	    | Some ch -> string_of_int ch
	 )
	 (Netexn.to_string err)
    );
  ( match ep.announce_notification with
      | None -> ()
      | Some notify ->
	  notify (`Error err);
	  ep.announce_notification <- None
  );
  error_regs_to_channel ep chan_opt err;
  ( match chan_opt with
      | None ->
	  let keys =
	    Hashtbl.fold
	      (fun ch_opt _ acc -> ch_opt :: acc)
	      ep.err_tab [] in
	  List.iter (fun ch_opt -> tell_listeners ep ch_opt err) keys

      | Some chan ->
	  List.iter
	    (fun ch_opt ->
	       tell_listeners ep ch_opt err
	    )
	    [ Some chan; None ]
  )


let abort_and_propagate_error ep err =
  abort ep;
  propagate_error ep err None;
  update_state ep (`Error err)


let enable_channel_i ep ch =
  if not (Hashtbl.mem ep.channels ch) then (
    dlogr (fun () -> sprintf "enable_channel %d" ch);
    Hashtbl.add ep.channels ch true;
    Hashtbl.add ep.out_q ch (Queue.create());
    Queue.add ch ep.out_next_ch
  )


let enable_channel ep ch =
  if ch < 1 || ch > 65535 then
    invalid_arg "Netamqp_endpoint.enable_channel";
  enable_channel_i ep ch


let disable_channel ep ch =
  if ch < 1 || ch > 65535 then
    invalid_arg "Netamqp_endpoint.disable_channel";
  if Hashtbl.mem ep.channels ch then (
    dlogr (fun () -> sprintf "disable_channel %d" ch);
    Hashtbl.remove ep.channels ch;
    Hashtbl.remove ep.out_q ch;
    Hashtbl.remove ep.in_build ch;
    error_regs_to_channel ep (Some ch) Method_dropped;
    rm_channel_from_in_tab ep ch;
    Hashtbl.remove ep.err_tab (Some ch);
    let q = Queue.create() in
    Queue.iter (fun ch' -> if ch <> ch' then Queue.add ch' q) ep.out_next_ch;
    Queue.clear ep.out_next_ch;
    Queue.transfer q ep.out_next_ch;
  )

let is_channel_enabled ep ch =
  Hashtbl.mem ep.channels ch

let suggest_channel ep =
  let rec loop () =
    let ch = ep.suggest_cnt in
    ep.suggest_cnt <- (ch+1) mod 65536;
    if Hashtbl.mem ep.channels ch then
      loop()
    else
      ch in
  if Hashtbl.length ep.channels >= 65536 then
    failwith "Netamqp_endpoint.suggest_channel: all channels are used";
  loop()


let flow_control ep ch flow_on =
  if not (Hashtbl.mem ep.channels ch) then
    failwith "Netamqp_endpoint.flow_control: this channel is disabled";
  dlogr (fun () -> sprintf "flow_control ch=%d flag=%B" ch flow_on);
  Hashtbl.replace ep.channels ch flow_on

let is_clean_for_disconnect ep =
  (* Can we finish the disconnect? Are all requested tasks done? *)
  ep.announced &&
  ( Queue.is_empty ep.out_prio_q ) &&
  ( let p = ref true in
    Hashtbl.iter (fun _ q -> p := !p && Queue.is_empty q) ep.out_q;
    !p
  ) &&
  ( ep.quick_disconnect ||
      ( let p = ref true in
	Hashtbl.iter (fun _ rl ->
			List.iter
			  (fun r ->
			     p := !p && not (r#reg_sync))
			  rl
		     ) ep.in_tab;
	!p
      )
  )

let maybe_disconnect ep =
  match ep.state with
    | `Disconnecting _ ->
	if is_clean_for_disconnect ep then (
	  dlog "disconnect";
	  abort ep;
	  update_state ep `Disconnected
	)
    | _ -> ()


let abort_for_eof ep =
  if ep.expect_eof then (
    dlog "expected eof";
    abort ep;
    update_state ep `Disconnected
  )
  else
    abort_and_propagate_error ep Unexpected_eof


(**********************************************************************)
(* Protocol versioning                                                *)
(**********************************************************************)

let method_has_content_0_9 m =
  let mtype = Netamqp_methods_0_9.type_of_method m in
  List.mem mtype Netamqp_methods_0_9.content_method_types


let type_of_method (m : method_t) : method_type_t =
  match m with
    | `AMQP_0_9 m' ->
	`AMQP_0_9 (Netamqp_methods_0_9.type_of_method m')

let type_of_any_server_to_client_method (m : any_server_to_client_method_t)
    : any_server_to_client_method_type_t =
  let mt1 = type_of_method (m :> method_t) in
  match mt1 with
    | `AMQP_0_9 mt2 ->
	( match mt2 with
	    | #any_server_to_client_method_type_0_9_t as mt3 ->
		`AMQP_0_9 mt3
	    | _ ->
		assert false
	)

let response_types_of_method (m : method_t)
    : any_server_to_client_method_type_t list =
  match m with
    | `AMQP_0_9 m' ->
	let mtype = Netamqp_methods_0_9.type_of_method m' in
	List.map
	  (fun mt -> 
	     match mt with
	       | #any_server_to_client_method_type_0_9_t as mt' ->
		   `AMQP_0_9 mt'
	       | _ ->
		   assert false
	  )
	  (Netamqp_methods_0_9.responses_of_method mtype)

let string_of_method_type (m : method_type_t) =
  match m with
    | `AMQP_0_9 m' ->
	Netamqp_methods_0_9.string_of_method_type m'

let string_of_method (m : method_t) =
  string_of_method_type (type_of_method m) ^ "(...)"

let proto_version_string =
  function
    | `AMQP_0_9 `One ->
	"\000\009\001"

let coerce_to_sync_server_to_client_method_t (m1 : method_t) 
    : sync_server_to_client_method_t =
  match m1 with
    | `AMQP_0_9 m2 ->
	( match m2 with
	    | #Netamqp_methods_0_9.sync_server_to_client_method_t as m3 ->
		`AMQP_0_9 m3
	    | _ ->
		assert false  (* coercion failed *)
	)


let coerce_to_async_server_to_client_method_t (m1 : method_t) 
    : async_server_to_client_method_t =
  match m1 with
    | `AMQP_0_9 m2 ->
	( match m2 with
	    | #Netamqp_methods_0_9.async_server_to_client_method_t as m3 ->
		`AMQP_0_9 m3
	    | _ ->
		assert false  (* coercion failed *)
	)


let coerce_to_sync_server_initiated_method_t (m1 : method_t)
    : sync_server_initiated_method_t =
  match m1 with
    | `AMQP_0_9 m2 ->
	( match m2 with
	    | #Netamqp_methods_0_9.sync_server_initiated_method_t as m3 ->
		`AMQP_0_9 m3
	    | _ ->
		assert false  (* coercion failed *)
	)


(**********************************************************************)
(* Input side                                                         *)
(**********************************************************************)

let dispatch_method ep (m : any_server_to_client_method_t) d_opt ch =
  let mt = type_of_any_server_to_client_method m in
  let regs =
    try Hashtbl.find ep.in_tab (ch, mt) with Not_found -> [] in
  if regs = [] then (
(*
    let mt = type_of_method (m :> method_t) in
    propagate_error ep
      (Method_cannot_be_dispatched(ch, string_of_method_type mt))
      (Some ch)
 *)
    (* FIXME: Unclear what to do now. We just log the incident *)
    Netlog.logf `Err
      "Netamqp: Method cannot be dispatched channel=%d method=%s"
      ch (string_of_method (m :> method_t))
  ) else (
    dlogr 
      (fun () ->
	 sprintf "dispatch_method ch=%d data=%B meth=%s"
	   ch (d_opt <> None) (string_of_method (m :> method_t))
      );
    invoke_regs ep ch mt m d_opt;
    maybe_disconnect ep
  )


let handle_frame_0_9 ep frame =
  (* AMQP-0-9-specific version *)
  dlogr
    (fun () ->
       let n = Xdr_mstring.length_mstrings frame.frame_payload in
       let s = Xdr_mstring.prefix_mstrings frame.frame_payload (min n 200) in
       sprintf "decode_message type=%s ch=%d payload=%s"
	 ( match frame.frame_type with
	     | `Method -> "method"
	     | `Header -> "header"
	     | `Body -> "body"
	     | `Heartbeat -> "heartbeat"
	     | `Proto_header -> "proto_header"
	 )
	 frame.frame_channel
	 (Rpc_util.hex_dump_s s 0 (String.length s) ^ 
	    if n > String.length s then "..." else "")
    );
  let msg = Netamqp_methods_0_9.decode_message frame in
  ( match msg with
      | `Method (#any_server_to_client_method_0_9_t as m) ->
	  (* First check whether we are already building a
	     data item
	   *)
	  if Hashtbl.mem ep.in_build frame.frame_channel 
	  then (
	    dlog "content not fully transmitted - dropping method";
	    abort_and_propagate_error ep (Unexpected_frame frame);
	    Hashtbl.remove ep.in_build frame.frame_channel
	  );
	  (* If the method is not followed by data we can immediately 
	     dispatch on it. Otherwise we need to create a builder
	   *)
	  if method_has_content_0_9 m then (
	    dlog "method with content";
	    let b =
	      { meth = `AMQP_0_9 m;
		props = None;
		exp_size = 0L;
		cur_size = 0L;
		data = Queue.create()
	      } in
	    Hashtbl.add ep.in_build frame.frame_channel b
	  )
	  else
	    dispatch_method ep (`AMQP_0_9 m) None frame.frame_channel;
	  if not ep.announced then (
	    match ep.announce_notification with
	      | None -> ()
	      | Some f -> 
		  f (`Done());
		  ep.announced <- true
	  )

      | `Method _ ->
	  dlog "client cannot receive this type of method";
	  abort_and_propagate_error ep (Unexpected_frame frame)
      | `Header(props,size) ->
	  (* Check whether we have a builder in the right state *)
	  ( try
	      let b = Hashtbl.find ep.in_build frame.frame_channel in
	      if b.props <> None then raise Not_found; (* bad *)
	      b.props <- Some (`AMQP_0_9 props);
	      b.exp_size <- size;
	      dlog "good content header";
	    with
	      | Not_found ->
		  dlog "unexpected content header";
		  abort_and_propagate_error ep (Unexpected_frame frame)
	  )
      | `Body mstrings ->
	  (* Check whether we have a builder in the right state *)
	  ( try
	      let b = Hashtbl.find ep.in_build frame.frame_channel in
	      if b.props = None then raise Not_found; (* bad *)
	      let props =
		match b.props with Some p -> p | None -> assert false in
	      let l = Xdr_mstring.length_mstrings mstrings in
	      b.cur_size <- Int64.add b.cur_size (Int64.of_int l);
	      if b.cur_size > b.exp_size then raise Not_found;
	      dlog "good content body frame";
	      List.iter
		(fun ms -> Queue.add ms b.data)
		mstrings;
	      if b.cur_size = b.exp_size then (
		Hashtbl.remove ep.in_build frame.frame_channel;
		let q =
		  Queue.fold (fun acc ms -> ms :: acc) [] b.data in
		let d =
		  (props, q) in
		dispatch_method ep b.meth (Some d) frame.frame_channel
	      )
	    with
	      | Not_found ->
		  dlog "unexpected content body, or size mismatch";
		  abort_and_propagate_error ep (Unexpected_frame frame)
	  )
      | `Heartbeat ->
	  ()
      | `Proto_header p ->
	  ( match ep.announce_notification with
	      | None ->
		  dlog "unexpexted proto header";
		  abort_and_propagate_error ep (Unexpected_frame frame)
	      | Some f ->
		  dlog "protocol not supported";
		  f (`Error Protocol_is_not_supported)
	  )
  )


let handle_frame ep frame =
  (* We received this frame. Do something with it. *)
  match ep.protocol with
    | `AMQP_0_9 _ ->
	handle_frame_0_9 ep frame


(* The input_thread is started once we are connected. The function
   continuously calls itself to accept new frames. When we get
   disconnected, the recursion silently dies.
 *)

let rec input_thread ep =
  match mplex_opt ep with
    | None ->
	()
    | Some mplex ->
	dlog "input_thread";
	mplex # start_reading
	  ~when_done:(fun result ->
			match result with
			  | `Ok frame ->
			      dlog "got frame";
			      let cont =
				try
				  ep.drop_frames || 
				    (handle_frame ep frame; true)
				with
				  | e ->
				      abort_and_propagate_error ep e; false in
			      if cont then
				input_thread ep
			  | `Error e ->
			      abort_and_propagate_error ep e
			  | `End_of_file ->
			      abort_for_eof ep
		     )
	  ()
	


(**********************************************************************)
(* Output side                                                        *)
(**********************************************************************)

let rec output_thread ep =
  if not ep.out_active then
    output_start ep

and output_start ep =
  match mplex_opt ep with
    | None ->
	()
    | Some mplex ->
	ep.out_active <- true;

	dlog "checking for output";
	
	let can_output = ref false in

	(* Check whether we can do something. *)
	if not(Queue.is_empty ep.out_prio_q) then (
	  can_output := true;
	  output_next ep mplex ((Queue.take ep.out_prio_q), (fun _ -> ()))
	)
	else (
	  let n = Queue.length ep.out_next_ch in
	  let k = ref 0 in
	  while !k < n do
	    incr k;
	    let ch = Queue.take ep.out_next_ch in
	    Queue.add ch ep.out_next_ch;
	    let ch_is_on =
	      try Hashtbl.find ep.channels ch with Not_found -> false in
	    if ch_is_on then (
	      let ch_q = 
		try Hashtbl.find ep.out_q ch
		with Not_found -> Queue.create() in
	      if not (Queue.is_empty ch_q) then (
		k := n;
		can_output := true;
		output_next ep mplex (Queue.take ch_q)
	      )
	    )
	  done;
	  
	  if not !can_output then (

	    (* If we reach this point, nothing can be output. Stop the
	       output thread. Also, we have to check whether the disconnect
	       procedure can be continued.
	     *)
	    ep.out_active <- false;
	    
	    dlog "nothing to output";
	    
	    maybe_disconnect ep
	  )
	)
  
and output_next ep mplex (frame,is_sent) =
  dlog "output_next";
  mplex # start_writing
    ~when_done:(fun r ->
		  ep.out_active <- false;
		  match r with
		    | `Ok() ->
			(* Success. Try to write the next frame: *)
			is_sent None;
			output_start ep
		    | `Error e ->
			(* Error *)
			is_sent (Some e);
			abort_and_propagate_error ep e
	       )
    frame


let shared_sub_mstring (ms : Xdr_mstring.mstring)
                       sub_pos sub_len : Xdr_mstring.mstring =
  (* Returns an mstring that accesses the substring of ms at sub_pos
     with length sub_len. The returned mstring shares the representation
     with ms
   *)
  let ms_len = ms#length in
  if sub_len < 0 || sub_pos < 0 || sub_pos > ms_len - sub_len then
    invalid_arg "Netamqp_endpoint.shared_sub_mstring";
  ( object(self)
      method length = sub_len
      method blit_to_string mpos s spos len =
	ms#blit_to_string (sub_pos+mpos) s spos len
      method blit_to_memory mpos mem mempos len =
	ms#blit_to_memory (sub_pos+mpos) mem mempos len
      method as_string =
	let (s,pos) = ms#as_string in
	(s,pos+sub_pos)
      method as_memory =
	let (m,pos) = ms#as_memory in
	(m,pos+sub_pos)
      method preferred =
	ms#preferred
    end
  )


let split_mstrings l max_len =
  (* Transform the list of mstrings l into another list where the
     mstrings are not longer than max_len
   *)
  let rec transform ms pos rem =
    let len = min rem max_len in
    let ms0 = shared_sub_mstring ms pos len in
    let rem' = rem - len in
    if rem' > 0 then
      ms0 :: transform ms (pos+len) rem'
    else
      [ms0] in

  List.flatten
    (List.map (fun ms -> transform ms 0 ms#length) l)


let mk_frames_0_9 ep (m : Netamqp_methods_0_9.method_t) d_opt ch =
  let m_frame =
    Netamqp_methods_0_9.encode_message (`Method m) ch in
  if method_has_content_0_9 m then (
    match d_opt with
      | None ->
	  failwith "Netamqp_endpoint: This method needs payload data!"
      | Some(prop,body) ->
	  ( match prop with
	      | `AMQP_0_9 prop' ->
		  let size =
		    Int64.of_int (Xdr_mstring.length_mstrings body) in
		  let h_frame =
		    Netamqp_methods_0_9.encode_message
		      (`Header(prop', size)) ch in
		  (* There is a limit on the maximum frame length.
		     The body frames must not be longer than that!
		     There are 8 bytes for the frame itself.
		   *)
		  let body_max =
		    match mplex_opt ep with
		      | None -> (* strange *) 4096 - 8
		      | Some mplex ->
			  mplex#eff_max_frame_size - 8 in
		  dlogr
		    (fun () -> sprintf "body_max=%d" body_max);
		  let split_bodies =
		    split_mstrings body body_max in
		  dlogr
		    (fun () -> sprintf "orig_length=%Ld bodies=%d"
		       size (List.length split_bodies)
		    );
		  let body_frames =
		    List.map
		      (fun b -> 
			 Netamqp_methods_0_9.encode_message (`Body [b]) ch
		      )
		      split_bodies in
		    
		  m_frame :: h_frame :: body_frames
	  )
  )
  else
    [ m_frame ]


let mk_frames ep m d_opt ch =
  match m with
    | `AMQP_0_9 m' ->
	mk_frames_0_9 ep m' d_opt ch


(**********************************************************************)
(* connect logic                                                      *)
(**********************************************************************)

let connect ep =
  let do_it =
    match ep.state with
      | `Off -> true
      | `Connected _ -> false
      | `Disconnecting _ | `Disconnected | `Error _ ->
	  failwith
	    "Netamqp_endpoint.connect: already disconnecting or disconnected" in
  if do_it then (
    dlog "connect";
    let conn_eng =
      match ep.transport_layer with
	| `TCP conn ->
	    let spec, host_opt =
	      match conn with
		| `Sockaddr (Unix.ADDR_INET(ip,port)) -> 
		    `Sock_inet(Unix.SOCK_STREAM, ip, port), None
		| `Sockaddr (Unix.ADDR_UNIX path) ->
		    `Sock_unix(Unix.SOCK_STREAM, path), None
		| `Inet(name,port) ->
		    `Sock_inet_byname(Unix.SOCK_STREAM, name, port), (Some name)
		| `Implied ->
		    failwith "Netamqp_endpoint.connect: `Implied is not a \
                              valid address for TCP" in
	    Uq_engines.connector
	      (`Socket(spec, Uq_engines.default_connect_options))
	      ep.esys
	    ++ (fun st ->
		  match st with
		    | `Socket(fd, _) ->
			dlog "socket connection established";
			let mplex =
			  Netamqp_transport.tcp_amqp_multiplex_controller
			    ~close_inactive_descr:true
			    fd ep.esys in
			( match ep.conn_tmo_group with
			    | None -> ()
			    | Some g -> Unixqueue.clear ep.esys g
			);
			ep.conn_tmo_group <- None;
			eps_e (`Done mplex) ep.esys
		    | _ -> assert false
	       )
	    >> (function
		  | `Done mplex -> `Done mplex
		  | `Error Not_found when host_opt <> None ->
		      (* Current ocamlnet-3.2 still raises Not_found when the
			 host is not found. This will be changed to
			 the following exception in one of the next releases.
		       *)
		      `Error (Uq_resolver.Host_not_found
				(match host_opt with
				   | Some h -> h | None -> assert false
				))
		  | `Error e -> `Error e
		  | `Aborted -> `Aborted
	       )
	| `Custom f ->
	    f() in
    ep.conn_eng <- Some conn_eng;
    if ep.timeout >= 0.0 then (
      let g = Unixqueue.new_group ep.esys in
      Unixqueue.once ep.esys g ep.timeout
	(fun () ->
	   dlog "connect times out";
	   abort_and_propagate_error ep Timeout
	);
      ep.conn_tmo_group <- Some g;
    );
    update_state ep (`Connected false);
    Uq_engines.when_state
      ~is_done:(fun _ ->
		  update_state ep (`Connected true);
		  input_thread ep;
		  output_thread ep;
	       )
      conn_eng;
  )


let disconnect ep =
  match ep.state with
    | `Off ->
	()   (* ignored *)
    | `Connected is_online ->
	dlog "disconnecting";
	update_state ep (`Disconnecting is_online);
	maybe_disconnect ep
	(* We don't handle the case specially when is_online is false.
	   This means we are still connecting to the server. It is
	   possible that the user has already requested something, so
	   we should execute these requests first.
	 *)
    | `Disconnecting _ ->
	()
    | `Disconnected ->
	()
    | `Error _ ->
	()

let quick_disconnect ep =
  ep.quick_disconnect <- true;
  disconnect ep



(**********************************************************************)
(* create/reset, API stuff                                            *)
(**********************************************************************)

let create trans protocol esys =
  let ep =
    { esys = esys;
      transport_layer = trans;
      protocol = protocol;
      state = `Off;
      state_notifications = Queue.create();
      timeout = 300.0;
      conn_eng = None;
      announced = false;
      announce_notification = None;
      conn_tmo_group = None;
      channels = Hashtbl.create 7;
      suggest_cnt = 1;
      out_q = Hashtbl.create 7;
      out_prio_q = Queue.create();
      out_next_ch = Queue.create();
      in_build = Hashtbl.create 7;
      in_tab = Hashtbl.create 7;
      err_tab = Hashtbl.create 7;
      out_active = false;
      drop_frames = false;
      expect_eof = false;
      quick_disconnect = false;
    } in
  enable_channel_i ep 0;
  ep

let reset ep =
  abort ep;
  ep.state <- `Off;
  ep.state_notifications <- Queue.create();
  ep.timeout <- 300.0;
  ep.conn_eng <- None;
  ep.announced <- false;
  ep.announce_notification <- None;
  ep.conn_tmo_group <- None;
  ep.channels <- Hashtbl.create 7;
  ep.out_q <- Hashtbl.create 7;
  ep.out_prio_q <- Queue.create();
  ep.out_next_ch <- Queue.create();
  ep.in_build <- Hashtbl.create 7;
  ep.in_tab <- Hashtbl.create 7;
  ep.err_tab <- Hashtbl.create 7;
  ep.out_active <- false;
  ep.drop_frames <- false;
  ep.expect_eof <- false;
  ep.quick_disconnect <- false;
  enable_channel_i ep 0

      
let configure_timeout ep tmo =
  ep.timeout <- tmo

let get_timeout ep =
  ep.timeout

let default_port =
  5672

let event_system ep =
  ep.esys

let state ep =
  ep.state

let state_change_e ep =
  let eng, notify = Uq_engines.signal_engine ep.esys in
  Queue.add (fun () -> notify (`Done ep.state)) ep.state_notifications;
  eng

let set_max_frame_size ep s =
  match mplex_opt ep with
    | None ->
	failwith "Netamqp_endpoint.set_max_frame_size: not connected"
    | Some mplex ->
	mplex#set_max_frame_size s

let eff_max_frame_size ep =
  match mplex_opt ep with
    | None ->
	failwith "Netamqp_endpoint.eff_max_frame_size: not connected"
    | Some mplex ->
	mplex#eff_max_frame_size
 

let getsockname ep =
  match mplex_opt ep with
    | None ->
	failwith "Netamqp_endpoint.getsockname: not connected"
    | Some mplex ->
	mplex#getsockname

let getpeername ep =
  match mplex_opt ep with
    | None ->
	failwith "Netamqp_endpoint.getpeername: not connected"
    | Some mplex ->
	mplex#getpeername

let drop_frames ep =
  ep.drop_frames <- true

let clear_output ep =
  Queue.clear ep.out_prio_q;
  Hashtbl.iter (fun ch q -> Queue.clear q) ep.out_q

let expect_eof ep =
  ep.expect_eof <- true

let listen_for_errors ep ch_opt f =
  ( match ch_opt with
      | None -> ()
      | Some ch ->
	  if ch < 0 || ch > 65535 then
	    invalid_arg "Netamqp_endpoint.listen_for_errors: bad channel";
	  if not (is_channel_enabled ep ch) then
	    failwith "Netamqp_endpoint.listen_for_errors: this channel is \
                      disabled";
  );
  let listener =
    ( object method err_callback = f end ) in
  let l =
    try Hashtbl.find ep.err_tab ch_opt with Not_found -> [] in
  Hashtbl.replace ep.err_tab ch_opt (listener :: l)


let create_method_exception proto ~class_id ~meth_id ~reply_code ~reply_text =
  let mname =
    try
      match proto with
	| `AMQP_0_9 `One ->
	    Netamqp_methods_0_9.string_of_method_id
	      ~class_id ~meth_id
    with
      | Not_found ->
	  sprintf "%d:%d" class_id meth_id in
  Method_exception(mname, reply_code, reply_text)


let check_up ?(off_ok=false) ep =
  match ep.state with
    | `Off ->
	if not off_ok then
	  failwith "Netamqp_endpoint: state=`Off - connect first!"
    | `Connected _ ->
	()
    | `Disconnecting _ ->
	failwith "Netamqp_endpoint: state=`Disconnecting - not accepting \
                  requests anymore"
    | `Disconnected ->
	failwith "Netamqp_endpoint: state=`Disconnected - reset&connect first!"
    | `Error _ ->
	failwith "Netamqp_endpoint: state=`Error - reset&connect first!"


(**********************************************************************)
(* API for queue interaction                                          *)
(**********************************************************************)

exception Exit_sync

let sync f arg =
  let e = f arg in
  if Unixqueue.is_running e#event_system then
    failwith "Event queue is already processing events";
  let exit_sync _ =
    let g = Unixqueue.new_group e#event_system in
    Unixqueue.once e#event_system g 0.0 (fun () -> raise Exit_sync) in
  Uq_engines.when_state
    ~is_done:exit_sync ~is_error:exit_sync ~is_aborted:exit_sync e;
  ( try
      Unixqueue.run e#event_system
    with Exit_sync -> ()
  );
  match e#state with
    | `Done v -> v
    | `Error e -> raise e
    | `Aborted -> failwith "Aborted"
    | `Working _ -> assert false


let announce_e ep =
  if ep.announce_notification <> None then
    failwith "Netamqp_endpoint.announce_e: has already been called";
  check_up ep;
  dlog "announce";
  let data = proto_version_string ep.protocol in
  let frame =
    { frame_type = `Proto_header;
      frame_channel = 0;
      frame_payload = [ Netamqp_rtypes.mk_mstring data ]
    } in
  Queue.add frame ep.out_prio_q;
  let eng, notify = Uq_engines.signal_engine ep.esys in
  ep.announce_notification <- Some notify;
  eng


let announce_s ep = sync announce_e ep

let async_c2s_e ep (m : async_client_to_server_method_t) d_opt ch =
  if ch < 0 || ch > 65535 then
    invalid_arg "Netamqp_endpoint.async_c2s_e: bad channel";
  check_up ep;
  let q =
    try Hashtbl.find ep.out_q ch
    with Not_found ->
      failwith "Netamqp_endpoint.async_c2s_e: this channel is disabled" in
  dlogr
    (fun() ->
       sprintf "async_c2s_e ch=%d data=%B meth=%s"
	 ch (d_opt <> None) (string_of_method (m :> method_t))
    );
  let (eng, signal) = Uq_engines.signal_engine ep.esys in
  let sg =
    function
      | None -> signal (`Done ())
      | Some e -> signal (`Error e) in
  let frames = mk_frames ep (m :> method_t) d_opt ch in
  List.iter (fun fr -> Queue.add (fr,sg) q) frames;
  output_thread ep;
  eng

let async_c2s_s ep m d_opt ch =
  sync (async_c2s_e ep m d_opt) ch

let async_c2s ep m d_opt ch =
  ignore (async_c2s_e ep m d_opt ch)

let sync_c2s_e ?no_wait
               ?(on_timeout = fun () -> ())
               ep (m : sync_client_initiated_method_t) (d_opt : data option)
               ch tmo =
  if ch < 0 || ch > 65535 then
    invalid_arg "Netamqp_endpoint.sync_c2s: bad channel";
  check_up ep;
  dlogr
    (fun() ->
       sprintf "sync_c2s request ch=%d data=%B meth=%s"
	 ch (d_opt <> None) (string_of_method (m :> method_t))
    );
  let q =
    try Hashtbl.find ep.out_q ch
    with Not_found ->
      failwith "Netamqp_endpoint.sync_c2s: this channel is disabled" in
  let eng, notify = Uq_engines.signal_engine ep.esys in
  let sg =
    match no_wait with
      | None -> (fun _ -> ())
      | Some nw ->
	  (function
	     | None -> notify (`Done (nw,None))
	     | Some e -> notify (`Error e) 
	  ) in
  let frames = mk_frames ep (m :> method_t) d_opt ch in
  List.iter (fun fr -> Queue.add (fr,sg) q) frames;
  let resp_mtypes = response_types_of_method (m :> method_t) in
  ( match no_wait with
      | None ->
	  let tmo_g = Unixqueue.new_group ep.esys in
	  let (reg : reg) =
	    ( object
		method reg_callback (m : any_server_to_client_method_t) d_opt =
		  dlogr
		    (fun() ->
		       sprintf "sync_c2s response ch=%d data=%B meth=%s"
			 ch (d_opt <> None) (string_of_method (m :> method_t))
		    );
		let m' =
		  coerce_to_sync_server_to_client_method_t (m :> method_t) in
		notify(`Done(m',d_opt))
		method reg_error err =
		  dlogr
		    (fun() ->
		       sprintf "sync_c2s error ch=%d err=%s"
			 ch (Netexn.to_string err)
		    );
		  notify(`Error err)
		method reg_tmo_group = Some tmo_g
		method reg_once = true
		method reg_sync = true
	      end
	    ) in
	  List.iter
	    (fun resp_mtype ->
	       let l =
		 try Hashtbl.find ep.in_tab (ch,resp_mtype)
		 with Not_found -> [] in
	       Hashtbl.replace ep.in_tab (ch,resp_mtype) (reg :: l)
	    )
	    resp_mtypes;
	  if tmo >= 0.0 then (
	    Unixqueue.once ep.esys tmo_g tmo
	      (fun () ->
		 on_timeout();
		 error_regs_to_channel ep (Some ch) Timeout
	      )
	  )
      | Some nw ->
	  ()
  );
  output_thread ep;
  eng

let sync_c2s_s ?no_wait ?on_timeout ep m d_opt ch tmo =
  sync (sync_c2s_e ?no_wait ?on_timeout ep m d_opt ch) tmo

let register_async_s2c ep (mtype : async_server_to_client_method_type_t) ch cb =
  if ch < 0 || ch > 65535 then
    invalid_arg "Netamqp_endpoint.register_async_s2c: bad channel";
  check_up ~off_ok:true ep;
  if not(is_channel_enabled ep ch) then
    failwith "Netamqp_endpoint.register_async_s2c: this channel is disabled";
  let reg =
    ( object
	method reg_callback m d_opt =
	  dlogr
	    (fun() ->
	       sprintf "async_s2c ch=%d data=%B meth=%s"
		 ch (d_opt <> None) (string_of_method (m :> method_t))
	    );
	  let m' = coerce_to_async_server_to_client_method_t (m :> method_t) in
	  cb m' d_opt
	method reg_error _ = ()
	method reg_tmo_group = None
	method reg_once = false
	method reg_sync = false
      end
    ) in
  let mtype' = (mtype :> any_server_to_client_method_type_t) in
  let l =
    try Hashtbl.find ep.in_tab (ch,mtype')
    with Not_found -> [] in
  Hashtbl.replace ep.in_tab (ch,mtype') (reg :: l)

let register_sync_s2c ep (mtype : sync_server_initiated_method_type_t) ch cb 
                      post_action =
  if ch < 0 || ch > 65535 then
    invalid_arg "Netamqp_endpoint.register_sync_s2c: bad channel";
  check_up ~off_ok:true ep;
  if not(is_channel_enabled ep ch) then
    failwith "Netamqp_endpoint.register_sync_s2c: this channel is disabled";
  let reg =
    ( object
	method reg_callback m d_opt =
	  dlogr
	    (fun() ->
	       sprintf "sync_s2c request ch=%d data=%B meth=%s"
		 ch (d_opt <> None) (string_of_method (m :> method_t))
	    );
	  let m' = coerce_to_sync_server_initiated_method_t (m :> method_t) in
	  let resp_opt = (cb m' : sync_client_to_server_method_t option) in
	  match resp_opt with
	    | None ->
		dlogr
		  (fun () ->
		     sprintf "sync_s2c no response ch=%d" ch
		  )
	    | Some resp ->
		dlogr
		  (fun() ->
		     sprintf "sync_s2c response ch=%d meth=%s"
		       ch (string_of_method (resp :> method_t))
		  );
		let frames = mk_frames ep (resp :> method_t) None ch in
		List.iter (fun fr -> Queue.add fr ep.out_prio_q) frames;
		post_action();
		output_thread ep
	method reg_error _ = ()
	method reg_tmo_group = None
	method reg_once = false
	method reg_sync = false
      end
    ) in
  let mtype' = (mtype :> any_server_to_client_method_type_t) in
  let l =
    try Hashtbl.find ep.in_tab (ch,mtype')
    with Not_found -> [] in
  Hashtbl.replace ep.in_tab (ch,mtype') (reg :: l)

