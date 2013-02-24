(* $Id: netamqp_transport.ml 53347 2011-03-01 00:38:28Z gerd $
 * ----------------------------------------------------------------------
 *
 * This module is derived from rpc_transport.ml, which is published
 * as part of Ocamlnet, and whose copyright holder is Gerd Stolpmann.
 *
 *)

open Netamqp_types
open Printf


type 't result =
    [ `Ok of 't
    | `Error of exn
    ]

type 't result_eof =
    [ 't result
    | `End_of_file
    ]


type sockaddr =
    [ `Implied
    | `Sockaddr of Unix.sockaddr
    ]

let string_of_sockaddr =
  function
    | `Implied -> "<implied>"
    | `Sockaddr sa -> Netsys.string_of_sockaddr sa

exception Error of string


class type amqp_multiplex_controller =
object
  method alive : bool
  method event_system : Unixqueue.event_system
  method getsockname : sockaddr
  method getpeername : sockaddr
  method transport_type : transport_type
  method set_max_frame_size : int -> unit
  method eff_max_frame_size : int
  method reading : bool
  method read_eof : bool
  method start_reading : 
    when_done:( frame result_eof -> unit) -> unit -> unit
  method cancel_rd_polling : unit -> unit
  method abort_rw : unit -> unit
  method writing : bool
  method start_writing :
    when_done:(unit result -> unit) -> frame -> unit
  method start_shutting_down :
    when_done:(unit result -> unit) -> unit -> unit
  method cancel_shutting_down : unit -> unit
  method set_timeout : notify:(unit -> unit) -> float -> unit
  method inactivate : unit -> unit
end

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netamqp_transport" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netamqp_transport" Debug.enable

let () =
  Netlog.Debug.register_module "Netamqp_transport" Debug.enable


let mem_size = Netsys_mem.pool_block_size Netsys_mem.default_pool
  (* for allocated bigarrays *)

let fallback_size = 16384   (* for I/O via Unix *)

let mem_alloc() =
  Netsys_mem.pool_alloc_memory Netsys_mem.default_pool


let mem_dummy() =
  Bigarray.Array1.create
    Bigarray.char Bigarray.c_layout 0

let mk_mstring s =
  Xdr_mstring.string_based_mstrings # create_from_string
    s 0 (String.length s) false
  
exception Continue of (unit -> unit)


class tcp_amqp_multiplex_controller sockname peername 
        (mplex : Uq_engines.multiplex_controller) esys 
      : amqp_multiplex_controller =
  let () = 
    dlogr (fun () ->
	     sprintf "new tcp_amqpQ_multiplex_controller mplex=%d"
	       (Oo.id mplex))
  in
  let lim_frame_size =
    if Sys.word_size = 64 then
      Int64.to_int 0xffff_ffffL
    else
      Sys.max_string_length in
object(self)
  val mutable rd_buffer = Netpagebuffer.create mem_size
  val mutable rd_buffer_nomem = 
    if mplex#mem_supported then "" else String.create fallback_size

  val mutable rd_mode = `Frame_header 0
  val mutable rd_processing = false
  val mutable rd_stream_at_beginning = true

  val mutable max_frame_size = lim_frame_size

  method alive = mplex # alive
  method event_system = esys
  method getsockname = sockname
  method getpeername = peername
  method transport_type = `TCP

  method set_max_frame_size size =
    if size < 255 then
      failwith "Netamqp_transport.set_max_frame_size: too low";
    max_frame_size <- min lim_frame_size size

  method eff_max_frame_size = max_frame_size

  method reading = mplex # reading
  method read_eof = mplex # read_eof
  method writing = mplex # writing

  val mutable aborted = false

  method start_reading ~when_done () =
    assert(not mplex#reading);

    let rec est_reading() =
      let mplex_when_done exn_opt n =
	self # timer_event `Stop `R;
	match exn_opt with
	  | None ->
	      process ()
	  | Some End_of_file ->
	      if rd_mode = `Frame_header 0 && Netpagebuffer.length rd_buffer=0
	      then
		return_eof()   (* EOF between messages *)
	      else
		return_error (Error "EOF within message")
	  | Some Uq_engines.Cancelled ->
	      ()   (* Ignore *)
	  | Some error ->
	      return_error error 
      in
      
      rd_processing <- false;
      if mplex#mem_supported then (
	let (b, start, len) = Netpagebuffer.page_for_additions rd_buffer in
	mplex # start_mem_reading 
	  ~when_done:(fun exn_opt n ->
			dlogr (fun () ->
				 sprintf "Reading [mem]: %s%s"
				   (Rpc_util.hex_dump_m b start (min n 200))
				   (if n > 200 then "..." else "")
			      );
			Netpagebuffer.advance rd_buffer n;
			mplex_when_done exn_opt n
		     )
	  b
	  start
	  len
      )
      else (
	mplex # start_reading
	  ~when_done:(fun exn_opt n ->
			dlogr (fun () ->
				 sprintf "Reading [str]: %s%s"
				   (Rpc_util.hex_dump_s 
				      rd_buffer_nomem 0 (min n 200))
				   (if n > 200 then "..." else "")
			      );
			Netpagebuffer.add_sub_string 
			  rd_buffer rd_buffer_nomem 0 n;
			mplex_when_done exn_opt n
		     )
	  rd_buffer_nomem
	  0
	  (String.length rd_buffer_nomem)
      );
      self # timer_event `Start `R

    and process () =
      rd_processing <- true;
      let len = Netpagebuffer.length rd_buffer in
      match rd_mode with
	| `Frame_header n ->
	    (* n: we already saw n bytes *)
	    let n' = min len 7 in
	    rd_mode <- `Frame_header n';
	    if n' = 7 then (
	      (* Decode the header. If we are at the beginning of the
		 stream, it is also possible that we see a protocol header
		 (with 8 bytes)
	       *)
	      try
		let s = Netpagebuffer.sub rd_buffer 0 7 in
		let frame_type =
		  match s.[0] with
		    | '\001' -> `Method
		    | '\002' -> `Header
		    | '\003' -> `Body
		    | '\008' -> `Heartbeat
		    | '\065' when rd_stream_at_beginning -> `Proto_header
		    | _ -> raise(Error "Bad frame header") in
		if frame_type = `Proto_header then (
		  if String.sub s 0 5 = "AMQP\000" then (
		    if len >= 8 then (
		      let p = Netpagebuffer.sub rd_buffer 5 3 in
		      let frame =
			{ frame_type = `Proto_header;
			  frame_channel = 0;
			  frame_payload = [mk_mstring p]
			} in
		      Netpagebuffer.delete_hd rd_buffer 8;
		      raise (Continue (fun () -> return_msg frame))
		    )
		    else raise (Continue est_reading)
		  )
		  else
		    raise(Error "Bad frame header")
		)
		else (
		  let channel =
		    Netamqp_rtypes.read_uint2_unsafe s 1 in
		  let size =
		    Rtypes.read_uint4_unsafe s 3 in
		  let max_size =
		    Rtypes.uint4_of_int (max_frame_size-8) in
		  if Rtypes.gt_uint4 size max_size then
		    raise(Error "Frame too long");
		  let size =
		    Rtypes.int_of_uint4 size in
		  rd_mode <- `Payload(frame_type, channel, size, 7)
		);
		raise (Continue process)
	      with
		| Continue f -> (* call f at tail position *)
		    f()
		| error ->
		    return_error error
	    )
	    else est_reading()
	| `Payload(frame_type, channel, size, payload_start) ->
	    if len >= size+payload_start+1 then (
	      let trailer =
		Netpagebuffer.sub rd_buffer (payload_start+size) 1 in
	      if trailer = "\xCE" then (
		let data =
		  Netpagebuffer.sub rd_buffer payload_start size in
		let ms =
		  mk_mstring data in
		let frame = 
		  { frame_type = frame_type;
		    frame_channel = channel;
		    frame_payload = [ms]
		  } in
		Netpagebuffer.delete_hd rd_buffer (payload_start+size+1);
		rd_mode <- `Frame_header 0;
		return_msg frame
	      )
	      else return_error (Error "Bad frame end")
	    )
	    else est_reading()

    and return_msg msg =
      rd_stream_at_beginning <- false;
      if not aborted then
	when_done (`Ok msg)

    and return_error e =
      rd_processing <- false;
      if not aborted then
	when_done (`Error e)

    and return_eof () =
      rd_processing <- false;
      if not aborted then
	when_done `End_of_file 

    in
    if rd_processing then
      process ()
    else
      est_reading()
	    

  method start_writing ~when_done frame =

    assert(not mplex#writing);

    (* - `String(s,p,l): We have still to write s[p] to s[p+l-1]
       - `Memory(m,p,l,ms,q): We have still to write
          m[p] to m[p+l-1], followed by ms[q] to end of ms
          (where ms is the managed string)
     *)

    let item_of_mstring ms r =
      (* Create the item for ms, starting at offset r *)
      let l = ms#length in
      assert(r <= l);
      match ms # preferred with
	| `String ->
	    let (s,pos) = ms#as_string in (* usually only r=0 *)
	    `String(s,pos+r,l-r)
	| `Memory ->
	    if mplex#mem_supported then (
	      let (m,pos) = ms#as_memory in
	      `Memory(m, pos+r, l-r, ms, l)
	    )
	    else
	      let (s,pos) = ms#as_string in
	      `String(s,pos+r,l-r) in

    let rec optimize_items items =
      (* Merge adjacent short items (only for strings) *)
      match items with
	| (`String(s1,p1,l1) as i1) :: (`String(s2,p2,l2) as i2) :: items' ->
	    if l1 < 256 && l2 < 256 then (
	      let b = Buffer.create (l1+l2) in
	      Buffer.add_substring b s1 p1 l1;
	      Buffer.add_substring b s2 p2 l2;
	      gather_items b items'
	    )
	    else
	      i1 :: optimize_items (i2 :: items')
	| other :: items' ->
	    other :: optimize_items items'
	| [] ->
	    []

    and gather_items b items =
      match items with
	| `String(s,p,l) :: items' when l < 256 ->
	    Buffer.add_substring b s p l;
	    gather_items b items'
	| _ ->
	    `String(Buffer.contents b, 0, Buffer.length b) :: 
	      optimize_items items in


    let item_is_empty =
      function
	| `String(_,_,l) -> l=0
	| `Memory(_,_,l,ms,q) -> l=0 && ms#length=q in

    let rec est_writing item remaining =
      (* [item] is the current buffer to write; and [remaining] need to be
	 written after that
       *)
      let mplex_when_done exn_opt n = (* n bytes written *)
	self # timer_event `Stop `W;
	match exn_opt with
	  | None ->
	      ( match item with
		  | `Memory(m,p,l,ms,q) ->
		      let l' = l-n in
		      if l' > 0 then
			est_writing (`Memory(m,p+n,l',ms,q)) remaining
		      else (
			let mlen = ms#length in
			if q < mlen then
			  let item' = item_of_mstring ms q in
			  est_writing item' remaining
			else
			  est_writing_next remaining
		      )
		  | `String(s,p,l) ->
		      let l' = l-n in
		      if l' > 0 then
			est_writing (`String(s,p+n,l')) remaining
		      else 
			est_writing_next remaining
	      )
	  | Some Uq_engines.Cancelled ->
	      ()  (* ignore *)
	  | Some error ->
	      if not aborted then
		when_done (`Error error)
      in

      ( match item with
	  | `Memory(m,p,l,_,_) ->
	      dlogr (fun () ->
		       sprintf "Writing [mem]: %s%s" 
			 (Rpc_util.hex_dump_m m p (min l 200))
			 (if l > 200 then "..." else "")
		    );
	      mplex # start_mem_writing
		~when_done:mplex_when_done m p l
	  | `String(s,p,l) ->
	      dlogr (fun () ->
		       sprintf "Writing [str]: %s%s" 
			 (Rpc_util.hex_dump_s s p (min l 200))
			 (if l > 200 then "..." else "")
		    );
	      mplex # start_writing
		~when_done:mplex_when_done s p l
      );
      self # timer_event `Start `W

    and  est_writing_next remaining =
      match remaining with
	| item :: remaining' ->
	    if item_is_empty item then
	      est_writing_next remaining'
	    else
	      est_writing item remaining'
	| [] ->
	    if not aborted then
	      when_done (`Ok ())
    in

    let write mstrings =
      est_writing_next
	(optimize_items
	   (List.map (fun ms -> item_of_mstring ms 0) mstrings)) in

    match frame.frame_type with
      | `Proto_header ->
	  let s = Xdr_mstring.concat_mstrings frame.frame_payload in
	  if String.length s <> 3 then
	    raise(Error "The `Proto_header frame requires a 3-byte payload");
	  let u = "AMQP\000" ^ s in
	  write [mk_mstring u]
      | _ ->
	  (* Create frame header and frame end mstrings: *)
	  let l = Xdr_mstring.length_mstrings frame.frame_payload in
	  if l > max_frame_size then (
	    dlogr 
	      (fun () -> sprintf "l=%d max_frame_size=%d" l max_frame_size);
	    raise(Error "The frame is too large")
	  );
	  let s = String.create 7 in
	  let c0 = 
	    match frame.frame_type with
	      | `Method -> '\001'
	      | `Header -> '\002'
	      | `Body -> '\003'
	      | `Heartbeat -> '\008'
	      | `Proto_header -> assert false in
	  s.[0] <- c0;
	  Netamqp_rtypes.write_uint2 s 1 frame.frame_channel;
	  Rtypes.write_uint4 s 3 (Rtypes.uint4_of_int l);
	  let header = mk_mstring s in
	  let trailer = mk_mstring "\xCE" in
	  write (header :: (frame.frame_payload @ [trailer]))

  method cancel_rd_polling () =
    if mplex#reading then
      mplex # cancel_reading()

  method abort_rw () =
    aborted <- true;
    mplex # cancel_reading();
    mplex # cancel_writing()
    
  method start_shutting_down ~when_done () =
    dlogr (fun () ->
	     sprintf "start_shutting_down mplex=%d"
	       (Oo.id mplex));
    mplex # start_shutting_down
      ~when_done:(fun exn_opt ->
		    dlogr (fun () ->
			     sprintf "done shutting_down mplex=%d"
			       (Oo.id mplex));
		    self # timer_event `Stop `D;
		    match exn_opt with
		      | None -> when_done (`Ok ())
		      | Some error -> when_done (`Error error)
		 )
      ();
    self # timer_event `Start `D

  method cancel_shutting_down () =
    self # timer_event `Stop `D;
    mplex # cancel_shutting_down()

  method inactivate () =
    dlogr (fun () ->
	     sprintf "inactivate mplex=%d"
	       (Oo.id mplex));
    self # stop_timer();
    mplex # inactivate()

  val mutable timer = None
  val mutable timer_r = `Stop
  val mutable timer_w = `Stop
  val mutable timer_d = `Stop
  val mutable timer_group = None

  method set_timeout ~notify tmo =
    timer <- Some(notify, tmo)

  method private timer_event start_stop which =
    ( match timer with
	| None -> ()
	| Some(notify, tmo) ->
	    ( match which with
		| `R -> timer_r <- start_stop
		| `W -> timer_w <- start_stop
		| `D -> timer_d <- start_stop
	    );
	    self # stop_timer();
	    if timer_r = `Start || timer_w = `Start || timer_d = `Start then (
	      let g = Unixqueue.new_group esys in
	      timer_group <- Some g;
	      Unixqueue.once esys g tmo
		(fun () -> 
		   timer_group <- None;
		   notify()
		)
	    );
    )


  method private stop_timer() =
    ( match timer_group with
	| None -> ()
	| Some g -> Unixqueue.clear esys g
    );
    timer_group <- None;
    timer_r <- `Stop;
    timer_w <- `Stop;
    timer_d <- `Stop

end



let tcp_amqp_multiplex_controller ?(close_inactive_descr=true)
                                  ?(preclose=fun()->()) fd esys =
  let sockname = 
    try
      `Sockaddr(Unix.getsockname fd) 
    with
      | Unix.Unix_error(_,_,_) -> `Implied in
  let peername = 
    try
      `Sockaddr(Netsys.getpeername fd)
    with
      | Unix.Unix_error(_,_,_) -> `Implied in
  let mplex = 
    Uq_engines.create_multiplex_controller_for_connected_socket
      ~close_inactive_descr ~preclose
      fd esys in
  new tcp_amqp_multiplex_controller sockname peername mplex esys
;;
