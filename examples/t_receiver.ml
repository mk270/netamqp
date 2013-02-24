(* This example creates a queue, and continously pulls messages from
   the queue. It does not put any messages onto the queue, though.
   Use t_sender.ml to do this.
 *)


#use "topfind";;
#require "netamqp";;

open Netamqp_types
open Printf

let () =
  Netamqp_endpoint.Debug.enable := true;
  Netamqp_transport.Debug.enable := true


let esys = Unixqueue.create_unix_event_system()

(* We assume there is a RabbitMQ on localhost, listening on the default
   port:
 *)
let p = `TCP(`Inet("localhost", Netamqp_endpoint.default_port))
let ep = Netamqp_endpoint.create p (`AMQP_0_9 `One) esys
let c = Netamqp_connection.create ep

(* In RabbitMQ there is a built-in default user, "guest". The password
   is also "guest". We authenticate as this user.
 *)
let auth = Netamqp_connection.plain_auth "guest" "guest"

(* For this application we use channel 1 on the created connection: *)
let channel = 1

(* The name of the queue: *)
let qname = "test_xy"

(* The routing key says how the queue can be reached (the address): *)
let routing_key = qname ^ "_routing_key"


(* Call the following function to start the receiver. The function does
   not finish, type CTRL-C to force it
 *)

let receiver() =
  (* At this point we create the TCP connection and establish the
     AMQP-managed connection logic. "en_US" is the locale of server-generated
     error messages. "/" is the virtual host.
   *)
  Netamqp_connection.open_s c [ auth ] (`Pref "en_US") "/";
  eprintf "*** Connection could be opened, and the proto handshake is done!\n%!";

  (* Now open the data channel. Channels are multiplexed over connections *)
  let co = Netamqp_channel.open_s c channel in
  eprintf "*** Channel could be opened!\n%!";

  (* We declare the queue. This happens by sending a Queue-declare message
     to the server and expecting a Queue-declare-ok message as response.
     These control messages are also called methods. "Queue" is the class.
     For documentation see the file amqp0-9-1.xml, and for the exact
     Ocaml typing Netamqp_method_0_9.mli.

     The functions sync_c2s_e/s are designed for request/response pairs
     where the first method is emitted on the client side (c2s = client
     to server). The "_e" variant (not used here) makes use of an
     Ocamlnet engine. The "_s" variant waits until the response arrives.

     What we effectively do: We create a queue if it not already exists with
     name qname. We enable the auto-delete feature - the queue is deleted
     when the last accessor is closed.
   *)
  let (r, _) =
    Netamqp_endpoint.sync_c2s_s
      ep
      (`AMQP_0_9 (`Queue_declare(0, qname, false, false, false, 
				 (* auto-delete: *) true, false,
				 [])))
      None    (* This value would allow to send content data along with the 
		 method. Only certain methods permit this, though.
	       *)
      channel
      (-1.0)  (* timeout *) in
  ( match r with
      | `AMQP_0_9 (`Queue_declare_ok(_,_,_)) -> ()
      | _ -> assert false
  );

  eprintf "*** Queue declared!\n%!";


  (* Another call: We bind the queue to an exchange. The exchange determines
     which messages are routed to which queue. There are pre-declared
     exchanges, and we use here "amq.direct". This is a direct exchange
     meaning that all content messages with the given routing_key are
     added to the queue.
   *)
  let (r, _) =
    Netamqp_endpoint.sync_c2s_s
      ep
      (`AMQP_0_9 (`Queue_bind(0, qname, (* exchange: *) "amq.direct",
			      routing_key,
			      false, [])))
      None
      channel
      (-1.0) in
  ( match r with
      | `AMQP_0_9 (`Queue_bind_ok) ->
	  ()
      | _ -> 
	  assert false
  );

  eprintf "*** Queue binding established!\n%!";

  (* We want now to achieve that we get all messages arriving at the queue.
     In order to do so, we have to tell the server that we consume
     from the queue. This is actually done in the next code block below.
     First, we configure what happens when messages arrive. (If we did
     not do this, the methods from the server carrying the queue messages
     would be dropped because of the missing registration.)

     The server will send us a Basic-deliver method for each queue message,
     and this method carries the data of the message as additional
     content payload. We register here a handler so all Basic-deliver
     methods arriving on the channel will be forwarded to our callback
     function.

     The payload data is made available in d_opt. Normally, this is
     always None, but if the method carries content, it is
     [Some(header,body)]. The header is here [`P_basic(...)] with a 
     lot of arguments (P=properties, and "basic" because we are
     using the Basic class for message handling). See t_sender.ml
     how the header looks exactly.

     The body is not a string but a list of mstring. The mstring object
     is an abstraction defined in the Ocamlnet library "rpc" 
     (Xdr_mstring). It is generally used for large binary data strings.
     It has two interesting features: First, it can not only be backed
     by normal strings to store the data blob but also by bigarrays of
     char. (There is special support in Ocamlnet for these bigarrays,
     also called "memory" there, e.g. there are special versions of
     Unix.read and Unix.write without any size limits and without any
     data copying in the ocaml wrapper.) The second feature is that
     an mstring can also pick any substring of the base representation
     as content. In general, the mstring abstraction avoids string
     copying. There are a number of helper functions in Xdr_mstring
     and also in Netamqp_rtypes.

     Each AMQP queue message needs to be acknowledged (unless this is
     turned off). An ACK is done by sending the Basic-ack method with
     the same delivery_tag we got in Basic-deliver. If we did not
     ACK we would not get the next queue message. (N.B. One can configure
     this with the Basic-qop method.)

     We see here two new ways of handling methods:
     - register_async_s2c: install a callback so that a function is
       invoked when a certain type of method arrives. This is only
       for one-way methods ("async methods" in AMQP speak).
     - async_c2s: send a one-way method to the server
   *)
  Netamqp_endpoint.register_async_s2c
    ep
    (`AMQP_0_9 `Basic_deliver)
    channel
    (fun m d_opt ->
       match m with
	 | `AMQP_0_9 (`Basic_deliver(_, delivery_tag, _, _, _) )->
	     eprintf "*** Got message!%!";
	     ( match d_opt with
		 | None ->
		     eprintf "*** No data, though\m%!"
		 | Some(header,body) ->
		     let n = Xdr_mstring.length_mstrings body in
		     eprintf "*** DATA: %s\n" 
		       (if n > 100 then
			  sprintf "[size: %d]" n
			else
			  Xdr_mstring.concat_mstrings body
		       )
	     );
	     (* ACK this message *)
	     Netamqp_endpoint.async_c2s
	       ep
	       (`AMQP_0_9(`Basic_ack(delivery_tag, false)))
	       None
	       channel
		     
	 | _ ->
	     assert false
    );

  (* After we registered the handler, we can enable queue consumption.
     This is done by calling Basic-consume and expecting Basic-consume-ok
     as response.

     The "tag" is the consumption tag. Useful for cancelling consumption.
   *)
  let (r, _) =
    Netamqp_endpoint.sync_c2s_s
      ep
      (`AMQP_0_9 (`Basic_consume(0, qname, "", false, false, false, 
				 false, [] )))
      None
      channel
      (-1.0) in
  let consumer_tag =
    match r with
      | `AMQP_0_9 (`Basic_consume_ok tag) -> tag
      | _ -> assert false in

  eprintf "*** Created consumer\n%!";

  (* As data reception is asynchronous business we need to run the
     event system to activate it. Note that this event loop runs
     forever.
   *)
  Unixqueue.run esys;
  
  co




    
    
let close co =
  if Netamqp_channel.is_open co then (
    Netamqp_channel.close_s co;
    eprintf "*** Channel could be closed!\n%!";
  );
  Netamqp_connection.close_s c;
  eprintf "*** Connection could be closed!\n%!"
