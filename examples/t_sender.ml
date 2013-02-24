(* This is the sender for receiver_t. Please read the comments there first! 

   Also, when trying this example, make sure the receiver is started first
   because the receiver declares the queue.
 *)

#use "topfind";;
#require "netamqp";;

open Netamqp_types
open Printf

let () =
  Netamqp_endpoint.Debug.enable := true;
  Netamqp_transport.Debug.enable := true


let esys = Unixqueue.create_unix_event_system()
let p = `TCP(`Inet("localhost", Netamqp_endpoint.default_port))
let ep = Netamqp_endpoint.create p (`AMQP_0_9 `One) esys
let c = Netamqp_connection.create ep
let auth = Netamqp_connection.plain_auth "guest" "guest"

let channel = 1
let qname = "test_xy"
let routing_key = qname ^ "_routing_key"


(* By calling sender this example is started: *)

let sender() =
  (* Preparation - same as in t_receiver *)
  Netamqp_connection.open_s c [ auth ] (`Pref "en_US") "/";
  eprintf "*** Connection could be opened, and the proto handshake is done!\n%!";

  let co = Netamqp_channel.open_s c channel in
  eprintf "*** Channel could be opened!\n%!";

  (* We declare now that we want to use the transactional interface provided
     by the Tx class. Basically, we could also do without. However, note
     that the Basic-publish method is asynchronous (we call it below), and
     thus there is no immediate feedback whether publication worked. Even
     worse, the Netamqp client does not even send out async methods immediately,
     but first when the event loop runs.

     The Tx-commit method allows us to wait until the publication is
     really done, and we have also an additional check on errors: If
     something did not work, we would get a Channel-close at commit time,
     together with an error code.

     For enabling transactions we call Tx-select and expect Tx-select-ok.
   *)
  let (r, _) =
    Netamqp_endpoint.sync_c2s_s
      ep
      (`AMQP_0_9 (`Tx_select))
      None
      channel
      (-1.0) in
  ( match r with
      | `AMQP_0_9 (`Tx_select_ok) ->
	  ()
      | _ -> 
	  assert false
  );

  eprintf "*** Transactions selected\n%!";

  (* This is the data string we send *)
  let large = String.make 1000_000 'x' in

  for n = 1 to 100 do
    (* The queued message d consists of a header and a body (data string).
       This is the header. Again see amqp0-9-1.xml for details. The
       documentation is in the section for the "BASIC" class. There are
       "field" definitions, each corresponding to one of the following
       arguments.
     *)
    let header =
      `AMQP_0_9
	(`P_basic
	   ( Some "text/plain",
	     Some "ISO-8859-1",
	     Some [ "foo", `Longstr "foofield";
		    "bar", `Bool true;
		    "baz", `Sint4 (Rtypes.int4_of_int 0xdd);
		  ],
	     Some 1,   (* non-persistent *)
	     Some 0,   (* priority *)
	     None,
	     None,
	     None,
	     None,
	     None,
	     None,
	     None,
	     None,
	     None
	   )
	) in
    (* d is the queued message. Note that the body is actually a list of
       mstring (see t_receiver.ml for explanations).
     *)
    let d =
      (header,
       [Netamqp_rtypes.mk_mstring (* (sprintf "Loop %d" n) *) large]
      ) in

    (* Publish d = send it to the exchange "amq.direct" which will identify
       the right queue by routing_key.
     *)
    Netamqp_endpoint.async_c2s
      ep
      (`AMQP_0_9(`Basic_publish(0, "amq.direct", routing_key,
				false, false)))
      (Some d)
      channel;

    (* Now do the Tx-commit/Tx-commit-ok sequence: *)
    let (r, _) =
      Netamqp_endpoint.sync_c2s_s
	ep
	(`AMQP_0_9 (`Tx_commit))
	None
	channel
	(-1.0) in
    ( match r with
	| `AMQP_0_9 (`Tx_commit_ok) ->
	    ()
	| _ -> 
	    assert false
    );

    eprintf "*** Message published!\n%!";

    Unix.sleep 1
  done;

  (* How to close everything from the client side: *)

  Netamqp_channel.close_s co;
  eprintf "*** Channel could be closed!\n%!";

  Netamqp_connection.close_s c;
  eprintf "*** Connection could be closed!\n%!"

