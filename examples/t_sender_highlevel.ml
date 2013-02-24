(* This is the sender for receiver_t. Please read the comments there first! 

   Also, when trying this example, make sure the receiver is started first
   because the receiver declares the queue.
 *)

#use "topfind";;
#require "netamqp";;

open Netamqp_types
open Printf
open Globals

let () =
  Netamqp_endpoint.Debug.enable := true;
  Netamqp_transport.Debug.enable := true


let esys = Unixqueue.create_unix_event_system()
let p = `TCP(`Inet(Globals.host, Netamqp_endpoint.default_port))
let ep = Netamqp_endpoint.create p (`AMQP_0_9 `One) esys
let c = Netamqp_connection.create ep
let auth = Netamqp_connection.plain_auth "guest" "guest"

let channel = 1
(*
let qname = "test_xy"
let routing_key = qname ^ "_routing_key"
*)

(* By calling sender this example is started: *)

let sender file_name =
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
  Netamqp_tx.select_s ~channel:co ();
  eprintf "*** Transactions selected\n%!";

  (* This is the data string we send *)
  (* let large = String.make 1000_000 'x' in *)

  let ic = Pervasives.open_in file_name in 
  try 
    while true do 
      let msg = Pervasives.input_line ic in 
      (* The queued message d consists of a header and a body (data string).
	 This is the header. Again see amqp0-9-1.xml for details. The
	 documentation is in the section for the "BASIC" class. There are
	 "field" definitions, each corresponding to one of the following
	 arguments.
      *)
      let msg =
	Netamqp_basic.create_message
	  ~content_type:"text/plain"
	  ~content_encoding:"ISO-8859-1"
	  ~headers: [ "foo", `Longstr "foofield";
		      "bar", `Bool true;
		      "baz", `Sint4 (Rtypes.int4_of_int 0xdd);
		    ]
	  ~delivery_mode:1   (* non-persistent *)
	  [Netamqp_rtypes.mk_mstring (* (sprintf "Loop %d" n) *) msg] in

    (* Publish d = send it to the exchange "amq.direct" which will identify
       the right queue by routing_key.
    *)
      Netamqp_basic.publish_s
	~channel:co
	~exchange:Globals.exchange
	~routing_key:Globals.routing_key
	msg;

    (* Now do the Tx-commit: *)
      Netamqp_tx.commit_s ~channel:co ();
      eprintf "*** Message published!\n%!";

      Unix.sleep 1
    done
  with End_of_file -> 
    begin
      (* How to close everything from the client side: *)
      Pervasives.close_in ic;

      Netamqp_channel.close_s co;
      eprintf "*** Channel could be closed!\n%!";

      Netamqp_connection.close_s c;
      eprintf "*** Connection could be closed!\n%!"
    end
