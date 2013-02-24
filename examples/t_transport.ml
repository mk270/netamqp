#use "topfind";;
#require "netamqp" ;;
open Netamqp_types;;

Netamqp_transport.Debug.enable := true;;

let esys = Unixqueue.create_unix_event_system();;

let sock =
  Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0;;

Unix.connect sock (Unix.ADDR_INET(Unix.inet_addr_loopback, 5672));;

let c = Netamqp_transport.tcp_amqp_multiplex_controller sock esys;;

let mk_mstring s =
  Xdr_mstring.string_based_mstrings # create_from_string
    s 0 (String.length s) false ;;

let f1 =
  { frame_type = `Proto_header;
    frame_channel = 0;
    frame_payload = mk_mstring "\000\009\001";
  };;

c # start_writing
  ~when_done:(function
		| `Ok () ->
		    print_endline "Sent proto-header"
		| `Error e ->
		    print_endline ("Proto-header: " ^ Netexn.to_string e)
	     )
  f1 ;;

Unixqueue.run esys;;

let frame = ref None;;

c # start_reading 
  ~when_done:(function 
		| `Ok f ->
		    frame := Some f;
		    ( match f.frame_type with
			| `Method ->
			    print_endline "Got method"
			| `Header ->
			    print_endline "Got header"
			| `Body ->
			    print_endline "Got body"
			| `Heartbeat ->
			    print_endline "Got heartbeat"
			| `Proto_header ->
			    print_endline "Got proto header"
		    )
		| `Error e ->
		    print_endline ("Start resp: " ^ Netexn.to_string e)
		| `End_of_file ->
		    print_endline "Start: EOF"
	     )
  () ;;

Unixqueue.run esys;;
