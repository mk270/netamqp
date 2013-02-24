(* $Id: netamqp_types.ml 53347 2011-03-01 00:38:28Z gerd $ *)

type channel = int

type transport_type =
    [ `TCP | `SSL ]

type frame_type =
    [ `Proto_header | `Method | `Header | `Body | `Heartbeat ]

type frame =
    { frame_type : frame_type;
      frame_channel : channel;
      frame_payload : Xdr_mstring.mstring list
    }

exception Decode_error of string
exception Encode_error of string
exception Not_connected
exception Timeout
exception Method_dropped
exception Unexpected_eof
exception Method_cannot_be_dispatched of channel * string
exception Method_exception of string * int * string
exception Protocol_is_not_supported
exception Protocol_violation of string
exception Unexpected_frame of frame

open Printf

let string_of_frame_type =
  function
    | `Proto_header -> "Proto_header"
    | `Method -> "Method"
    | `Header -> "Header"
    | `Body -> "Body"
    | `Heartbeat -> "Heartbeat"

(* Exception printers: *)
let () =
  Netexn.register_printer
    (Unexpected_frame { frame_type=`Proto_header; 
			frame_channel=0; 
			frame_payload=[]
		      }
    )
    (fun e ->
       match e with
	 | Unexpected_frame f ->
	     sprintf "<frame t=%s ch=%d #payload=%d>"
	       (string_of_frame_type f.frame_type)
	       f.frame_channel
	       (Xdr_mstring.length_mstrings f.frame_payload)
	 | _ -> "_"
    )
