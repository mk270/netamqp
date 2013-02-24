(* $Id: netamqp_types.mli 53444 2011-03-10 14:08:13Z gerd $ *)

(** General types for AMQP *)

(** {1 Transport} *)

type channel = int
    (** AMQP channels have numbers 0-65535. Channel 0 has a special function *)
    
type transport_type =
    [ `TCP | `SSL ]

type frame_type =
    [ `Proto_header | `Method | `Header | `Body | `Heartbeat ]

type frame =
    { frame_type : frame_type;
      frame_channel : channel;
      frame_payload : Xdr_mstring.mstring list;
    }
  (** A frame is the transport unit on the wire.

      The [frame_payload] may be based on strings or bigarrays. The
      current implementation will always use the string-based 
      representation for received frames ( - in the future this might
      become configurable). For sent frames both representations are
      supported equally well.

      For a type [`Proto_header], the payload consists of the three
      bytes describing the protocol version (major version, minor
      version, revision).
   *)


exception Decode_error of string
  (** Cannot decode data from the server *)

exception Encode_error of string
  (** Cannot encode a message to the binary format *)

exception Not_connected
  (** The endpoint is not in [`Connected] state and cannot accept new 
      requests *)

exception Timeout
  (** Sync calls get timeout *)

exception Method_dropped
  (** The channel or connection is now down, and the method
      cannot be continued
   *)

exception Unexpected_eof
  (** Got an EOF from the server but was not expecting it *)

exception Method_cannot_be_dispatched of channel * string
  (** No handler is defined for this *)

exception Unexpected_frame of frame
  (** This type of frame is unexpected now *)

exception Method_exception of string * int * string
  (** An exception token wrapped as [connection.close] or [channel.close] 
      method

      First arg: name of method
   *)

exception Protocol_is_not_supported
  (** The server does not support our protocol version *)

exception Protocol_violation of string
  (** A violation of the protocol (invalid sequence of methods) *)
  

val string_of_frame_type : frame_type -> string
