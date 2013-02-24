(* $Id: netamqp_connection.mli 53444 2011-03-10 14:08:13Z gerd $ *)

(** Manage AMQP connections *)

(** A connection on the AMQP level is a managed version of the transport
    stream (i.e. the TCP connection). There is a handshake at
    connection establishment time where protocol details are negotiated
    and the client is authenticated.

    There is another handshake at connection close time where one peer
    initiates the closure and the other peer has to approve. Either
    peer can initiate closure. It is possible to transmit error codes
    at connection close time.

    The transport stream is automatically closed when the close handshake
    has finished.

    In this implementation, connection management is separated from
    endpoint management. The latter is done in {!Netamqp_endpoint},
    and describes the AMQP client on a lower level. 
 *)


exception Not_open
  (** Raised if the connection is not known to be open on the client side *)

exception Error of string
  (** An error occurred in connection management *)

type connection
  (** The connection management object *)

type auth_method
  (** An authentication method, including client identity and credentials *)

type locale_preference =
    [ `Pref of string
    | `Select of string list -> string
    ]
  (** One can select the locale for error messages coming from the server:
      - [`Pref p]: Prefer the locale [p] if possible, otherwise choose the
        first locale offered by the server
      - [`Select f]: Call the function [f] with all possibilities to select
        the locale.

      Locales are identified as in Unix, e.g. "en_US".
   *)

val create : Netamqp_endpoint.endpoint -> connection
  (** Create the connection management object for the endpoint. The endpoint
      should be in [`Off] state. The returned connection object is 
      still closed, and may now be opened.
   *)

val open_e : connection ->
             auth_method list ->
             locale_preference ->
             string ->
                  unit Uq_engines.engine
  (** Opens a connection: The endpoint is instructed to connect on the
      transport level, to negotiate protocol parameters, and to
      authenticate the client.

      The string argument is the virtual host of the AMQP server to
      connect to, e.g. "/".
   *)

val open_s : connection ->
             auth_method list ->
             locale_preference ->
             string ->
                unit
  (** Same as synchronous function *)

val close_e : connection -> unit Uq_engines.engine
val close_s : connection -> unit
  (** Request client side closure. *)

val is_open : connection -> bool
  (** The connection is first open after the open handshake has been
      completed, i.e. when [open_e] or [open_s] are done.

      Note that the peer can close channels at
      any time, so there is no guarantee that the channel is still open
      when [is_open] returns [true].

      Checking [is_open] is mainly useful for recovery after exceptions.
      Some exceptions close the channel, some do not.
   *)

(*
val status : connection -> 
      (int * string * Netamqp_endpoint.method_type_t) option
 *)

(* TODO: hooks for status changes etc *)

val plain_auth : string -> string -> auth_method
  (** [plain_auth username password]: use [PLAIN] authentication.
      
      Note that username and password are not encrypted on the wire!
   *)

val endpoint : connection -> Netamqp_endpoint.endpoint
  (** Return the endpoint the connection uses *)

val event_system : connection -> Unixqueue.event_system
  (** Return the event system the connection uses *)
