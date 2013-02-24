(* $Id: netamqp_channel.mli 53444 2011-03-10 14:08:13Z gerd $ *)

(** Manage AMQP channels *)

(** A channel is a data path between the client and the server. Several
    channels are multiplexed over a single TCP connection.

    The client chooses the channel number identifying the channel.
    It is allowed to reuse channel numbers after closing a channel.

    Channels are equipped with flow control: If the server demands to
    send content data slower, the client will respond to this. There
    is no configuration necessary to enable this feature.

    Channels can be closed by either peer. It is possible to transmit
    error codes at channel close time.
 *)

exception Not_open
  (** Raised if the channel is not known to be open on the client side *)

exception Error of string
  (** An error occurred in channel management *)

type channel = int
  (** A channel number is an int from 0 to 65535. Channel 0 is reserved, so
      channel numbers >= 1 can be used by the application.
   *)

type channel_obj
  (** A channel management object *)

val open_e : Netamqp_connection.connection ->
             channel ->
               channel_obj Uq_engines.engine
  (** Opens a channel on this connection. Fails if the channel is already
      known to be open on the client side.
   *)

val open_s : Netamqp_connection.connection ->
             channel ->
               channel_obj
  (** Same as synchronous call *)

val open_next_e : Netamqp_connection.connection -> channel_obj Uq_engines.engine
val open_next_s : Netamqp_connection.connection -> channel_obj
  (** Opens a channel, and chooses the channel number automatically. *)

val close_e : channel_obj -> unit Uq_engines.engine
  (** Closes the channel. Raises {!Netamqp_channel.Not_open} if the
      channel is not open.
   *)

val close_s : channel_obj -> unit
  (** Same as synchronous call *)

val is_open : channel_obj -> bool
  (** Whether a channel is open. Note that the peer can close channels at
      any time, so there is no guarantee that the channel is still open
      when [is_open] returns [true].

      Checking [is_open] is mainly useful for recovery after exceptions.
      Some exceptions close the channel, some do not.
   *)

val configure_sync_timeout : channel_obj -> float -> unit
  (** Configure the timeout for synchronous request/response methods.
      Defaults to 60 seconds. When timing out the channel is closed,
      and all pending synchronous calls get the {!Netamqp_types.Timeout}
      exception.
   *)

(*
val status : channel_obj -> 
      (int * string * Netamqp_endpoint.method_type_t) option
 *)

val sync_c2s_e : ?no_wait:Netamqp_endpoint.sync_server_to_client_method_t ->
                 channel_obj ->
                 Netamqp_endpoint.sync_client_initiated_method_t ->
                 Netamqp_endpoint.data option ->
                   (Netamqp_endpoint.sync_server_to_client_method_t * 
		      Netamqp_endpoint.data option) Uq_engines.engine
val sync_c2s_s : ?no_wait:Netamqp_endpoint.sync_server_to_client_method_t ->
                 channel_obj ->
                 Netamqp_endpoint.sync_client_initiated_method_t ->
                 Netamqp_endpoint.data option ->
                   (Netamqp_endpoint.sync_server_to_client_method_t * 
		      Netamqp_endpoint.data option)
  (** Works very much like {!Netamqp_endpoint.sync_c2s_e}, but the [channel_obj]
      can be passed, and the right timeout handling is done for this channel.
   *)

val number : channel_obj -> channel
  (** Return the channel number *)

val connection : channel_obj -> Netamqp_connection.connection
  (** Return the connection the channel uses *)

val endpoint : channel_obj ->  Netamqp_endpoint.endpoint
  (** Return the endpoint the channel uses *)

val event_system : channel_obj -> Unixqueue.event_system
  (** Return the event system the channel uses *)
