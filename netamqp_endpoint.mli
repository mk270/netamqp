(* $Id: netamqp_endpoint.mli 53444 2011-03-10 14:08:13Z gerd $ *)

(** Endpoints are low-level containers for the protocol state

    They implement the send/receive queues, and the message dispatcher
 *)

open Netamqp_types

type connector =
    [ `Sockaddr of Unix.sockaddr
    | `Inet of string * int           (* hostname, port *)
    | `Implied
    ]
  (** How to connect to the server:
      - [`Sockaddr a]: the server port is given by socket address [a]
      - [`Inet(host,port)]: the server is identified by this host/port pair
      - [`Implied]: a reserved connector when the server address is implicitly
        known only
   *)

type transport_layer =
    [ `TCP of connector
    (* `SSL of ... *)
    | `Custom of 
	(unit -> Netamqp_transport.amqp_multiplex_controller Uq_engines.engine)
    ]
  (** Which transport layer to choose:
      - [`TCP connector]: Use TCP
      - [`Custom f]: Call [f ()] to get a new transport stream
   *)

type endpoint
  (** The abstract endpoint container *)

type state =
    [ `Off
    | `Connected of bool
    | `Disconnecting of bool
    | `Disconnected
    | `Error of exn
    ]
  (** States:
      - [`Off]: Endpoint is still down (initial state)
      - [`Connected flag]: The endpoint accepts requests. If [flag],
        the TCP connection is established. Note that the endpoint
        already allows to add requests before the TCP connection is
        fully established.
      - [`Disconnecting flag]: A disconnect is requested when all async
        methods are sent and all sync methods are responded, i.e. when
        all locally requested operations are done. If [flag], the
        TCP connection is established. The case [flag=false] can only
        happen if a not-yet fully connected endpoint is disconnected.
      - [`Disconnected]: Endpoint is down after regular disconnect
      - [`Error]: A low-level error occurred, and the endpoint is down
   *)

type proto_revision_0_9 =
    [ `One ]

type protocol =
    [ `AMQP_0_9 of proto_revision_0_9 ]
  (** The protocol variant. Patch releases are not counted here:
      - [`AMQP_0_9]: This actually chooses AMQP-0-9-1 with as much
        compatibility as possible for existing servers. 0-9-1 is
        the subset of 0-9 that is widely implemented.
   *)


(** Methods are control messages exchanged between client and server.
    They exist in two major styles:

    - {b Synchronous} methods are part of request/response pairs
    - {b Asynchronous} methods are one-way messages

    There is a bit confusion in terminology because the sync/async
    distinction is also used for the way socket events are handled.
    This is not meant in this context!

    Both styles of message exchange can be started by either peer,
    client or server.

    In a request/response pair, {b each} of the two control messages
    is called a method.

    A method usually has arguments. The method type is the method
    without arguments.

    The methods are described in more detail in an XML file (e.g.
    amqp0-9-1.xml). There is also a version-specific Ocaml module that
    is generated: {!Netamqp_method_0_9}. See there how the methods
    are formalized (e.g. which types the arguments have).

 *)

type sync_client_to_server_method_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.sync_client_to_server_method_t ]
  (** The subset of methods:
      - Synchronous methods the client can send to the server. This
        includes both initial requests from the client and responses
        to the server when the server initiates the request/response pair.
   *)

type sync_server_to_client_method_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.sync_server_to_client_method_t ]
  (** The subset of methods:
      - Synchronous methods the server can send to the client. This
        includes both initial requests from the server and responses
        to the client when the client initiates the request/response pair.
   *)

type sync_client_initiated_method_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.sync_client_initiated_method_t ]
  (** The subset of methods:
      - Synchronous methods where the client sends the first message
   *)
     
type sync_server_initiated_method_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.sync_server_initiated_method_t ]
  (** The subset of methods:
      - Synchronous methods where the server sends the first message
   *)

type sync_server_initiated_method_type_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.sync_server_initiated_method_type_t ]
  (** The subset of method types:
      - Synchronous method types where the server sends the first message
   *)

type async_client_to_server_method_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.async_client_to_server_method_t ]
  (** The subset of methods:
      - Asynchronous methods the client can send to the server
   *)

type async_server_to_client_method_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.async_server_to_client_method_t ]
  (** The subset of methods:
      - Asynchronous methods the server can send to the server
   *)

type async_server_to_client_method_type_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.async_server_to_client_method_type_t ]
  (** The subset of method types:
      - Asynchronous method types the server can send to the server
   *)

type method_type_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.method_type_t ]
  (** All method types *)

(** Some methods can also carry content data, i.e. the real data that
    goes to or comes from the message queue. Content data is primarily
    a long string, but there is also a header with properties.
 *)

type props_t =
    [ `AMQP_0_9 of Netamqp_methods_0_9.props_t ]
  (** Possible properties. Note that the property class must match the
      class that is used for content exchange. Right now this means
      only [`P_basic] is actually available.
 *)

type data =
    props_t * Xdr_mstring.mstring list
  (** Content data as pair of properties and a string. The string is
      represented as list of [mstring], an abstraction over several
      possible representations of byte arrays provided by Ocamlnet
      (see the [rpc] library for [Xdr_mstring]).

      Data received from the server is often returned as a true list
      with more than one element. Each element represents a frame on
      the transport level.

      There is no need to split strings into frames before passing
      the strings to the endpoint for sending them to the server. This
      is done automatically if needed.
 *)


(** {2 Creating and (dis)conecting} *)

val create : transport_layer -> protocol -> Unixqueue.event_system -> endpoint
  (** Creates a new endpoint which is initially [`Off]. *)

val configure_timeout : endpoint -> float -> unit
  (** Configures the transport-level timeout. This only affects:
      - Connecting to the server
      - Sending messages
      - Receiving messages while a frame is being read (but not between frames)

      Defaults to 300 seconds if not configured.
   *)

val get_timeout : endpoint -> float
  (** Return the configured timeout value *)

val default_port : int
  (** Default port number for AMQP *)

val connect : endpoint -> unit
  (** Triggers the connect to the server. The new state is [`Connected false].
      If the endpoint is already connected, nothing changes.

      After calling [connect] the underlying transport is still inactive,
      but the connection procedure is triggered. It is nevertheless immediately
      possible to add requests to the endpoint. These will be carried out
      once the connection on the transport level is established.
   *)

val disconnect : endpoint -> unit
  (** Requests an orderly disconnect. The disconnect is only triggered
      but first done when
      - All async methods are sent to the server
      - All locally invoked sync methods are responded
      - The event loop runs the next time.

      The new state is [`Disconnecting].
   *)

val quick_disconnect : endpoint -> unit
  (** A quick disconnect proceeds faster than an orderly disconnect.
      In particular, it is not waited until the locally invoked
      sync methods are responded.
   *)

val reset : endpoint -> unit
  (** Turns the endpoint off again. Operations, if any, are immediately
      aborted.
   *)

val state : endpoint -> state
  (** Reports the connection state *)

val state_change_e : endpoint -> state Uq_engines.engine
  (** The engine finishes at the next [state] change. 

      It is possible to immediately create another [state_change_e] when
      the previous engine finishes. This allows it to continuously watch 
      for state changes.
   *)

val event_system : endpoint -> Unixqueue.event_system
  (** Return the event system the endpoint uses *)

val protocol : endpoint -> protocol
  (** Return the protocol *)



(** {2 Using an activated endpoint} *)

(** The following methods must only be called when the state is
    [`Connected], i.e. after calling [connect].
 *)

val getsockname : endpoint -> Netamqp_transport.sockaddr
val getpeername : endpoint -> Netamqp_transport.sockaddr
  (** Get the names of the own socket and the peer socket, resp. 
      These functions fail if the endpoint socket is not connected
      at the moment of checking.
   *)

val enable_channel : endpoint -> channel -> unit
val disable_channel : endpoint -> channel -> unit
  (** Enable/disable channels. Only an enabled channel can be used for
      communication.

      Channel 0 is always enabled.

      Disabling a channel immediately drops all unsent messages except
      those on the priority queue. Also, 
      pending synchronous calls will get the exception
      {!Netamqp_types.Method_dropped}. All registrations
      for the channel are deleted.
   *)

val is_channel_enabled : endpoint -> channel -> bool
  (** Whether a channel is enabled *)

val suggest_channel : endpoint -> channel
  (** Suggests a channel number *)

val flow_control : endpoint -> channel -> bool -> unit
  (** Requests that the flow is enabled (true) or disabled (false) for
      a certain channel. By default, the channel flow is enabled.

      This only affects content messages sent to the server.
   *)

val drop_frames : endpoint -> unit
  (** Request to drop any incoming frames (to be used after having
      received the connection.close method)
   *)

val clear_output : endpoint -> unit
  (** Remove any frames from output queues *)

val expect_eof : endpoint -> unit
  (** Instruct the endpoint not to generate an {!Netamqp_types.Unexpected_eof}
      exception when EOF is seen from the server
   *)

val set_max_frame_size : endpoint -> int -> unit
val eff_max_frame_size : endpoint -> int
  (** These two function talk to the transport, see
      {!Netampq_transport.amqp_multiplex_controller}
   *)



(** {2 Sending and receiving messages} *)


val announce_e : endpoint -> unit Uq_engines.engine
val announce_s : endpoint -> unit
  (** Sends the protocol header, and waits until the response arrives.
      If the response is the right server method, the engine finishes
      normally. Otherwise it enters the error state.
   *)

val sync_c2s_e : ?no_wait:sync_server_to_client_method_t ->
                 ?on_timeout:(unit -> unit) ->
                 endpoint ->
                 sync_client_initiated_method_t ->
                 data option ->
                 channel ->
                 float ->
                   (sync_server_to_client_method_t * data option)
                      Uq_engines.engine
val sync_c2s_s : ?no_wait:sync_server_to_client_method_t ->
                 ?on_timeout:(unit -> unit) ->
                 endpoint ->
                 sync_client_initiated_method_t ->
                 data option ->
                 channel ->
                 float ->
                   (sync_server_to_client_method_t * data option)
  (** Synchronous calls initiated by the client: Calls the argument method
      and waits for the right reply method.

      Only certain methods may be accompanied with a data item
      ([`Basic_return]). Only certain methods
      can have a data item in the response ([`Basic_get_ok]).

      Actually, the possible return methods are much more restricted
      than [sync_server_to_client_method_t], e.g. [`Channel_open]
      can only be responded with [`Channel_open_ok]. This is not
      reflected in the function type, though.

      The [float] arg is the timeout. If the message is not responded
      within that time frame, the exception [Timeout] is raised.

      Option [no_wait]: If set, the function does not wait for the reply,
      but immediately returns the method [no_wait] (and no data).

      Option [on_timeout]: This function is called first when a timeout
      occurs. (Additionally, all pending sync_c2s calls on the same
      channel get a [Timeout] exception.)
   *)

val register_sync_s2c : endpoint ->
                        sync_server_initiated_method_type_t ->
                        channel ->
                        (sync_server_initiated_method_t ->
		         sync_client_to_server_method_t option) ->
                        (unit -> unit) ->   (* post action *)
                          unit
  (** Synchronous calls initiated by the server:
      Registers that the callback function is called 
      when the server sends a method of the given type.

      Normally, the callback returns [Some r] where [r] is the
      response method. The callback is also allowed to return [None] 
      in case of an error.
      Some additional reaction should be provided, though, e.g. by requesting a
      connection close.

      The post action function is only invoked if the callback returns
      a result. The idea is that another action can be triggered after
      the response has been added to the output queue.

      The response is not added to the normal output queue, but to
      the priority output queue, and has precedence to all other
      methods emitted by the client.

      The registered handler is not notified if there is a state
      change of the endpoint or if an error is propagated. If this
      is required, the handler should configure additional monitoring
      for these events.
   *)

val async_c2s : endpoint ->
                async_client_to_server_method_t ->
                data option ->
                channel ->
                  unit
  (** Asynchronous calls from the client: This function just 
      sends the given method to the server. Note that the actual
      transmission is controlled by the event loop and does not
      happen immediately.

      Only certain methods may be accompanied with a data item
      ([`Basic_publish]).

      There is no indication of any kind whether the operation was
      successful, not even whether it could be sent to the
      server. If feedback is required one must use transactions
      ([Tx] class).
   *)

val async_c2s_e : endpoint ->
                  async_client_to_server_method_t ->
                  data option ->
                  channel ->
                    unit Uq_engines.engine
val async_c2s_s : endpoint ->
                  async_client_to_server_method_t ->
                  data option ->
                  channel ->
                    unit
  (** These versions of [async_c2s] return first when the message is
      passed to the socket.
   *)

val register_async_s2c : endpoint ->
                         async_server_to_client_method_type_t -> 
                         channel ->
                     (async_server_to_client_method_t -> data option -> unit) ->
                           unit
  (** Asynchronous calls from the server:
      Registers that the callback function is called
      when the server sends a method of the given type.

      The registered handler is not notified if there is a state
      change of the endpoint or if an error is propagated. If this
      is required, the handler should configure additional monitoring
      for these events.
   *)


(** {2 Error propagation} *)

(** Errors from the socket level lead immediately to
    a shutdown of all activities, and the TCP connection breaks.
    The state transitions to [`Error e], where the exception [e]
    describes the error. Some other kinds of locally detected errors are also
    handled like this. After shutting the endpoint down, the error is
    propagated (see below).

    Exception codes coming from the server are wrapped as [`Connection_close]
    or [`Channel_close] methods. These are forwarded to registered
    handlers ([register_sync_s2c]). The handlers can decide to propagate
    the error code further by calling [propagate_error] with a
    [Method_exception].

    Error propagation is done for all errors detected by the endpoint,
    but it can also be invoked from outside (function [propagate_error],
    see below).

    The following kinds of errors can be generated by the endpoint:

      - Socket errors (as [Unix.Unix_error]) (1) (2)
      - Decoder errors (as {!Netamqp_types.Decode}) (1) (2)
      - Encoder errors (as {!Netamqp_types.Encode}) (1) (2)
      - {!Netamqp_types.Protocol_is_not_supported} (1) (2)
      - {!Netamqp_types.Timeout}: when a synchronous call times out
      - {!Netamqp_types.Method_cannot_be_dispatched}: when the endpoint
        finds a frame
        that can be decoded but not be dispatched (no handler) (2)
      - {!Netamqp_types.Unexpected_eof} (1) (2)
      - {!Netamqp_types.Unexpected_frame} (1) (2)

    (1) = The endpoint is automatically shut down as described

    (2) = This error is not associated to a particular channel


    The following codes should be used for injecting errors from outside:
    - {!Netamqp_types.Method_exception}: for all exceptions coming from the 
      server
    - {!Netamqp_types.Protocol_violation}: for all hard violations of the 
      protocol, especially those that lead to a shutdown
    - {!Netamqp_types.Method_dropped}: when a synchronous call cannot
      be continued because
      the client is shut down, and no better error code exists
 *)

val propagate_error : endpoint -> exn -> channel option -> unit
  (** Propagate the exception to handlers and engines.

      If the channel is [None], all possible handlers and engines receive
      the exception. Otherwise, only the handlers and engines get it that
      are connected with the given channel.

      The engines and handlers receiving the exceptions:
      - [announce_e]
      - [sync_c2s_e]
      - The defined listeners ([listen_for_errors]).

      Note that there is no way to notify registered handlers that
      just wait for incoming server methods ([register_sync_s2c] and
      [register_async_s2c]). If these handlers need to be notified about
      errors they should register an error listener.

      The endpoint state is not modified.
   *)

val abort_and_propagate_error : endpoint -> exn -> unit
  (** The endpoint is aborted and set to the [`Error] state. Also,
      the passed exception is propagated unconditionally.
   *)

val listen_for_errors : endpoint -> channel option -> (exn -> bool) -> unit
  (** Defines an error listener.

      The callback function must return [true] to remain active; otherwise
      the listener is disabled.
   *)

val create_method_exception : 
       protocol ->
       class_id:int -> meth_id:int -> reply_code:int -> reply_text:string ->
         exn
  (** Returns a [Method_exception] *)

module Debug : sig
  val enable : bool ref
end


(**/**)

val sync : ('a -> 'b Uq_engines.engine) -> 'a -> 'b
