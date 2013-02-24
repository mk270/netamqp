(* $Id: netamqp_transport.mli 53300 2011-02-22 00:09:06Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Low-level AMQP transporters *)

open Netamqp_types

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

val string_of_sockaddr : sockaddr -> string
  (** Convert to string, for debugging purposes *)


exception Error of string
  (** Passed back as [`Error]. Such errors are fatal. *)

class type amqp_multiplex_controller =
object
  method alive : bool
    (** If the controller is alive, the socket is not yet completely down. *)

  method event_system : Unixqueue.event_system
    (** Returns the event system *)

  method getsockname : sockaddr
    (** The address of this socket *)

  method getpeername : sockaddr
    (** The address of the peer's socket. Only available if the socket
      * is connected. (Fails otherwise.)
     *)

  method transport_type : transport_type
    (** The transport type *)

  method set_max_frame_size : int -> unit
    (** The maximum frame size. By default, this is 2^32-1 on 64 bit
	platforms, and [Sys.max_string_length] on 32 bit platforms.
	The defaults are also the maximum possible values - larger
	configurations are automatically clipped to the maximum
     *)

  method eff_max_frame_size : int
    (** The effective maximum frame size *)

  method reading : bool
    (** True iff there is a reader *)

  method read_eof : bool
    (** Whether the EOF marker has been read *)

  method start_reading : 
    when_done:( frame result_eof -> unit) -> unit -> unit
    (** Start reading from the connection. When a whole message has been
      * received, the [when_done] callback is invoked with the
      * received [frame] as argument.
      *
      * This starts one-time read job only, i.e. it is not restarted
      * after [when_done] has been invoked.
      *
      * It is an error to start reading several times at once.
     *)

  method writing : bool
   (** True iff there is a writer *)

  method start_writing :
    when_done:(unit result -> unit) -> frame -> unit
    (** Starts writing the frame. Invoked [when_done] when it is written,
      * or an error condition is reached.
      *
      * This starts one-time write job only, i.e. it is not restarted
      * after [when_done] has been invoked.
      *
      * It is an error to start writing several times at once.
     *)

  method cancel_rd_polling : unit -> unit
    (** Cancels polling for the next input message. This method must not be
      * called from the [before_record] callback function. Polling can be
      * resumed by calling [start_reading] again.
     *)

  method abort_rw : unit -> unit
    (** Aborts the current reader and/or writer forever. Note that there is no
      * clean way of resuming reading and/or writing. The readers/writers
      * are not notified about cancellation.
     *)

  method start_shutting_down :
    when_done:(unit result -> unit) -> unit -> unit
    (** Start shutting down the connection. After going through the shutdown
      * procedure, the [when_done] callback is invoked reporting the success
      * or failure.
      *
      * The underlying file descriptor (if any) is not closed. A shutdown
      * is only a protocol handshake. After a shutdown,[read_eof]
      * is true. Call [inactivate] to close the descriptor.
     *)
 
  method cancel_shutting_down : unit -> unit
    (** Cancels the shutdown procedure. After that, the state of the 
      * connection is undefined. The [when_done] callback is invoked with
      * the [`Cancelled].
      *
      * It is no error if no shutdown is in progress.
     *)

  method set_timeout : notify:(unit -> unit) -> float -> unit
    (** If a requested read or write cannot be done for the passed number of
      * seconds, the [notify] callback is invoked.
     *)

  method inactivate : unit -> unit
    (** Inactivates the connection immediately, and releases any resources
      * the controller is responsible for (e.g. closes file descriptors). 
      * Note that this is more than
      * cancelling all pending operations and shutting the connection down.
      * However, the details of this method are implementation-defined.
      * Callbacks are not invoked.
     *)

end


val tcp_amqp_multiplex_controller :
       ?close_inactive_descr:bool ->
       ?preclose:(unit -> unit) ->
       Unix.file_descr -> Unixqueue.event_system ->
         amqp_multiplex_controller
  (** The multiplex controller for stream encapsulation

      - [close_inactive_descr]: If true, the descriptor is closed when
        inactivated
      - [preclose]: This function is called just before the descriptor
        is closed.
   *)

(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging *)

end
