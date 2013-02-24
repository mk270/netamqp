(* $Id: netamqp_queue.mli 53389 2011-03-08 17:16:37Z gerd $ *)

(** Managing queues *)

open Netamqp_types
open Netamqp_rtypes

type queue_name = string
    (** Queues are identified by name *)

type 'a declare_result =
    out:( queue_name:queue_name ->
          message_count:Rtypes.uint4 ->
	  consumer_count:Rtypes.uint4 ->
            'a) ->
    unit ->
      'a
  (** The result of [declare] is returned by providing this function to the caller.
      When this function is called with an [out] argument, it will immediately
      call [out] back with the result values. The return value of [cb] is
      the return value of [declare_result].

      Arguments of [out]:
      - [queue_name]: the queue name
      - [message_count]: Number of messages in the queue 
      - [consumer_count]: Number of active consumers
   *)

val declare_passively_e : 
              channel:Netamqp_channel.channel_obj -> 
              queue:queue_name ->
              ?no_wait:bool ->
              unit ->
                'a declare_result Uq_engines.engine
val declare_passively_s : 
              channel:Netamqp_channel.channel_obj -> 
              queue:queue_name ->
              ?no_wait:bool ->
              unit ->
                'a declare_result
  (** Check whether a queue with the given [queue_name] exists, and raise
      a channel error if not.

      - [no_wait]: see below

      Example how to call this function:
      {[
        let r = declare_passively_s ~channel ~queue ()
        let qn = r ~out:(fun ~queue_name ~message_count ~consumer_count -> 
                           queue_name) ()
        (* qn is now the returned queue name *)
      ]}
   *)

val declare_e :
              channel:Netamqp_channel.channel_obj -> 
              queue:queue_name ->
              ?durable:bool ->
              ?exclusive:bool ->
              ?auto_delete:bool ->
              ?no_wait:bool ->
              ?arguments:table ->
              unit ->
                'a declare_result Uq_engines.engine
val declare_s :
              channel:Netamqp_channel.channel_obj -> 
              queue:queue_name ->
              ?durable:bool ->
              ?exclusive:bool ->
              ?auto_delete:bool ->
              ?no_wait:bool ->
              ?arguments:table ->
              unit ->
                'a declare_result
 (** Declare a new queue: Create it if the queue does not exist yet, or else
     check whether the queue exists with the given properties. A channel
     error is raised if these conditions are not met.

     Arguments:
     - [queue]: The queue name
     - [durable]:  whether to keep the queue across server restarts
     - [exclusive]: whether to consider the queue as private for this
       connection, and to delete it automatically when the connection is closed.
     - [auto_delete]: whether to delete the queue when all consumers have
       finished using it
     - [no_wait]: whether not to wait for the response of the declaration.
       This is faster, but errors are not immediately reported.
     - [arguments]: Arguments.

      Example how to call this function:
      {[
        let r = declare_s ~channel ~queue ()
        let qn = r ~out:(fun ~queue_name ~message_count ~consumer_count -> 
                           queue_name) ()
        (* qn is now the returned queue name *)
      ]}
  *)

val bind_e :
              channel:Netamqp_channel.channel_obj -> 
              queue:queue_name ->
              exchange:Netamqp_exchange.exchange_name ->
              routing_key:string ->
              ?no_wait:bool ->
              ?arguments:table ->
              unit ->
                unit Uq_engines.engine
val bind_s :
              channel:Netamqp_channel.channel_obj -> 
              queue:queue_name ->
              exchange:Netamqp_exchange.exchange_name ->
              routing_key:string ->
              ?no_wait:bool ->
              ?arguments:table ->
              unit ->
                unit
  (** Bind a queue to an exchange, and set the routing key.

      Arguments:
      - [queue]: The queue to bind
      - [exchange]: The exchange the queue is bound to
      - [routing_key]: The routing keys used by the exchange as criterion
        whether messages are appended to the queue.
      - [no_wait]: whether not to wait for the response of the request.
        This is faster, but errors are not immediately reported.
      - [arguments]: Arguments. At least the "headers" exchange type needs
        arguments.
  *)

val unbind_e :
              channel:Netamqp_channel.channel_obj -> 
              queue:queue_name ->
              exchange:Netamqp_exchange.exchange_name ->
              routing_key:string ->
              ?arguments:table ->
              unit ->
                unit Uq_engines.engine
val unbind_s :
              channel:Netamqp_channel.channel_obj -> 
              queue:queue_name ->
              exchange:Netamqp_exchange.exchange_name ->
              routing_key:string ->
              ?arguments:table ->
              unit ->
                unit
  (** Remove a binding. The arguments [queue], [exchange], [routing_key]
      and [arguments] identify the binding to delete.
   *)

val purge_e :
              channel:Netamqp_channel.channel_obj -> 
              queue:queue_name ->
              ?no_wait:bool ->
              unit ->
                Rtypes.uint4 Uq_engines.engine
val purge_s :
              channel:Netamqp_channel.channel_obj -> 
              queue:queue_name ->
              ?no_wait:bool ->
              unit ->
                Rtypes.uint4 
  (** This function removes all messages from a queue which are not awaiting
      acknowledgment.

      Arguments:
      - [queue]: the name of the queue
      - [no_wait]: whether not to wait for the response of the request.
        This is faster, but errors are not immediately reported.

      Result: The function returns the number of purged messages.
   *)

val delete_e :
              channel:Netamqp_channel.channel_obj -> 
              queue:queue_name ->
              ?if_unused:bool ->
              ?if_empty:bool ->
              ?no_wait:bool ->
              unit ->
                Rtypes.uint4 Uq_engines.engine
val delete_s :
              channel:Netamqp_channel.channel_obj -> 
              queue:queue_name ->
              ?if_unused:bool ->
              ?if_empty:bool ->
              ?no_wait:bool ->
              unit ->
                Rtypes.uint4 
  (** This function deletes the queue.

      Arguments:
      - [queue]: The queue to delete
      - [if_unused]: Only delete if there are no consumers
      - [if_empty]: Only delete if the queue is empty
      - [no_wait]: whether not to wait for the response of the request.
        This is faster, but errors are not immediately reported.

      Result: The function returns the number of deleted messages.
   *)
