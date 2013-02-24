(* $Id: netamqp_basic.mli 53389 2011-03-08 17:16:37Z gerd $ *)

(** Sending and receiving messages via a queue *)

open Netamqp_types
open Netamqp_rtypes

class type message =
object
  method content_type : string option
    (** MIME typing *)

  method content_encoding : string option
    (** MIME typing *)

  method headers : table option
    (** For applications, and for header exchange routing *)

  method delivery_mode : int option
    (** 1=non-persistent, 2=persistent *)

  method priority : int option
    (** message priority, 0 to 9 *)

  method correlation_id : string option
    (** For application use, no formal behaviour  *)

  method reply_to : string option
    (** For application use, no formal behaviour but may hold the
        name of a private response queue, when used in request messages *)

  method expiration : string option
    (** For implementation use, no formal behaviour *)

  method message_id : string option
    (** For application use, no formal behaviour  *)

  method timestamp : float option
    (** For application use, no formal behaviour  *)

  method typ : string option
    (** For application use, no formal behaviour  *)

  method user_id : string option
    (** For application use, no formal behaviour  *)

  method app_id : string option
    (** For application use, no formal behaviour  *)

  method amqp_header : Netamqp_endpoint.props_t
    (** The complete message header (all of the above fields in once) *)

  method amqp_body : Xdr_mstring.mstring list
    (** The message body *)

end
  
val create_message :
      ?content_type : string ->
      ?content_encoding : string ->
      ?headers : table ->
      ?delivery_mode : int ->
      ?priority : int ->
      ?correlation_id : string ->
      ?reply_to : string ->
      ?expiration : string ->
      ?message_id : string ->
      ?timestamp : float ->
      ?typ : string ->
      ?user_id : string ->
      ?app_id : string ->
      Xdr_mstring.mstring list ->
	message
  (** Creates a new message. See the class type {!Netamqp_basic.message}
      for documentation of the optional header fields.

      The unnamed argument is the body.
   *)

val qos_e :
              channel:Netamqp_channel.channel_obj -> 
              ?prefetch_size:int ->
              ?prefetch_count:int ->
              ?global:bool ->
              unit ->
                unit Uq_engines.engine
val qos_s :
              channel:Netamqp_channel.channel_obj -> 
              ?prefetch_size:int ->
              ?prefetch_count:int ->
              ?global:bool ->
              unit ->
                unit
  (** Sets the quality of service for the channel (or the whole connection
      if [global=true]).

      - [prefetch_size]: Sets the prefetch window in octets
      - [prefetch_count]: Sets the prefetch window in number of messages
   *)

val consume_e :
              channel:Netamqp_channel.channel_obj -> 
              queue:Netamqp_queue.queue_name ->
              ?consumer_tag:string ->
              ?no_local:bool ->
              ?no_ack:bool ->
              ?exclusive:bool ->
              ?no_wait:bool ->
              ?arguments:table ->
              unit ->
                string Uq_engines.engine
val consume_s :
              channel:Netamqp_channel.channel_obj -> 
              queue:Netamqp_queue.queue_name ->
              ?consumer_tag:string ->
              ?no_local:bool ->
              ?no_ack:bool ->
              ?exclusive:bool ->
              ?no_wait:bool ->
              ?arguments:table ->
              unit ->
                string
  (** This method asks the server to start a "consumer", which is a
      transient request for messages from a specific
      queue. Consumers last as long as the channel they were
      declared on, or until the client cancels them.

      Arguments:
      - [queue]: The queue to consume from
      - [consumer_tag]: Identifies the consumer. If empty or omitted,
        the server creates a unique identifier
      - [no_local]: Do not receive messages that were published over
        this connection
      - [no_ack]: Do not expect acknowledgements for consumed messages
      - [exclusive]: Request exclusive access to the queue
      - [no_wait]: whether not to wait for the response of the request.
        This is faster, but errors are not immediately reported,
        and automatically created consumer tags cannot be returned to
        the client.
      - [arguments]: Depends on the server implementation

      Return value: The actual consumer tag

*)

val cancel_e :
              channel:Netamqp_channel.channel_obj -> 
              consumer_tag:string ->
              ?no_wait:bool ->
              unit ->
                string Uq_engines.engine
val cancel_s :
              channel:Netamqp_channel.channel_obj -> 
              consumer_tag:string ->
              ?no_wait:bool ->
              unit ->
                string
 (** Cancels the consumer identified by [consumer_tag]. This does not
     affect already delivered messages, but it does mean the server
     will not send any more messages for that consumer. The client
     may receive an arbitrary number of messages in between sending
     the cancel method and receiving the cancel-ok reply. 

     Arguments:
     - [consumer_tag]: The consumer to cancel
     - [no_wait]: whether not to wait for the response of the request.
       This is faster, but errors are not immediately reported.

     Return value: The tag of the actually cancelled consumer
 *)

val publish_e :
              channel:Netamqp_channel.channel_obj -> 
              exchange:Netamqp_exchange.exchange_name ->
              routing_key:string ->
              ?mandatory:bool ->
              ?immediate:bool ->
              message ->
                unit Uq_engines.engine
val publish_s :
              channel:Netamqp_channel.channel_obj -> 
              exchange:Netamqp_exchange.exchange_name ->
              routing_key:string ->
              ?mandatory:bool ->
              ?immediate:bool ->
              message ->
                unit
  (** Publishes the passed message. Note that this is an async operation,
      and when this function is finished, this only means that the message
      has been written to the socket, but not more.

      Arguments:
      - [exchange]: Name of exchange to send the message to
      - [routing_key]: The routing key for the exchange
      - [mandatory]: This flag tells the server how to react if the message
        cannot be routed to a queue. If this flag is set, the server will
        return an unroutable message with a Return method. If this flag is
        false, the server silently drops the message.
      - [immediate]: This flag tells the server how to react if the message cannot be
        routed to a queue consumer immediately. If this flag is set, the
        server will return an undeliverable message with a Return method. 
        If this flag is false, the server will queue the message, but with 
        no guarantee that it will ever be consumed.
   *)

val on_return :
              channel:Netamqp_channel.channel_obj -> 
              cb:(reply_code:int -> 
                  reply_text:string -> 
                  exchange:Netamqp_exchange.exchange_name ->
                  routing_key:string ->
                  message ->
                    unit
		 ) ->
              unit ->
                unit
  (** Registers a handler so that [cb] is called back whenever a
      message is returned to the client. The handler remains active
      as long as the channel is open.

      Arguments of the callback:
      - [reply_code]: Reason for return, as error code
      - [reply_text]: Reason for return, as error text
      - [exchange]: the exchange the message was originally published
         to.  May be empty, meaning the default exchange.
      - [routing_key]: the routing key name specified when the message was
         published
   *)

val on_deliver :
              channel:Netamqp_channel.channel_obj -> 
              cb:(consumer_tag:string ->
                  delivery_tag:Rtypes.uint8 ->
                  redelivered:bool ->
                  exchange:Netamqp_exchange.exchange_name ->
                  routing_key:string ->
                  message ->
                    unit
	      ) ->
              unit ->
                unit
  (** Registers a handler so that [cb] is called back whenever a
      message is delivered to the client. The handler remains active
      as long as the channel is open.

      Arguments of the callback:
      - [consumer_tag]: The name of the consumer to which this delivery
        refers to
      - [delivery_tag]: A unique name of this delivery. Needed for
        acknowledging the message
      - [redelivered]: indicates that the message has been previously
        delivered to this or another client
      - [exchange]: the exchange the message was originally published
        to. May be empty, meaning the default exchange.
      - [routing_key]: the routing key name specified when the message was
        published
   *)

type 'a get_message =
  out:( delivery_tag : Rtypes.uint8 ->
        redelivered : bool ->
        exchange : Netamqp_exchange.exchange_name ->
        routing_key:string ->
        message_count:Rtypes.uint4 ->
        message ->
         'a) ->
  unit ->
    'a
  (** The result of [get] is returned by providing this function to the caller.
      When this function is called with an [out] argument, it will immediately
      call [out] back with the result values. The return value of [cb] is
      the return value of [get_message].

      Arguments of [out]:
      - [delivery_tag]: A unique name of this delivery. Needed for
        acknowledging the message
      - [redelivered]: indicates that the message has been previously
        delivered to this or another client
      - [exchange]: the exchange the message was originally published
        to. May be empty, meaning the default exchange.
      - [routing_key]: the routing key name specified when the message was
        published
      - [message_count]: ?
   *)

type 'a get_result =
  [ `Message of 'a get_message | `Empty ]
  (** Responses of [get] can return a message, or indicate that the queue
      is empty
   *)

val get_e :
              channel:Netamqp_channel.channel_obj -> 
              queue:Netamqp_queue.queue_name ->
              ?no_ack:bool ->
              unit ->
                'a get_result Uq_engines.engine
val get_s :
              channel:Netamqp_channel.channel_obj -> 
              queue:Netamqp_queue.queue_name ->
              ?no_ack:bool ->
              unit ->
                'a get_result
  (** Fetches a message synchronously from the queue.

      Arguments:
      - [queue]: the name of the queue
      - [no_ack]: Do not expect acknowledgements for consumed messages

      The result is made available via a function [get_result]. For
      example, to just get the message:

      {[
        let get_result = get_s ... () in
        match get_result with
          | `Empty -> failwith "No message on the queue"
          | `Message get_msg ->
              let message =
		  get_msg 
		    ~out:(fun ~delivery_tag ~redelivered ~exchange ~routing_key
			      ~message_count message ->
			    message
			 )
		    () in
                 ...
      ]}
   *)

val ack_e :
              channel:Netamqp_channel.channel_obj -> 
              delivery_tag:Rtypes.uint8 ->
              ?multiple:bool ->
              unit ->
                unit Uq_engines.engine
val ack_s :
              channel:Netamqp_channel.channel_obj -> 
              delivery_tag:Rtypes.uint8 ->
              ?multiple:bool ->
              unit ->
                unit 
  (** Acknowledges the delivery of the message identified by [delivery_tag].
      Note that this is an async operation,
      and when this function is finished, this only means that the request
      has been written to the socket, but not more.

      Arguments:
      - [multiple]: If set, the delivery tag is treated as "up to and
        including", so that the client can acknowledge multiple messages 
        with a single method.
   *)

val reject_e :
              channel:Netamqp_channel.channel_obj -> 
              delivery_tag:Rtypes.uint8 ->
              requeue:bool ->
              unit ->
                unit Uq_engines.engine
val reject_s :
              channel:Netamqp_channel.channel_obj -> 
              delivery_tag:Rtypes.uint8 ->
              requeue:bool ->
              unit ->
                unit
  (** Rejects a message (instead of acknowledging it).
      Note that this is an async operation,
      and when this function is finished, this only means that the request
      has been written to the socket, but not more.

      Arguments:
      - [delivery_tag]: identifies the message to reject
      - [requeue]: Requeue the message. The message is never sent again to
        this channel.
   *)

val recover_e :
              channel:Netamqp_channel.channel_obj -> 
              requeue:bool ->
              unit ->
                unit Uq_engines.engine
val recover_s :
              channel:Netamqp_channel.channel_obj -> 
              requeue:bool ->
              unit ->
                unit
  (** Redeliver all unacknowledged messages on a
      specified channel. Zero or more messages may be redelivered.

      Arguments:
      - [requeue]: If this field is false, the message will be redelivered
        to the original recipient. If this bit is set, the server will attempt
        to requeue the message,
        potentially then delivering it to an alternative subscriber.
   *)

(** recover-async is deprecated and omitted here *)

