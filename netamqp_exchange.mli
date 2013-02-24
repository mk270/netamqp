(* $Id: netamqp_exchange.mli 53368 2011-03-03 22:45:18Z gerd $ *)

(** Managing exchanges *)

open Netamqp_types
open Netamqp_rtypes

type exchange_name = string
    (** Exchange instances are identified by strings *)

val amq_direct : exchange_name
val amq_fanout : exchange_name
val amq_topic : exchange_name
val amq_match : exchange_name
  (** The four standard exchanges. It is not required that the server
      implements all of these - exception [amq_direct] which is mandatory.
   *)

type exchange_type = string
   (** Exchange types are identified by strings *)

val direct : exchange_type
val fanout : exchange_type
val topic : exchange_type
val headers : exchange_type
val system : exchange_type
  (** The five standard exchange types. *)


val declare_passively_e : 
              channel:Netamqp_channel.channel_obj -> 
              exchange_name:exchange_name ->
              ?no_wait:bool ->
              unit ->
                unit Uq_engines.engine
val declare_passively_s : 
              channel:Netamqp_channel.channel_obj -> 
              exchange_name:exchange_name ->
              ?no_wait:bool ->
              unit ->
                unit
  (** Check whether an exchange with the given [exchange_name] exists,
      and raise a channel error if not.

      - [no_wait]: see below
   *)
              
val declare_e : channel:Netamqp_channel.channel_obj -> 
                exchange_name:exchange_name ->
                exchange_type:exchange_type ->
                ?durable:bool ->
                ?no_wait:bool ->
                ?arguments:table ->
                unit ->
                  unit Uq_engines.engine
val declare_s : channel:Netamqp_channel.channel_obj -> 
                exchange_name:exchange_name ->
                exchange_type:exchange_type ->
                ?durable:bool ->
                ?no_wait:bool ->
                ?arguments:table ->
                unit ->
                  unit
  (** Declare a new exchange: Create it if the exchange does not exist yet,
      or else check whether the exchange exists with the given properties.
      A channel error is raised if these conditions are not met.

      Arguments:
      - [exchange_name]: The name
      - [exchange_type]: The type
      - [durable]: whether to keep the exchange instance across server restarts
      - [no_wait]: whether not to wait for the response of the declaration.
        This is faster, but errors are not immediately reported.
      - [arguments]: Arguments. The standard exchange types do not need
        arguments.
   *)

val delete_e : channel:Netamqp_channel.channel_obj -> 
               exchange_name:exchange_name ->
               ?if_unused:bool ->
               ?no_wait:bool ->
               unit ->
                  unit Uq_engines.engine
val delete_s : channel:Netamqp_channel.channel_obj -> 
               exchange_name:exchange_name ->
               ?if_unused:bool ->
               ?no_wait:bool ->
               unit ->
                  unit
  (** Delete an exchange.

      Options:
      - [exchange_name]: the name of the instance
      - [if_unused]: only delete if there are no queue bindings
      - [no_wait]: whether not to wait for the response of the declaration.
        This is faster, but errors are not immediately reported.
   *)

