(* $Id: netamqp_tx.mli 53389 2011-03-08 17:16:37Z gerd $ *)

(** Transactions *)

(** Transactions cover only the [publish] and [ack] methods *)

open Netamqp_types
open Netamqp_rtypes

val select_e :
              channel:Netamqp_channel.channel_obj -> 
              unit ->
                unit Uq_engines.engine
val select_s :
              channel:Netamqp_channel.channel_obj -> 
              unit ->
                unit
  (** Selects transactions for this channel *)

val commit_e :
              channel:Netamqp_channel.channel_obj -> 
              unit ->
                unit Uq_engines.engine
val commit_s :
              channel:Netamqp_channel.channel_obj -> 
              unit ->
                unit
  (** Commits the current transaction and starts a new one *)

val rollback_e :
              channel:Netamqp_channel.channel_obj -> 
              unit ->
                unit Uq_engines.engine
val rollback_s :
              channel:Netamqp_channel.channel_obj -> 
              unit ->
                unit 
  (** Rolls the current transaction back and starts a new one *)
