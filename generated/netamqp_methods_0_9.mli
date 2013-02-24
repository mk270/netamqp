(* This file is generated! Do not edit! *)

type no_ack = bool

type octet = int  (* 0..255 *)

type reply_code = int  (* 0..65535 *)
  (* where: not null *)

type redelivered = bool

type peer_properties = Netamqp_rtypes.table

type queue_name = string  (* up to 255 chars *)
  (* where: length <= 127 *)
  (* where: matches regexp ^[a-zA-Z0-9-_.:]*$ *)

type table = Netamqp_rtypes.table

type timestamp = float

type longstr = string  (* up to 4G chars *)

type long = Rtypes.uint4

type short = int  (* 0..65535 *)

type shortstr = string  (* up to 255 chars *)

type bit = bool

type consumer_tag = string  (* up to 255 chars *)

type method_id = int  (* 0..65535 *)

type path = string  (* up to 255 chars *)
  (* where: not null *)
  (* where: length <= 127 *)

type no_local = bool

type reply_text = string  (* up to 255 chars *)
  (* where: not null *)

type exchange_name = string  (* up to 255 chars *)
  (* where: length <= 127 *)
  (* where: matches regexp ^[a-zA-Z0-9-_.:]*$ *)

type longlong = Rtypes.uint8

type class_id = int  (* 0..65535 *)

type no_wait = bool

type message_count = Rtypes.uint4

type delivery_tag = Rtypes.uint8

type arg_Connection_start =
     ( octet           (* version-major *)
     * octet           (* version-minor *)
     * peer_properties (* server-properties *)
     * longstr         (* mechanisms, where not null *)
     * longstr         (* locales, where not null *)
     )

type arg_Connection_start_ok =
     ( peer_properties (* client-properties *)
     * shortstr        (* mechanism, where not null *)
     * longstr         (* response, where not null *)
     * shortstr        (* locale, where not null *)
     )

type arg_Connection_secure =
     ( longstr         (* challenge *)
     )

type arg_Connection_secure_ok =
     ( longstr         (* response, where not null *)
     )

type arg_Connection_tune =
     ( short           (* channel-max *)
     * long            (* frame-max *)
     * short           (* heartbeat *)
     )

type arg_Connection_tune_ok =
     ( short           (* channel-max, where not null *)
     * long            (* frame-max *)
     * short           (* heartbeat *)
     )

type arg_Connection_open =
     ( path            (* virtual-host *)
     * shortstr        (* reserved-1 *)
     * bit             (* reserved-2 *)
     )

type arg_Connection_open_ok =
     ( shortstr        (* reserved-1 *)
     )

type arg_Connection_close =
     ( reply_code      (* reply-code *)
     * reply_text      (* reply-text *)
     * class_id        (* class-id *)
     * method_id       (* method-id *)
     )

type arg_Channel_open =
     ( shortstr        (* reserved-1 *)
     )

type arg_Channel_open_ok =
     ( longstr         (* reserved-1 *)
     )

type arg_Channel_flow =
     ( bit             (* active *)
     )

type arg_Channel_flow_ok =
     ( bit             (* active *)
     )

type arg_Channel_close =
     ( reply_code      (* reply-code *)
     * reply_text      (* reply-text *)
     * class_id        (* class-id *)
     * method_id       (* method-id *)
     )

type arg_Exchange_declare =
     ( short           (* reserved-1 *)
     * exchange_name   (* exchange, where not null *)
     * shortstr        (* type *)
     * bit             (* passive *)
     * bit             (* durable *)
     * bit             (* reserved-2 *)
     * bit             (* reserved-3 *)
     * no_wait         (* no-wait *)
     * table           (* arguments *)
     )

type arg_Exchange_delete =
     ( short           (* reserved-1 *)
     * exchange_name   (* exchange, where not null *)
     * bit             (* if-unused *)
     * no_wait         (* no-wait *)
     )

type arg_Queue_declare =
     ( short           (* reserved-1 *)
     * queue_name      (* queue *)
     * bit             (* passive *)
     * bit             (* durable *)
     * bit             (* exclusive *)
     * bit             (* auto-delete *)
     * no_wait         (* no-wait *)
     * table           (* arguments *)
     )

type arg_Queue_declare_ok =
     ( queue_name      (* queue, where not null *)
     * message_count   (* message-count *)
     * long            (* consumer-count *)
     )

type arg_Queue_bind =
     ( short           (* reserved-1 *)
     * queue_name      (* queue *)
     * exchange_name   (* exchange *)
     * shortstr        (* routing-key *)
     * no_wait         (* no-wait *)
     * table           (* arguments *)
     )

type arg_Queue_unbind =
     ( short           (* reserved-1 *)
     * queue_name      (* queue *)
     * exchange_name   (* exchange *)
     * shortstr        (* routing-key *)
     * table           (* arguments *)
     )

type arg_Queue_purge =
     ( short           (* reserved-1 *)
     * queue_name      (* queue *)
     * no_wait         (* no-wait *)
     )

type arg_Queue_purge_ok =
     ( message_count   (* message-count *)
     )

type arg_Queue_delete =
     ( short           (* reserved-1 *)
     * queue_name      (* queue *)
     * bit             (* if-unused *)
     * bit             (* if-empty *)
     * no_wait         (* no-wait *)
     )

type arg_Queue_delete_ok =
     ( message_count   (* message-count *)
     )

type arg_Basic_qos =
     ( long            (* prefetch-size *)
     * short           (* prefetch-count *)
     * bit             (* global *)
     )

type arg_Basic_consume =
     ( short           (* reserved-1 *)
     * queue_name      (* queue *)
     * consumer_tag    (* consumer-tag *)
     * no_local        (* no-local *)
     * no_ack          (* no-ack *)
     * bit             (* exclusive *)
     * no_wait         (* no-wait *)
     * table           (* arguments *)
     )

type arg_Basic_consume_ok =
     ( consumer_tag    (* consumer-tag *)
     )

type arg_Basic_cancel =
     ( consumer_tag    (* consumer-tag *)
     * no_wait         (* no-wait *)
     )

type arg_Basic_cancel_ok =
     ( consumer_tag    (* consumer-tag *)
     )

type arg_Basic_publish =
     ( short           (* reserved-1 *)
     * exchange_name   (* exchange *)
     * shortstr        (* routing-key *)
     * bit             (* mandatory *)
     * bit             (* immediate *)
     )

type arg_Basic_return =
     ( reply_code      (* reply-code *)
     * reply_text      (* reply-text *)
     * exchange_name   (* exchange *)
     * shortstr        (* routing-key *)
     )

type arg_Basic_deliver =
     ( consumer_tag    (* consumer-tag *)
     * delivery_tag    (* delivery-tag *)
     * redelivered     (* redelivered *)
     * exchange_name   (* exchange *)
     * shortstr        (* routing-key *)
     )

type arg_Basic_get =
     ( short           (* reserved-1 *)
     * queue_name      (* queue *)
     * no_ack          (* no-ack *)
     )

type arg_Basic_get_ok =
     ( delivery_tag    (* delivery-tag *)
     * redelivered     (* redelivered *)
     * exchange_name   (* exchange *)
     * shortstr        (* routing-key *)
     * message_count   (* message-count *)
     )

type arg_Basic_get_empty =
     ( shortstr        (* reserved-1 *)
     )

type arg_Basic_ack =
     ( delivery_tag    (* delivery-tag *)
     * bit             (* multiple *)
     )

type arg_Basic_reject =
     ( delivery_tag    (* delivery-tag *)
     * bit             (* requeue *)
     )

type arg_Basic_recover_async =
     ( bit             (* requeue *)
     )

type arg_Basic_recover =
     ( bit             (* requeue *)
     )

type method_t = [
 | `Connection_start of arg_Connection_start
 | `Connection_start_ok of arg_Connection_start_ok
 | `Connection_secure of arg_Connection_secure
 | `Connection_secure_ok of arg_Connection_secure_ok
 | `Connection_tune of arg_Connection_tune
 | `Connection_tune_ok of arg_Connection_tune_ok
 | `Connection_open of arg_Connection_open
 | `Connection_open_ok of arg_Connection_open_ok
 | `Connection_close of arg_Connection_close
 | `Connection_close_ok
 | `Channel_open of arg_Channel_open
 | `Channel_open_ok of arg_Channel_open_ok
 | `Channel_flow of arg_Channel_flow
 | `Channel_flow_ok of arg_Channel_flow_ok
 | `Channel_close of arg_Channel_close
 | `Channel_close_ok
 | `Exchange_declare of arg_Exchange_declare
 | `Exchange_declare_ok
 | `Exchange_delete of arg_Exchange_delete
 | `Exchange_delete_ok
 | `Queue_declare of arg_Queue_declare
 | `Queue_declare_ok of arg_Queue_declare_ok
 | `Queue_bind of arg_Queue_bind
 | `Queue_bind_ok
 | `Queue_unbind of arg_Queue_unbind
 | `Queue_unbind_ok
 | `Queue_purge of arg_Queue_purge
 | `Queue_purge_ok of arg_Queue_purge_ok
 | `Queue_delete of arg_Queue_delete
 | `Queue_delete_ok of arg_Queue_delete_ok
 | `Basic_qos of arg_Basic_qos
 | `Basic_qos_ok
 | `Basic_consume of arg_Basic_consume
 | `Basic_consume_ok of arg_Basic_consume_ok
 | `Basic_cancel of arg_Basic_cancel
 | `Basic_cancel_ok of arg_Basic_cancel_ok
 | `Basic_publish of arg_Basic_publish
 | `Basic_return of arg_Basic_return
 | `Basic_deliver of arg_Basic_deliver
 | `Basic_get of arg_Basic_get
 | `Basic_get_ok of arg_Basic_get_ok
 | `Basic_get_empty of arg_Basic_get_empty
 | `Basic_ack of arg_Basic_ack
 | `Basic_reject of arg_Basic_reject
 | `Basic_recover_async of arg_Basic_recover_async
 | `Basic_recover of arg_Basic_recover
 | `Basic_recover_ok
 | `Tx_select
 | `Tx_select_ok
 | `Tx_commit
 | `Tx_commit_ok
 | `Tx_rollback
 | `Tx_rollback_ok
 ]

type sync_client_to_server_method_t = [
 | `Connection_start_ok of arg_Connection_start_ok
 | `Connection_secure_ok of arg_Connection_secure_ok
 | `Connection_tune_ok of arg_Connection_tune_ok
 | `Connection_open of arg_Connection_open
 | `Connection_close of arg_Connection_close
 | `Connection_close_ok
 | `Channel_open of arg_Channel_open
 | `Channel_flow of arg_Channel_flow
 | `Channel_flow_ok of arg_Channel_flow_ok
 | `Channel_close of arg_Channel_close
 | `Channel_close_ok
 | `Exchange_declare of arg_Exchange_declare
 | `Exchange_delete of arg_Exchange_delete
 | `Queue_declare of arg_Queue_declare
 | `Queue_bind of arg_Queue_bind
 | `Queue_unbind of arg_Queue_unbind
 | `Queue_purge of arg_Queue_purge
 | `Queue_delete of arg_Queue_delete
 | `Basic_qos of arg_Basic_qos
 | `Basic_consume of arg_Basic_consume
 | `Basic_cancel of arg_Basic_cancel
 | `Basic_get of arg_Basic_get
 | `Basic_recover of arg_Basic_recover
 | `Tx_select
 | `Tx_commit
 | `Tx_rollback
 ]

type sync_server_to_client_method_t = [
 | `Connection_start of arg_Connection_start
 | `Connection_secure of arg_Connection_secure
 | `Connection_tune of arg_Connection_tune
 | `Connection_open_ok of arg_Connection_open_ok
 | `Connection_close of arg_Connection_close
 | `Connection_close_ok
 | `Channel_open_ok of arg_Channel_open_ok
 | `Channel_flow of arg_Channel_flow
 | `Channel_flow_ok of arg_Channel_flow_ok
 | `Channel_close of arg_Channel_close
 | `Channel_close_ok
 | `Exchange_declare_ok
 | `Exchange_delete_ok
 | `Queue_declare_ok of arg_Queue_declare_ok
 | `Queue_bind_ok
 | `Queue_unbind_ok
 | `Queue_purge_ok of arg_Queue_purge_ok
 | `Queue_delete_ok of arg_Queue_delete_ok
 | `Basic_qos_ok
 | `Basic_consume_ok of arg_Basic_consume_ok
 | `Basic_cancel_ok of arg_Basic_cancel_ok
 | `Basic_get_ok of arg_Basic_get_ok
 | `Basic_get_empty of arg_Basic_get_empty
 | `Basic_recover_ok
 | `Tx_select_ok
 | `Tx_commit_ok
 | `Tx_rollback_ok
 ]

type sync_client_initiated_method_t = [
 | `Connection_open of arg_Connection_open
 | `Connection_close of arg_Connection_close
 | `Channel_open of arg_Channel_open
 | `Channel_flow of arg_Channel_flow
 | `Channel_close of arg_Channel_close
 | `Exchange_declare of arg_Exchange_declare
 | `Exchange_delete of arg_Exchange_delete
 | `Queue_declare of arg_Queue_declare
 | `Queue_bind of arg_Queue_bind
 | `Queue_unbind of arg_Queue_unbind
 | `Queue_purge of arg_Queue_purge
 | `Queue_delete of arg_Queue_delete
 | `Basic_qos of arg_Basic_qos
 | `Basic_consume of arg_Basic_consume
 | `Basic_cancel of arg_Basic_cancel
 | `Basic_get of arg_Basic_get
 | `Basic_recover of arg_Basic_recover
 | `Tx_select
 | `Tx_commit
 | `Tx_rollback
 ]

type sync_server_initiated_method_t = [
 | `Connection_start of arg_Connection_start
 | `Connection_secure of arg_Connection_secure
 | `Connection_tune of arg_Connection_tune
 | `Connection_close of arg_Connection_close
 | `Channel_flow of arg_Channel_flow
 | `Channel_close of arg_Channel_close
 ]

type async_client_to_server_method_t = [
 | `Basic_publish of arg_Basic_publish
 | `Basic_ack of arg_Basic_ack
 | `Basic_reject of arg_Basic_reject
 | `Basic_recover_async of arg_Basic_recover_async
 ]

type async_server_to_client_method_t = [
 | `Basic_return of arg_Basic_return
 | `Basic_deliver of arg_Basic_deliver
 ]

type content_method_t = [
 | `Basic_publish of arg_Basic_publish
 | `Basic_return of arg_Basic_return
 | `Basic_deliver of arg_Basic_deliver
 | `Basic_get_ok of arg_Basic_get_ok
 ]

type method_type_t = [
 | `Connection_start
 | `Connection_start_ok
 | `Connection_secure
 | `Connection_secure_ok
 | `Connection_tune
 | `Connection_tune_ok
 | `Connection_open
 | `Connection_open_ok
 | `Connection_close
 | `Connection_close_ok
 | `Channel_open
 | `Channel_open_ok
 | `Channel_flow
 | `Channel_flow_ok
 | `Channel_close
 | `Channel_close_ok
 | `Exchange_declare
 | `Exchange_declare_ok
 | `Exchange_delete
 | `Exchange_delete_ok
 | `Queue_declare
 | `Queue_declare_ok
 | `Queue_bind
 | `Queue_bind_ok
 | `Queue_unbind
 | `Queue_unbind_ok
 | `Queue_purge
 | `Queue_purge_ok
 | `Queue_delete
 | `Queue_delete_ok
 | `Basic_qos
 | `Basic_qos_ok
 | `Basic_consume
 | `Basic_consume_ok
 | `Basic_cancel
 | `Basic_cancel_ok
 | `Basic_publish
 | `Basic_return
 | `Basic_deliver
 | `Basic_get
 | `Basic_get_ok
 | `Basic_get_empty
 | `Basic_ack
 | `Basic_reject
 | `Basic_recover_async
 | `Basic_recover
 | `Basic_recover_ok
 | `Tx_select
 | `Tx_select_ok
 | `Tx_commit
 | `Tx_commit_ok
 | `Tx_rollback
 | `Tx_rollback_ok
 ]

type sync_client_to_server_method_type_t = [
 | `Connection_start_ok
 | `Connection_secure_ok
 | `Connection_tune_ok
 | `Connection_open
 | `Connection_close
 | `Connection_close_ok
 | `Channel_open
 | `Channel_flow
 | `Channel_flow_ok
 | `Channel_close
 | `Channel_close_ok
 | `Exchange_declare
 | `Exchange_delete
 | `Queue_declare
 | `Queue_bind
 | `Queue_unbind
 | `Queue_purge
 | `Queue_delete
 | `Basic_qos
 | `Basic_consume
 | `Basic_cancel
 | `Basic_get
 | `Basic_recover
 | `Tx_select
 | `Tx_commit
 | `Tx_rollback
 ]

type sync_server_to_client_method_type_t = [
 | `Connection_start
 | `Connection_secure
 | `Connection_tune
 | `Connection_open_ok
 | `Connection_close
 | `Connection_close_ok
 | `Channel_open_ok
 | `Channel_flow
 | `Channel_flow_ok
 | `Channel_close
 | `Channel_close_ok
 | `Exchange_declare_ok
 | `Exchange_delete_ok
 | `Queue_declare_ok
 | `Queue_bind_ok
 | `Queue_unbind_ok
 | `Queue_purge_ok
 | `Queue_delete_ok
 | `Basic_qos_ok
 | `Basic_consume_ok
 | `Basic_cancel_ok
 | `Basic_get_ok
 | `Basic_get_empty
 | `Basic_recover_ok
 | `Tx_select_ok
 | `Tx_commit_ok
 | `Tx_rollback_ok
 ]

type sync_client_initiated_method_type_t = [
 | `Connection_open
 | `Connection_close
 | `Channel_open
 | `Channel_flow
 | `Channel_close
 | `Exchange_declare
 | `Exchange_delete
 | `Queue_declare
 | `Queue_bind
 | `Queue_unbind
 | `Queue_purge
 | `Queue_delete
 | `Basic_qos
 | `Basic_consume
 | `Basic_cancel
 | `Basic_get
 | `Basic_recover
 | `Tx_select
 | `Tx_commit
 | `Tx_rollback
 ]

type sync_server_initiated_method_type_t = [
 | `Connection_start
 | `Connection_secure
 | `Connection_tune
 | `Connection_close
 | `Channel_flow
 | `Channel_close
 ]

type async_client_to_server_method_type_t = [
 | `Basic_publish
 | `Basic_ack
 | `Basic_reject
 | `Basic_recover_async
 ]

type async_server_to_client_method_type_t = [
 | `Basic_return
 | `Basic_deliver
 ]

type content_method_type_t = [
 | `Basic_publish
 | `Basic_return
 | `Basic_deliver
 | `Basic_get_ok
 ]

val type_of_method : method_t -> method_type_t

val responses_of_method : method_type_t -> method_type_t list

val content_method_types : method_type_t list

val string_of_method_type : method_type_t -> string

val string_of_method_id : class_id:int -> meth_id:int -> string

type props_t = [
 | `P_connection

 | `P_channel

 | `P_exchange

 | `P_queue

 | `P_basic of
     ( shortstr option (* content-type *)
     * shortstr option (* content-encoding *)
     * table option    (* headers *)
     * octet option    (* delivery-mode *)
     * octet option    (* priority *)
     * shortstr option (* correlation-id *)
     * shortstr option (* reply-to *)
     * shortstr option (* expiration *)
     * shortstr option (* message-id *)
     * timestamp option (* timestamp *)
     * shortstr option (* type *)
     * shortstr option (* user-id *)
     * shortstr option (* app-id *)
     * shortstr option (* reserved *)
     )
 | `P_tx

 ]

type message_t = [
  | `Method of method_t
  | `Header of props_t * int64 (* size *)
  | `Body of Xdr_mstring.mstring list
  | `Heartbeat
  | `Proto_header of string
]

val decode_method_message : Netamqp_types.frame -> method_t

val encode_method_message : method_t -> int -> Netamqp_types.frame

val decode_header_message : Netamqp_types.frame -> props_t * int64

val encode_header_message : props_t -> int64 -> int -> Netamqp_types.frame

val encode_heartbeat_message : unit -> Netamqp_types.frame

val encode_body_message : Xdr_mstring.mstring list -> int -> Netamqp_types.frame

val encode_proto_header_message : string -> Netamqp_types.frame

val decode_message : Netamqp_types.frame -> message_t

val encode_message : message_t -> int -> Netamqp_types.frame

