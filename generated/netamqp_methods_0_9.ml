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

let type_of_method m =
  match m with
   | `Connection_start _ -> `Connection_start
   | `Connection_start_ok _ -> `Connection_start_ok
   | `Connection_secure _ -> `Connection_secure
   | `Connection_secure_ok _ -> `Connection_secure_ok
   | `Connection_tune _ -> `Connection_tune
   | `Connection_tune_ok _ -> `Connection_tune_ok
   | `Connection_open _ -> `Connection_open
   | `Connection_open_ok _ -> `Connection_open_ok
   | `Connection_close _ -> `Connection_close
   | `Connection_close_ok -> `Connection_close_ok
   | `Channel_open _ -> `Channel_open
   | `Channel_open_ok _ -> `Channel_open_ok
   | `Channel_flow _ -> `Channel_flow
   | `Channel_flow_ok _ -> `Channel_flow_ok
   | `Channel_close _ -> `Channel_close
   | `Channel_close_ok -> `Channel_close_ok
   | `Exchange_declare _ -> `Exchange_declare
   | `Exchange_declare_ok -> `Exchange_declare_ok
   | `Exchange_delete _ -> `Exchange_delete
   | `Exchange_delete_ok -> `Exchange_delete_ok
   | `Queue_declare _ -> `Queue_declare
   | `Queue_declare_ok _ -> `Queue_declare_ok
   | `Queue_bind _ -> `Queue_bind
   | `Queue_bind_ok -> `Queue_bind_ok
   | `Queue_unbind _ -> `Queue_unbind
   | `Queue_unbind_ok -> `Queue_unbind_ok
   | `Queue_purge _ -> `Queue_purge
   | `Queue_purge_ok _ -> `Queue_purge_ok
   | `Queue_delete _ -> `Queue_delete
   | `Queue_delete_ok _ -> `Queue_delete_ok
   | `Basic_qos _ -> `Basic_qos
   | `Basic_qos_ok -> `Basic_qos_ok
   | `Basic_consume _ -> `Basic_consume
   | `Basic_consume_ok _ -> `Basic_consume_ok
   | `Basic_cancel _ -> `Basic_cancel
   | `Basic_cancel_ok _ -> `Basic_cancel_ok
   | `Basic_publish _ -> `Basic_publish
   | `Basic_return _ -> `Basic_return
   | `Basic_deliver _ -> `Basic_deliver
   | `Basic_get _ -> `Basic_get
   | `Basic_get_ok _ -> `Basic_get_ok
   | `Basic_get_empty _ -> `Basic_get_empty
   | `Basic_ack _ -> `Basic_ack
   | `Basic_reject _ -> `Basic_reject
   | `Basic_recover_async _ -> `Basic_recover_async
   | `Basic_recover _ -> `Basic_recover
   | `Basic_recover_ok -> `Basic_recover_ok
   | `Tx_select -> `Tx_select
   | `Tx_select_ok -> `Tx_select_ok
   | `Tx_commit -> `Tx_commit
   | `Tx_commit_ok -> `Tx_commit_ok
   | `Tx_rollback -> `Tx_rollback
   | `Tx_rollback_ok -> `Tx_rollback_ok

let responses_of_method m =
  match m with
   | `Connection_start -> [ `Connection_start_ok ]
   | `Connection_start_ok -> [  ]
   | `Connection_secure -> [ `Connection_secure_ok ]
   | `Connection_secure_ok -> [  ]
   | `Connection_tune -> [ `Connection_tune_ok ]
   | `Connection_tune_ok -> [  ]
   | `Connection_open -> [ `Connection_open_ok ]
   | `Connection_open_ok -> [  ]
   | `Connection_close -> [ `Connection_close_ok ]
   | `Connection_close_ok -> [  ]
   | `Channel_open -> [ `Channel_open_ok ]
   | `Channel_open_ok -> [  ]
   | `Channel_flow -> [ `Channel_flow_ok ]
   | `Channel_flow_ok -> [  ]
   | `Channel_close -> [ `Channel_close_ok ]
   | `Channel_close_ok -> [  ]
   | `Exchange_declare -> [ `Exchange_declare_ok ]
   | `Exchange_declare_ok -> [  ]
   | `Exchange_delete -> [ `Exchange_delete_ok ]
   | `Exchange_delete_ok -> [  ]
   | `Queue_declare -> [ `Queue_declare_ok ]
   | `Queue_declare_ok -> [  ]
   | `Queue_bind -> [ `Queue_bind_ok ]
   | `Queue_bind_ok -> [  ]
   | `Queue_unbind -> [ `Queue_unbind_ok ]
   | `Queue_unbind_ok -> [  ]
   | `Queue_purge -> [ `Queue_purge_ok ]
   | `Queue_purge_ok -> [  ]
   | `Queue_delete -> [ `Queue_delete_ok ]
   | `Queue_delete_ok -> [  ]
   | `Basic_qos -> [ `Basic_qos_ok ]
   | `Basic_qos_ok -> [  ]
   | `Basic_consume -> [ `Basic_consume_ok ]
   | `Basic_consume_ok -> [  ]
   | `Basic_cancel -> [ `Basic_cancel_ok ]
   | `Basic_cancel_ok -> [  ]
   | `Basic_publish -> [  ]
   | `Basic_return -> [  ]
   | `Basic_deliver -> [  ]
   | `Basic_get -> [ `Basic_get_ok; `Basic_get_empty ]
   | `Basic_get_ok -> [  ]
   | `Basic_get_empty -> [  ]
   | `Basic_ack -> [  ]
   | `Basic_reject -> [  ]
   | `Basic_recover_async -> [  ]
   | `Basic_recover -> [ `Basic_recover_ok ]
   | `Basic_recover_ok -> [  ]
   | `Tx_select -> [ `Tx_select_ok ]
   | `Tx_select_ok -> [  ]
   | `Tx_commit -> [ `Tx_commit_ok ]
   | `Tx_commit_ok -> [  ]
   | `Tx_rollback -> [ `Tx_rollback_ok ]
   | `Tx_rollback_ok -> [  ]

let content_method_types =
 [ `Basic_publish; `Basic_return; `Basic_deliver; `Basic_get_ok ]

let string_of_method_type =
  function
   | `Connection_start -> "connection.start"
   | `Connection_start_ok -> "connection.start-ok"
   | `Connection_secure -> "connection.secure"
   | `Connection_secure_ok -> "connection.secure-ok"
   | `Connection_tune -> "connection.tune"
   | `Connection_tune_ok -> "connection.tune-ok"
   | `Connection_open -> "connection.open"
   | `Connection_open_ok -> "connection.open-ok"
   | `Connection_close -> "connection.close"
   | `Connection_close_ok -> "connection.close-ok"
   | `Channel_open -> "channel.open"
   | `Channel_open_ok -> "channel.open-ok"
   | `Channel_flow -> "channel.flow"
   | `Channel_flow_ok -> "channel.flow-ok"
   | `Channel_close -> "channel.close"
   | `Channel_close_ok -> "channel.close-ok"
   | `Exchange_declare -> "exchange.declare"
   | `Exchange_declare_ok -> "exchange.declare-ok"
   | `Exchange_delete -> "exchange.delete"
   | `Exchange_delete_ok -> "exchange.delete-ok"
   | `Queue_declare -> "queue.declare"
   | `Queue_declare_ok -> "queue.declare-ok"
   | `Queue_bind -> "queue.bind"
   | `Queue_bind_ok -> "queue.bind-ok"
   | `Queue_unbind -> "queue.unbind"
   | `Queue_unbind_ok -> "queue.unbind-ok"
   | `Queue_purge -> "queue.purge"
   | `Queue_purge_ok -> "queue.purge-ok"
   | `Queue_delete -> "queue.delete"
   | `Queue_delete_ok -> "queue.delete-ok"
   | `Basic_qos -> "basic.qos"
   | `Basic_qos_ok -> "basic.qos-ok"
   | `Basic_consume -> "basic.consume"
   | `Basic_consume_ok -> "basic.consume-ok"
   | `Basic_cancel -> "basic.cancel"
   | `Basic_cancel_ok -> "basic.cancel-ok"
   | `Basic_publish -> "basic.publish"
   | `Basic_return -> "basic.return"
   | `Basic_deliver -> "basic.deliver"
   | `Basic_get -> "basic.get"
   | `Basic_get_ok -> "basic.get-ok"
   | `Basic_get_empty -> "basic.get-empty"
   | `Basic_ack -> "basic.ack"
   | `Basic_reject -> "basic.reject"
   | `Basic_recover_async -> "basic.recover-async"
   | `Basic_recover -> "basic.recover"
   | `Basic_recover_ok -> "basic.recover-ok"
   | `Tx_select -> "tx.select"
   | `Tx_select_ok -> "tx.select-ok"
   | `Tx_commit -> "tx.commit"
   | `Tx_commit_ok -> "tx.commit-ok"
   | `Tx_rollback -> "tx.rollback"
   | `Tx_rollback_ok -> "tx.rollback-ok"
let string_of_method_id ~class_id ~meth_id =
  match (class_id,meth_id) with
   | (10, 10) -> "connection.start"
   | (10, 11) -> "connection.start-ok"
   | (10, 20) -> "connection.secure"
   | (10, 21) -> "connection.secure-ok"
   | (10, 30) -> "connection.tune"
   | (10, 31) -> "connection.tune-ok"
   | (10, 40) -> "connection.open"
   | (10, 41) -> "connection.open-ok"
   | (10, 50) -> "connection.close"
   | (10, 51) -> "connection.close-ok"
   | (20, 10) -> "channel.open"
   | (20, 11) -> "channel.open-ok"
   | (20, 20) -> "channel.flow"
   | (20, 21) -> "channel.flow-ok"
   | (20, 40) -> "channel.close"
   | (20, 41) -> "channel.close-ok"
   | (40, 10) -> "exchange.declare"
   | (40, 11) -> "exchange.declare-ok"
   | (40, 20) -> "exchange.delete"
   | (40, 21) -> "exchange.delete-ok"
   | (50, 10) -> "queue.declare"
   | (50, 11) -> "queue.declare-ok"
   | (50, 20) -> "queue.bind"
   | (50, 21) -> "queue.bind-ok"
   | (50, 50) -> "queue.unbind"
   | (50, 51) -> "queue.unbind-ok"
   | (50, 30) -> "queue.purge"
   | (50, 31) -> "queue.purge-ok"
   | (50, 40) -> "queue.delete"
   | (50, 41) -> "queue.delete-ok"
   | (60, 10) -> "basic.qos"
   | (60, 11) -> "basic.qos-ok"
   | (60, 20) -> "basic.consume"
   | (60, 21) -> "basic.consume-ok"
   | (60, 30) -> "basic.cancel"
   | (60, 31) -> "basic.cancel-ok"
   | (60, 40) -> "basic.publish"
   | (60, 50) -> "basic.return"
   | (60, 60) -> "basic.deliver"
   | (60, 70) -> "basic.get"
   | (60, 71) -> "basic.get-ok"
   | (60, 72) -> "basic.get-empty"
   | (60, 80) -> "basic.ack"
   | (60, 90) -> "basic.reject"
   | (60, 100) -> "basic.recover-async"
   | (60, 110) -> "basic.recover"
   | (60, 111) -> "basic.recover-ok"
   | (90, 10) -> "tx.select"
   | (90, 11) -> "tx.select-ok"
   | (90, 20) -> "tx.commit"
   | (90, 21) -> "tx.commit-ok"
   | (90, 30) -> "tx.rollback"
   | (90, 31) -> "tx.rollback-ok"
   | _ -> raise Not_found

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

let decode_method_message _frame =
  let _s = Xdr_mstring.concat_mstrings _frame.Netamqp_types.frame_payload in
  let _l = String.length _s in
  if _l < 4 then raise(Netamqp_types.Decode_error "Message too short");
  let _class_index = Netamqp_rtypes.read_uint2_unsafe _s 0 in
  let _meth_index = Netamqp_rtypes.read_uint2_unsafe _s 2 in
  let _c = ref 4 in
  match (_class_index lsl 16) + _meth_index with
   | 655370 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let version_major = Char.code(String.unsafe_get _s (!_c+0)) in
        let version_minor = Char.code(String.unsafe_get _s (!_c+1)) in
        _c := !_c + 2;
        let server_properties = Netamqp_rtypes.decode_table _s _c _l in
        let mechanisms = Netamqp_rtypes.decode_longstr _s _c _l in
        let locales = Netamqp_rtypes.decode_longstr _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Connection_start(version_major,version_minor,server_properties,mechanisms,locales)
   | 655371 ->
        let client_properties = Netamqp_rtypes.decode_table _s _c _l in
        let mechanism = Netamqp_rtypes.decode_shortstr _s _c _l in
        let response = Netamqp_rtypes.decode_longstr _s _c _l in
        let locale = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Connection_start_ok(client_properties,mechanism,response,locale)
   | 655380 ->
        let challenge = Netamqp_rtypes.decode_longstr _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Connection_secure(challenge)
   | 655381 ->
        let response = Netamqp_rtypes.decode_longstr _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Connection_secure_ok(response)
   | 655390 ->
        if !_c > _l-8 then raise(Netamqp_types.Decode_error "Message too short");
        let channel_max = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        let frame_max = Rtypes.read_uint4_unsafe _s (!_c+2) in
        let heartbeat = Netamqp_rtypes.read_uint2_unsafe _s (!_c+6) in
        _c := !_c + 8;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Connection_tune(channel_max,frame_max,heartbeat)
   | 655391 ->
        if !_c > _l-8 then raise(Netamqp_types.Decode_error "Message too short");
        let channel_max = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        let frame_max = Rtypes.read_uint4_unsafe _s (!_c+2) in
        let heartbeat = Netamqp_rtypes.read_uint2_unsafe _s (!_c+6) in
        _c := !_c + 8;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Connection_tune_ok(channel_max,frame_max,heartbeat)
   | 655400 ->
        let virtual_host = Netamqp_rtypes.decode_shortstr _s _c _l in
        let reserved_1 = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let reserved_2 = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 1;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Connection_open(virtual_host,reserved_1,reserved_2)
   | 655401 ->
        let reserved_1 = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Connection_open_ok(reserved_1)
   | 655410 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let reply_code = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        _c := !_c + 2;
        let reply_text = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-4 then raise(Netamqp_types.Decode_error "Message too short");
        let class_id = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        let method_id = Netamqp_rtypes.read_uint2_unsafe _s (!_c+2) in
        _c := !_c + 4;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Connection_close(reply_code,reply_text,class_id,method_id)
   | 655411 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Connection_close_ok
   | 1310730 ->
        let reserved_1 = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Channel_open(reserved_1)
   | 1310731 ->
        let reserved_1 = Netamqp_rtypes.decode_longstr _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Channel_open_ok(reserved_1)
   | 1310740 ->
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let active = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 1;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Channel_flow(active)
   | 1310741 ->
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let active = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 1;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Channel_flow_ok(active)
   | 1310760 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let reply_code = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        _c := !_c + 2;
        let reply_text = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-4 then raise(Netamqp_types.Decode_error "Message too short");
        let class_id = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        let method_id = Netamqp_rtypes.read_uint2_unsafe _s (!_c+2) in
        _c := !_c + 4;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Channel_close(reply_code,reply_text,class_id,method_id)
   | 1310761 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Channel_close_ok
   | 2621450 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let reserved_1 = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        _c := !_c + 2;
        let exchange = Netamqp_rtypes.decode_shortstr _s _c _l in
        let typ = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let passive = (_x lsr 0) land 1 <> 0 in
        let durable = (_x lsr 1) land 1 <> 0 in
        let reserved_2 = (_x lsr 2) land 1 <> 0 in
        let reserved_3 = (_x lsr 3) land 1 <> 0 in
        let no_wait = (_x lsr 4) land 1 <> 0 in
        _c := !_c + 1;
        let arguments = Netamqp_rtypes.decode_table _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Exchange_declare(reserved_1,exchange,typ,passive,durable,reserved_2,reserved_3,no_wait,arguments)
   | 2621451 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Exchange_declare_ok
   | 2621460 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let reserved_1 = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        _c := !_c + 2;
        let exchange = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let if_unused = (_x lsr 0) land 1 <> 0 in
        let no_wait = (_x lsr 1) land 1 <> 0 in
        _c := !_c + 1;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Exchange_delete(reserved_1,exchange,if_unused,no_wait)
   | 2621461 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Exchange_delete_ok
   | 3276810 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let reserved_1 = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        _c := !_c + 2;
        let queue = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let passive = (_x lsr 0) land 1 <> 0 in
        let durable = (_x lsr 1) land 1 <> 0 in
        let exclusive = (_x lsr 2) land 1 <> 0 in
        let auto_delete = (_x lsr 3) land 1 <> 0 in
        let no_wait = (_x lsr 4) land 1 <> 0 in
        _c := !_c + 1;
        let arguments = Netamqp_rtypes.decode_table _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Queue_declare(reserved_1,queue,passive,durable,exclusive,auto_delete,no_wait,arguments)
   | 3276811 ->
        let queue = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-8 then raise(Netamqp_types.Decode_error "Message too short");
        let message_count = Rtypes.read_uint4_unsafe _s (!_c+0) in
        let consumer_count = Rtypes.read_uint4_unsafe _s (!_c+4) in
        _c := !_c + 8;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Queue_declare_ok(queue,message_count,consumer_count)
   | 3276820 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let reserved_1 = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        _c := !_c + 2;
        let queue = Netamqp_rtypes.decode_shortstr _s _c _l in
        let exchange = Netamqp_rtypes.decode_shortstr _s _c _l in
        let routing_key = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let no_wait = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 1;
        let arguments = Netamqp_rtypes.decode_table _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Queue_bind(reserved_1,queue,exchange,routing_key,no_wait,arguments)
   | 3276821 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Queue_bind_ok
   | 3276850 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let reserved_1 = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        _c := !_c + 2;
        let queue = Netamqp_rtypes.decode_shortstr _s _c _l in
        let exchange = Netamqp_rtypes.decode_shortstr _s _c _l in
        let routing_key = Netamqp_rtypes.decode_shortstr _s _c _l in
        let arguments = Netamqp_rtypes.decode_table _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Queue_unbind(reserved_1,queue,exchange,routing_key,arguments)
   | 3276851 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Queue_unbind_ok
   | 3276830 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let reserved_1 = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        _c := !_c + 2;
        let queue = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let no_wait = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 1;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Queue_purge(reserved_1,queue,no_wait)
   | 3276831 ->
        if !_c > _l-4 then raise(Netamqp_types.Decode_error "Message too short");
        let message_count = Rtypes.read_uint4_unsafe _s (!_c+0) in
        _c := !_c + 4;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Queue_purge_ok(message_count)
   | 3276840 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let reserved_1 = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        _c := !_c + 2;
        let queue = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let if_unused = (_x lsr 0) land 1 <> 0 in
        let if_empty = (_x lsr 1) land 1 <> 0 in
        let no_wait = (_x lsr 2) land 1 <> 0 in
        _c := !_c + 1;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Queue_delete(reserved_1,queue,if_unused,if_empty,no_wait)
   | 3276841 ->
        if !_c > _l-4 then raise(Netamqp_types.Decode_error "Message too short");
        let message_count = Rtypes.read_uint4_unsafe _s (!_c+0) in
        _c := !_c + 4;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Queue_delete_ok(message_count)
   | 3932170 ->
        if !_c > _l-7 then raise(Netamqp_types.Decode_error "Message too short");
        let prefetch_size = Rtypes.read_uint4_unsafe _s (!_c+0) in
        let prefetch_count = Netamqp_rtypes.read_uint2_unsafe _s (!_c+4) in
        let _x = Char.code(String.unsafe_get _s (!_c+6)) in
        let global = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 7;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_qos(prefetch_size,prefetch_count,global)
   | 3932171 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_qos_ok
   | 3932180 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let reserved_1 = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        _c := !_c + 2;
        let queue = Netamqp_rtypes.decode_shortstr _s _c _l in
        let consumer_tag = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let no_local = (_x lsr 0) land 1 <> 0 in
        let no_ack = (_x lsr 1) land 1 <> 0 in
        let exclusive = (_x lsr 2) land 1 <> 0 in
        let no_wait = (_x lsr 3) land 1 <> 0 in
        _c := !_c + 1;
        let arguments = Netamqp_rtypes.decode_table _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_consume(reserved_1,queue,consumer_tag,no_local,no_ack,exclusive,no_wait,arguments)
   | 3932181 ->
        let consumer_tag = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_consume_ok(consumer_tag)
   | 3932190 ->
        let consumer_tag = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let no_wait = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 1;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_cancel(consumer_tag,no_wait)
   | 3932191 ->
        let consumer_tag = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_cancel_ok(consumer_tag)
   | 3932200 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let reserved_1 = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        _c := !_c + 2;
        let exchange = Netamqp_rtypes.decode_shortstr _s _c _l in
        let routing_key = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let mandatory = (_x lsr 0) land 1 <> 0 in
        let immediate = (_x lsr 1) land 1 <> 0 in
        _c := !_c + 1;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_publish(reserved_1,exchange,routing_key,mandatory,immediate)
   | 3932210 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let reply_code = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        _c := !_c + 2;
        let reply_text = Netamqp_rtypes.decode_shortstr _s _c _l in
        let exchange = Netamqp_rtypes.decode_shortstr _s _c _l in
        let routing_key = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_return(reply_code,reply_text,exchange,routing_key)
   | 3932220 ->
        let consumer_tag = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-9 then raise(Netamqp_types.Decode_error "Message too short");
        let delivery_tag = Rtypes.read_uint8_unsafe _s (!_c+0) in
        let _x = Char.code(String.unsafe_get _s (!_c+8)) in
        let redelivered = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 9;
        let exchange = Netamqp_rtypes.decode_shortstr _s _c _l in
        let routing_key = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_deliver(consumer_tag,delivery_tag,redelivered,exchange,routing_key)
   | 3932230 ->
        if !_c > _l-2 then raise(Netamqp_types.Decode_error "Message too short");
        let reserved_1 = Netamqp_rtypes.read_uint2_unsafe _s (!_c+0) in
        _c := !_c + 2;
        let queue = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let no_ack = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 1;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_get(reserved_1,queue,no_ack)
   | 3932231 ->
        if !_c > _l-9 then raise(Netamqp_types.Decode_error "Message too short");
        let delivery_tag = Rtypes.read_uint8_unsafe _s (!_c+0) in
        let _x = Char.code(String.unsafe_get _s (!_c+8)) in
        let redelivered = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 9;
        let exchange = Netamqp_rtypes.decode_shortstr _s _c _l in
        let routing_key = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c > _l-4 then raise(Netamqp_types.Decode_error "Message too short");
        let message_count = Rtypes.read_uint4_unsafe _s (!_c+0) in
        _c := !_c + 4;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_get_ok(delivery_tag,redelivered,exchange,routing_key,message_count)
   | 3932232 ->
        let reserved_1 = Netamqp_rtypes.decode_shortstr _s _c _l in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_get_empty(reserved_1)
   | 3932240 ->
        if !_c > _l-9 then raise(Netamqp_types.Decode_error "Message too short");
        let delivery_tag = Rtypes.read_uint8_unsafe _s (!_c+0) in
        let _x = Char.code(String.unsafe_get _s (!_c+8)) in
        let multiple = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 9;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_ack(delivery_tag,multiple)
   | 3932250 ->
        if !_c > _l-9 then raise(Netamqp_types.Decode_error "Message too short");
        let delivery_tag = Rtypes.read_uint8_unsafe _s (!_c+0) in
        let _x = Char.code(String.unsafe_get _s (!_c+8)) in
        let requeue = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 9;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_reject(delivery_tag,requeue)
   | 3932260 ->
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let requeue = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 1;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_recover_async(requeue)
   | 3932270 ->
        if !_c > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
        let _x = Char.code(String.unsafe_get _s (!_c+0)) in
        let requeue = (_x lsr 0) land 1 <> 0 in
        _c := !_c + 1;
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_recover(requeue)
   | 3932271 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Basic_recover_ok
   | 5898250 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Tx_select
   | 5898251 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Tx_select_ok
   | 5898260 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Tx_commit
   | 5898261 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Tx_commit_ok
   | 5898270 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Tx_rollback
   | 5898271 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        `Tx_rollback_ok
    | _ -> raise(Netamqp_types.Decode_error "Unkown class/method")

let encode_method_message _msg _channel =
  let _payload =
    match _msg with
     | `Connection_start(version_major,version_minor,server_properties,mechanisms,locales) ->
          let _acc = [ "\000\n\000\n" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if version_major < 0 || version_major > 255 then raise(Netamqp_types.Encode_error "Value out of range");
          String.unsafe_set _s 0 (Char.unsafe_chr version_major);
          if version_minor < 0 || version_minor > 255 then raise(Netamqp_types.Encode_error "Value out of range");
          String.unsafe_set _s 1 (Char.unsafe_chr version_minor);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_table server_properties in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_longstr mechanisms in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_longstr locales in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Connection_start_ok(client_properties,mechanism,response,locale) ->
          let _acc = [ "\000\n\000\011" ] in
          let _acc_len = 4 in
          let (_x,_l) = Netamqp_rtypes.encode_table client_properties in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr mechanism in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_longstr response in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr locale in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Connection_secure(challenge) ->
          let _acc = [ "\000\n\000\020" ] in
          let _acc_len = 4 in
          let (_x,_l) = Netamqp_rtypes.encode_longstr challenge in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Connection_secure_ok(response) ->
          let _acc = [ "\000\n\000\021" ] in
          let _acc_len = 4 in
          let (_x,_l) = Netamqp_rtypes.encode_longstr response in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Connection_tune(channel_max,frame_max,heartbeat) ->
          let _acc = [ "\000\n\000\030" ] in
          let _acc_len = 4 in
          let _s = String.create 8 in
          if channel_max < 0 || channel_max > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 channel_max;
          Rtypes.write_uint4_unsafe _s 2 frame_max;
          if heartbeat < 0 || heartbeat > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 6 heartbeat;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 8 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Connection_tune_ok(channel_max,frame_max,heartbeat) ->
          let _acc = [ "\000\n\000\031" ] in
          let _acc_len = 4 in
          let _s = String.create 8 in
          if channel_max < 0 || channel_max > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 channel_max;
          Rtypes.write_uint4_unsafe _s 2 frame_max;
          if heartbeat < 0 || heartbeat > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 6 heartbeat;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 8 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Connection_open(virtual_host,reserved_1,reserved_2) ->
          let _acc = [ "\000\n\000(" ] in
          let _acc_len = 4 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr virtual_host in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr reserved_1 in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if reserved_2 then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Connection_open_ok(reserved_1) ->
          let _acc = [ "\000\n\000)" ] in
          let _acc_len = 4 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr reserved_1 in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Connection_close(reply_code,reply_text,class_id,method_id) ->
          let _acc = [ "\000\n\0002" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if reply_code < 0 || reply_code > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 reply_code;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr reply_text in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 4 in
          if class_id < 0 || class_id > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 class_id;
          if method_id < 0 || method_id > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 2 method_id;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 4 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Connection_close_ok ->
          "\000\n\0003"
     | `Channel_open(reserved_1) ->
          let _acc = [ "\000\020\000\n" ] in
          let _acc_len = 4 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr reserved_1 in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Channel_open_ok(reserved_1) ->
          let _acc = [ "\000\020\000\011" ] in
          let _acc_len = 4 in
          let (_x,_l) = Netamqp_rtypes.encode_longstr reserved_1 in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Channel_flow(active) ->
          let _acc = [ "\000\020\000\020" ] in
          let _acc_len = 4 in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if active then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Channel_flow_ok(active) ->
          let _acc = [ "\000\020\000\021" ] in
          let _acc_len = 4 in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if active then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Channel_close(reply_code,reply_text,class_id,method_id) ->
          let _acc = [ "\000\020\000(" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if reply_code < 0 || reply_code > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 reply_code;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr reply_text in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 4 in
          if class_id < 0 || class_id > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 class_id;
          if method_id < 0 || method_id > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 2 method_id;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 4 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Channel_close_ok ->
          "\000\020\000)"
     | `Exchange_declare(reserved_1,exchange,typ,passive,durable,reserved_2,reserved_3,no_wait,arguments) ->
          let _acc = [ "\000(\000\n" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if reserved_1 < 0 || reserved_1 > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 reserved_1;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr exchange in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr typ in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if passive then 1 else 0) lsl 0) lor _x in
          let _x = ((if durable then 1 else 0) lsl 1) lor _x in
          let _x = ((if reserved_2 then 1 else 0) lsl 2) lor _x in
          let _x = ((if reserved_3 then 1 else 0) lsl 3) lor _x in
          let _x = ((if no_wait then 1 else 0) lsl 4) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          let (_x,_l) = Netamqp_rtypes.encode_table arguments in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Exchange_declare_ok ->
          "\000(\000\011"
     | `Exchange_delete(reserved_1,exchange,if_unused,no_wait) ->
          let _acc = [ "\000(\000\020" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if reserved_1 < 0 || reserved_1 > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 reserved_1;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr exchange in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if if_unused then 1 else 0) lsl 0) lor _x in
          let _x = ((if no_wait then 1 else 0) lsl 1) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Exchange_delete_ok ->
          "\000(\000\021"
     | `Queue_declare(reserved_1,queue,passive,durable,exclusive,auto_delete,no_wait,arguments) ->
          let _acc = [ "\0002\000\n" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if reserved_1 < 0 || reserved_1 > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 reserved_1;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr queue in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if passive then 1 else 0) lsl 0) lor _x in
          let _x = ((if durable then 1 else 0) lsl 1) lor _x in
          let _x = ((if exclusive then 1 else 0) lsl 2) lor _x in
          let _x = ((if auto_delete then 1 else 0) lsl 3) lor _x in
          let _x = ((if no_wait then 1 else 0) lsl 4) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          let (_x,_l) = Netamqp_rtypes.encode_table arguments in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Queue_declare_ok(queue,message_count,consumer_count) ->
          let _acc = [ "\0002\000\011" ] in
          let _acc_len = 4 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr queue in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 8 in
          Rtypes.write_uint4_unsafe _s 0 message_count;
          Rtypes.write_uint4_unsafe _s 4 consumer_count;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 8 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Queue_bind(reserved_1,queue,exchange,routing_key,no_wait,arguments) ->
          let _acc = [ "\0002\000\020" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if reserved_1 < 0 || reserved_1 > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 reserved_1;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr queue in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr exchange in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr routing_key in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if no_wait then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          let (_x,_l) = Netamqp_rtypes.encode_table arguments in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Queue_bind_ok ->
          "\0002\000\021"
     | `Queue_unbind(reserved_1,queue,exchange,routing_key,arguments) ->
          let _acc = [ "\0002\0002" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if reserved_1 < 0 || reserved_1 > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 reserved_1;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr queue in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr exchange in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr routing_key in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_table arguments in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Queue_unbind_ok ->
          "\0002\0003"
     | `Queue_purge(reserved_1,queue,no_wait) ->
          let _acc = [ "\0002\000\030" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if reserved_1 < 0 || reserved_1 > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 reserved_1;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr queue in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if no_wait then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Queue_purge_ok(message_count) ->
          let _acc = [ "\0002\000\031" ] in
          let _acc_len = 4 in
          let _s = String.create 4 in
          Rtypes.write_uint4_unsafe _s 0 message_count;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 4 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Queue_delete(reserved_1,queue,if_unused,if_empty,no_wait) ->
          let _acc = [ "\0002\000(" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if reserved_1 < 0 || reserved_1 > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 reserved_1;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr queue in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if if_unused then 1 else 0) lsl 0) lor _x in
          let _x = ((if if_empty then 1 else 0) lsl 1) lor _x in
          let _x = ((if no_wait then 1 else 0) lsl 2) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Queue_delete_ok(message_count) ->
          let _acc = [ "\0002\000)" ] in
          let _acc_len = 4 in
          let _s = String.create 4 in
          Rtypes.write_uint4_unsafe _s 0 message_count;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 4 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_qos(prefetch_size,prefetch_count,global) ->
          let _acc = [ "\000<\000\n" ] in
          let _acc_len = 4 in
          let _s = String.create 7 in
          Rtypes.write_uint4_unsafe _s 0 prefetch_size;
          if prefetch_count < 0 || prefetch_count > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 4 prefetch_count;
          let _x = 0 in
          let _x = ((if global then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 6 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 7 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_qos_ok ->
          "\000<\000\011"
     | `Basic_consume(reserved_1,queue,consumer_tag,no_local,no_ack,exclusive,no_wait,arguments) ->
          let _acc = [ "\000<\000\020" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if reserved_1 < 0 || reserved_1 > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 reserved_1;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr queue in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr consumer_tag in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if no_local then 1 else 0) lsl 0) lor _x in
          let _x = ((if no_ack then 1 else 0) lsl 1) lor _x in
          let _x = ((if exclusive then 1 else 0) lsl 2) lor _x in
          let _x = ((if no_wait then 1 else 0) lsl 3) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          let (_x,_l) = Netamqp_rtypes.encode_table arguments in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_consume_ok(consumer_tag) ->
          let _acc = [ "\000<\000\021" ] in
          let _acc_len = 4 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr consumer_tag in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_cancel(consumer_tag,no_wait) ->
          let _acc = [ "\000<\000\030" ] in
          let _acc_len = 4 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr consumer_tag in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if no_wait then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_cancel_ok(consumer_tag) ->
          let _acc = [ "\000<\000\031" ] in
          let _acc_len = 4 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr consumer_tag in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_publish(reserved_1,exchange,routing_key,mandatory,immediate) ->
          let _acc = [ "\000<\000(" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if reserved_1 < 0 || reserved_1 > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 reserved_1;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr exchange in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr routing_key in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if mandatory then 1 else 0) lsl 0) lor _x in
          let _x = ((if immediate then 1 else 0) lsl 1) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_return(reply_code,reply_text,exchange,routing_key) ->
          let _acc = [ "\000<\0002" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if reply_code < 0 || reply_code > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 reply_code;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr reply_text in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr exchange in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr routing_key in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_deliver(consumer_tag,delivery_tag,redelivered,exchange,routing_key) ->
          let _acc = [ "\000<\000<" ] in
          let _acc_len = 4 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr consumer_tag in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 9 in
          Rtypes.write_uint8_unsafe _s 0 delivery_tag;
          let _x = 0 in
          let _x = ((if redelivered then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 8 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 9 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr exchange in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr routing_key in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_get(reserved_1,queue,no_ack) ->
          let _acc = [ "\000<\000F" ] in
          let _acc_len = 4 in
          let _s = String.create 2 in
          if reserved_1 < 0 || reserved_1 > 65535 then raise(Netamqp_types.Encode_error "Value out of range");
          Netamqp_rtypes.write_uint2_unsafe _s 0 reserved_1;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 2 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr queue in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if no_ack then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_get_ok(delivery_tag,redelivered,exchange,routing_key,message_count) ->
          let _acc = [ "\000<\000G" ] in
          let _acc_len = 4 in
          let _s = String.create 9 in
          Rtypes.write_uint8_unsafe _s 0 delivery_tag;
          let _x = 0 in
          let _x = ((if redelivered then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 8 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 9 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr exchange in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr routing_key in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          let _s = String.create 4 in
          Rtypes.write_uint4_unsafe _s 0 message_count;
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 4 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_get_empty(reserved_1) ->
          let _acc = [ "\000<\000H" ] in
          let _acc_len = 4 in
          let (_x,_l) = Netamqp_rtypes.encode_shortstr reserved_1 in
          let _acc = _x @ _acc in
          let _acc_len = _acc_len + _l in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_ack(delivery_tag,multiple) ->
          let _acc = [ "\000<\000P" ] in
          let _acc_len = 4 in
          let _s = String.create 9 in
          Rtypes.write_uint8_unsafe _s 0 delivery_tag;
          let _x = 0 in
          let _x = ((if multiple then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 8 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 9 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_reject(delivery_tag,requeue) ->
          let _acc = [ "\000<\000Z" ] in
          let _acc_len = 4 in
          let _s = String.create 9 in
          Rtypes.write_uint8_unsafe _s 0 delivery_tag;
          let _x = 0 in
          let _x = ((if requeue then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 8 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 9 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_recover_async(requeue) ->
          let _acc = [ "\000<\000d" ] in
          let _acc_len = 4 in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if requeue then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_recover(requeue) ->
          let _acc = [ "\000<\000n" ] in
          let _acc_len = 4 in
          let _s = String.create 1 in
          let _x = 0 in
          let _x = ((if requeue then 1 else 0) lsl 0) lor _x in
          String.unsafe_set _s 0 (Char.chr _x);
          let _acc = _s :: _acc in
          let _acc_len = _acc_len + 1 in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `Basic_recover_ok ->
          "\000<\000o"
     | `Tx_select ->
          "\000Z\000\n"
     | `Tx_select_ok ->
          "\000Z\000\011"
     | `Tx_commit ->
          "\000Z\000\020"
     | `Tx_commit_ok ->
          "\000Z\000\021"
     | `Tx_rollback ->
          "\000Z\000\030"
     | `Tx_rollback_ok ->
          "\000Z\000\031"
  in
  { Netamqp_types.frame_type = `Method;
    frame_channel = _channel;
    frame_payload = [Netamqp_rtypes.mk_mstring _payload];
  }

let decode_header_message _frame =
  let _s = Xdr_mstring.concat_mstrings _frame.Netamqp_types.frame_payload in
  let _l = String.length _s in
  if _l < 14 then raise(Netamqp_types.Decode_error "Message too short");
  let _class_index = Netamqp_rtypes.read_uint2_unsafe _s 0 in
  let _size_rt = Rtypes.read_uint8_unsafe _s 4 in
  let _flags = Netamqp_rtypes.read_uint2_unsafe _s 12 in
  let _c = ref 14 in
  match _class_index with
   | 10 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        (`P_connection,Rtypes.int64_of_uint8 _size_rt)
   | 20 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        (`P_channel,Rtypes.int64_of_uint8 _size_rt)
   | 40 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        (`P_exchange,Rtypes.int64_of_uint8 _size_rt)
   | 50 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        (`P_queue,Rtypes.int64_of_uint8 _size_rt)
   | 60 ->
        let _e_content_type = ((_flags lsr 15) land 1) = 1 in
        let _e_content_encoding = ((_flags lsr 14) land 1) = 1 in
        let _e_headers = ((_flags lsr 13) land 1) = 1 in
        let _e_delivery_mode = ((_flags lsr 12) land 1) = 1 in
        let _e_priority = ((_flags lsr 11) land 1) = 1 in
        let _e_correlation_id = ((_flags lsr 10) land 1) = 1 in
        let _e_reply_to = ((_flags lsr 9) land 1) = 1 in
        let _e_expiration = ((_flags lsr 8) land 1) = 1 in
        let _e_message_id = ((_flags lsr 7) land 1) = 1 in
        let _e_timestamp = ((_flags lsr 6) land 1) = 1 in
        let _e_typ = ((_flags lsr 5) land 1) = 1 in
        let _e_user_id = ((_flags lsr 4) land 1) = 1 in
        let _e_app_id = ((_flags lsr 3) land 1) = 1 in
        let _e_reserved = ((_flags lsr 2) land 1) = 1 in
        let content_type =
          if _e_content_type then (
            Some(Netamqp_rtypes.decode_shortstr _s _c _l)
          ) else None in
        let content_encoding =
          if _e_content_encoding then (
            Some(Netamqp_rtypes.decode_shortstr _s _c _l)
          ) else None in
        let headers =
          if _e_headers then (
            Some(Netamqp_rtypes.decode_table _s _c _l)
          ) else None in
        let _c0 = !_c in
        let delivery_mode =
          if _e_delivery_mode then (
            if _c0 > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
            _c := _c0 + 1;
            Some(Char.code(String.unsafe_get _s _c0))
          ) else None in
        let _c0 = !_c in
        let priority =
          if _e_priority then (
            if _c0 > _l-1 then raise(Netamqp_types.Decode_error "Message too short");
            _c := _c0 + 1;
            Some(Char.code(String.unsafe_get _s _c0))
          ) else None in
        let correlation_id =
          if _e_correlation_id then (
            Some(Netamqp_rtypes.decode_shortstr _s _c _l)
          ) else None in
        let reply_to =
          if _e_reply_to then (
            Some(Netamqp_rtypes.decode_shortstr _s _c _l)
          ) else None in
        let expiration =
          if _e_expiration then (
            Some(Netamqp_rtypes.decode_shortstr _s _c _l)
          ) else None in
        let message_id =
          if _e_message_id then (
            Some(Netamqp_rtypes.decode_shortstr _s _c _l)
          ) else None in
        let _c0 = !_c in
        let timestamp =
          if _e_timestamp then (
            if _c0 > _l-8 then raise(Netamqp_types.Decode_error "Message too short");
            _c := _c0 + 8;
            Some(Int64.to_float(Rtypes.int64_of_uint8(Rtypes.read_uint8_unsafe _s _c0)))
          ) else None in
        let typ =
          if _e_typ then (
            Some(Netamqp_rtypes.decode_shortstr _s _c _l)
          ) else None in
        let user_id =
          if _e_user_id then (
            Some(Netamqp_rtypes.decode_shortstr _s _c _l)
          ) else None in
        let app_id =
          if _e_app_id then (
            Some(Netamqp_rtypes.decode_shortstr _s _c _l)
          ) else None in
        let reserved =
          if _e_reserved then (
            Some(Netamqp_rtypes.decode_shortstr _s _c _l)
          ) else None in
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        (`P_basic(content_type,content_encoding,headers,delivery_mode,priority,correlation_id,reply_to,expiration,message_id,timestamp,typ,user_id,app_id,reserved),Rtypes.int64_of_uint8 _size_rt)
   | 90 ->
        if !_c <> _l then raise(Netamqp_types.Decode_error "Message too long");
        (`P_tx,Rtypes.int64_of_uint8 _size_rt)
    | _ -> raise(Netamqp_types.Decode_error "Unkown class/method")

let encode_header_message _props _size _channel =
  let _payload =
    match _props with
     | `P_connection ->
          let _s = String.make 14 '\000' in
          String.unsafe_set _s 0 '\x00';
          String.unsafe_set _s 1 '\x0a';
          Rtypes.write_uint8_unsafe _s 4 (Rtypes.uint8_of_int64 _size);
          _s
     | `P_channel ->
          let _s = String.make 14 '\000' in
          String.unsafe_set _s 0 '\x00';
          String.unsafe_set _s 1 '\x14';
          Rtypes.write_uint8_unsafe _s 4 (Rtypes.uint8_of_int64 _size);
          _s
     | `P_exchange ->
          let _s = String.make 14 '\000' in
          String.unsafe_set _s 0 '\x00';
          String.unsafe_set _s 1 '\x28';
          Rtypes.write_uint8_unsafe _s 4 (Rtypes.uint8_of_int64 _size);
          _s
     | `P_queue ->
          let _s = String.make 14 '\000' in
          String.unsafe_set _s 0 '\x00';
          String.unsafe_set _s 1 '\x32';
          Rtypes.write_uint8_unsafe _s 4 (Rtypes.uint8_of_int64 _size);
          _s
     | `P_basic(content_type,content_encoding,headers,delivery_mode,priority,correlation_id,reply_to,expiration,message_id,timestamp,typ,user_id,app_id,reserved) ->
          let _s = String.make 14 '\000' in
          String.unsafe_set _s 0 '\x00';
          String.unsafe_set _s 1 '\x3c';
          Rtypes.write_uint8_unsafe _s 4 (Rtypes.uint8_of_int64 _size);
          let _flags =
            (if content_type <> None then 32768 else 0)
            lor (if content_encoding <> None then 16384 else 0)
            lor (if headers <> None then 8192 else 0)
            lor (if delivery_mode <> None then 4096 else 0)
            lor (if priority <> None then 2048 else 0)
            lor (if correlation_id <> None then 1024 else 0)
            lor (if reply_to <> None then 512 else 0)
            lor (if expiration <> None then 256 else 0)
            lor (if message_id <> None then 128 else 0)
            lor (if timestamp <> None then 64 else 0)
            lor (if typ <> None then 32 else 0)
            lor (if user_id <> None then 16 else 0)
            lor (if app_id <> None then 8 else 0)
            lor (if reserved <> None then 4 else 0)
            in
          String.unsafe_set _s 12 (Char.chr((_flags lsr 8) land 0xff));
          String.unsafe_set _s 13 (Char.chr(_flags land 0xff));
          let _acc = [ _s ] in
          let _acc_len = 14 in
          let (_acc, _acc_len) =
            match content_type with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 let (_a,_l) = Netamqp_rtypes.encode_shortstr _x in
                 (_a @ _acc, _acc_len+_l) in
          let (_acc, _acc_len) =
            match content_encoding with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 let (_a,_l) = Netamqp_rtypes.encode_shortstr _x in
                 (_a @ _acc, _acc_len+_l) in
          let (_acc, _acc_len) =
            match headers with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 let (_a,_l) = Netamqp_rtypes.encode_table _x in
                 (_a @ _acc, _acc_len+_l) in
          let (_acc, _acc_len) =
            match delivery_mode with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 if _x < 0 || _x > 255 then raise(Netamqp_types.Encode_error "Value out of range");
                 let _s = String.create 1 in
                 String.unsafe_set _s 0 (Char.unsafe_chr _x);
                 (_s :: _acc, _acc_len+1) in
          let (_acc, _acc_len) =
            match priority with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 if _x < 0 || _x > 255 then raise(Netamqp_types.Encode_error "Value out of range");
                 let _s = String.create 1 in
                 String.unsafe_set _s 0 (Char.unsafe_chr _x);
                 (_s :: _acc, _acc_len+1) in
          let (_acc, _acc_len) =
            match correlation_id with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 let (_a,_l) = Netamqp_rtypes.encode_shortstr _x in
                 (_a @ _acc, _acc_len+_l) in
          let (_acc, _acc_len) =
            match reply_to with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 let (_a,_l) = Netamqp_rtypes.encode_shortstr _x in
                 (_a @ _acc, _acc_len+_l) in
          let (_acc, _acc_len) =
            match expiration with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 let (_a,_l) = Netamqp_rtypes.encode_shortstr _x in
                 (_a @ _acc, _acc_len+_l) in
          let (_acc, _acc_len) =
            match message_id with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 let (_a,_l) = Netamqp_rtypes.encode_shortstr _x in
                 (_a @ _acc, _acc_len+_l) in
          let (_acc, _acc_len) =
            match timestamp with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 let _s = String.create 8 in
                 let _x' = Rtypes.uint8_of_int64(Int64.of_float _x) in
                 Rtypes.write_uint8_unsafe _s 0 _x';
                 (_s :: _acc, _acc_len+8) in
          let (_acc, _acc_len) =
            match typ with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 let (_a,_l) = Netamqp_rtypes.encode_shortstr _x in
                 (_a @ _acc, _acc_len+_l) in
          let (_acc, _acc_len) =
            match user_id with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 let (_a,_l) = Netamqp_rtypes.encode_shortstr _x in
                 (_a @ _acc, _acc_len+_l) in
          let (_acc, _acc_len) =
            match app_id with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 let (_a,_l) = Netamqp_rtypes.encode_shortstr _x in
                 (_a @ _acc, _acc_len+_l) in
          let (_acc, _acc_len) =
            match reserved with
             | None -> (_acc,_acc_len)
             | Some _x ->
                 let (_a,_l) = Netamqp_rtypes.encode_shortstr _x in
                 (_a @ _acc, _acc_len+_l) in
          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len
     | `P_tx ->
          let _s = String.make 14 '\000' in
          String.unsafe_set _s 0 '\x00';
          String.unsafe_set _s 1 '\x5a';
          Rtypes.write_uint8_unsafe _s 4 (Rtypes.uint8_of_int64 _size);
          _s
  in
  { Netamqp_types.frame_type = `Header;
    frame_channel = _channel;
    frame_payload = [Netamqp_rtypes.mk_mstring _payload];
  }

let encode_heartbeat_message() =
  { Netamqp_types.frame_type = `Heartbeat;
    frame_channel = 0;
    frame_payload = [Netamqp_rtypes.mk_mstring "\000\000"];
  }

let encode_body_message data channel =
  { Netamqp_types.frame_type = `Body;
    frame_channel = channel;
    frame_payload = data;
  }

let encode_proto_header_message data =
  { Netamqp_types.frame_type = `Proto_header;
    frame_channel = 0;
    frame_payload = [Netamqp_rtypes.mk_mstring data];
  }

let decode_message frame =
  match frame.Netamqp_types.frame_type with
   | `Method -> `Method(decode_method_message frame)
   | `Header -> `Header(decode_header_message frame)
   | `Body -> `Body frame.Netamqp_types.frame_payload
   | `Heartbeat -> `Heartbeat
   | `Proto_header -> `Proto_header (Xdr_mstring.concat_mstrings frame.Netamqp_types.frame_payload)

let encode_message msg channel =
  match msg with
   | `Method m -> encode_method_message m channel
   | `Header(props,size) -> encode_header_message props size channel
   | `Body data -> encode_body_message data channel
   | `Heartbeat -> encode_heartbeat_message()
   | `Proto_header p -> encode_proto_header_message p

