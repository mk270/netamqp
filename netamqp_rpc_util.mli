(* $Id: netamqp_rpc_util.mli, section copied from rpc_util.mli of ocaml 3.6.5: 2010-04-27 15:51:50Z gerd $ *)

(** Utility functions *)
val hex_dump_m :
      Netsys_mem.memory -> int -> int -> string
val hex_dump_s :
      string -> int -> int -> string
   (** Format the contents as hex sequence *)
