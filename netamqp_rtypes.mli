(* $Id: netamqp_rtypes.mli 53347 2011-03-01 00:38:28Z gerd $ *)

(** Support for remote types not already covered by [Rtypes] *)

(** {2 Integers} *)

(** Short *)

type uint2 = int

val read_uint2 : string -> int -> uint2
val read_uint2_unsafe : string -> int -> uint2
val write_uint2 : string -> int -> uint2 -> unit
val write_uint2_unsafe : string -> int -> uint2 -> unit
val uint2_as_string : uint2 -> string


(** {2 Strings} *)

val decode_shortstr : string -> int ref -> int -> string
val decode_longstr : string -> int ref -> int -> string

val encode_shortstr : string -> (string list * int)
val encode_longstr : string -> (string list * int)
  (* result (list, n): [list] is the list of part strings in reverse order,
     [n] is the length of the [list] contents in bytes
   *)


(** {2 Tables} *)

(** Convention:
    - [Sint<n>]: signed integer with n bytes
    - [Uint<n>]: unsigned integer with n bytes

    Note that there is confusion about what is allowed in tables, see
    http://dev.rabbitmq.com/wiki/Amqp091Errata. For best compatibility
    one should only use [`Sint4], [`Decimal], [`Longstr], [`Timestamp],
    [`Table], and [`Null].

 *)

type ('table_field,'table) table_field_standard =
    [ `Sint4 of Rtypes.int4
    | `Decimal of int * Rtypes.uint4
    | `Longstr of string  (* up to 4G chars *)
    | `Timestamp of float  (* only int precision *)
    | `Table of 'table
    | `Null
    ]

type ('table_field,'table) table_field_ok =
    [ `Bool of bool
    | `Sint1 of int
    | `Float of float   (* single precision only *)
    | `Double of float
    ]

type ('table_field,'table) table_field_problematic =
    [ `Uint1 of int
    | `Sint2 of int
    | `Uint2 of int
    | `Uint4 of Rtypes.uint4
    | `Sint8 of Rtypes.int8
    | `Uint8 of Rtypes.uint8
    | `Shortstr of string (* up to 255 chars *)
    | `Array of 'table_field list
    ]

type table_field =
    [ (table_field,table) table_field_standard
    | (table_field,table) table_field_ok
    (* | table_field_problematic *)
    ]

and table =
    (string * table_field) list


val decode_table : string -> int ref -> int -> table
val encode_table : table -> (string list * int)

(** {2 Misc} *)

val unsafe_rev_concat : string list -> int -> string

val mk_mstring : string -> Xdr_mstring.mstring
