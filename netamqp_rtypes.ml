(* $Id: netamqp_rtypes.ml 53347 2011-03-01 00:38:28Z gerd $ *)

open Netamqp_types

type uint2 = int

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


let read_uint2_unsafe s p =
  let c1 = String.unsafe_get s p in
  let c0 = String.unsafe_get s (p+1) in
  ((Char.code c1) lsl 8) lor (Char.code c0)


let read_uint2 s p =
  let l = String.length s in
  if p < 0 || p > l-2 then
    invalid_arg "Netamqp_rtypes.read_uint2";
  read_uint2_unsafe s p


let write_uint2_unsafe s p x =
  String.unsafe_set s p (Char.unsafe_chr ((x lsr 8) land 0xff));
  String.unsafe_set s (p+1) (Char.unsafe_chr (x land 0xff))

let write_uint2 s p x =
  let l = String.length s in
  if p < 0 || p > l-2 || x < 0 || x > 65535 then
    invalid_arg "Netamqp_rtypes.write_uint2";
  write_uint2_unsafe s p x

let uint2_as_string x =
  let s = String.create 2 in
  write_uint2 s 0 x;
  s


let decode_shortstr s c l =
  assert(String.length s >= l);
  if !c >= l then raise(Decode_error "Message too short");
  let n = Char.code(String.unsafe_get s !c) in
  if !c >= l - n then raise(Decode_error "Message too short");
  let u = String.sub s (!c+1) n in
  c := !c + n + 1;
  u


let encode_shortstr s =
  let n = String.length s in
  if n > 255 then raise(Encode_error "String too long (shortstr)");
  let p = String.make 1 (Char.unsafe_chr n) in
  ( [s; p], n+1 )

let encode_shortstr_straight s =
  let n = String.length s in
  if n > 255 then raise(Encode_error "String too long (shortstr)");
  let p = String.make 1 (Char.unsafe_chr n) in
  ( [p; s] (* ! *), n+1 )

let encode_shortstr_for_field s =
  let n = String.length s in
  if n > 255 then raise(Encode_error "String too long (shortstr)");
  let p = String.create 2 in
  String.unsafe_set p 0 's';
  String.unsafe_set p 1 (Char.unsafe_chr n);
  ( [p; s], n+2 )


let decode_longstr_nocopy s c l =
  assert(String.length s >= l);
  if !c >= l - 3 then raise(Decode_error "Message too short");
  let n_rt = Rtypes.read_uint4_unsafe s !c in
  let n =
    try Rtypes.int_of_uint4 n_rt
    with Rtypes.Cannot_represent _ ->
      raise(Decode_error "Cannot represent field because it is too long") in
  if !c >= l - n - 3 then raise(Decode_error "Message too short");
  let p = !c+4 in
  c := !c + n + 4;
  (p, n)


let decode_longstr s c l =
  let (p,n) = decode_longstr_nocopy s c l in
  String.sub s p n


let encode_longstr s =
  let n = String.length s in
  let n_rt =
    try Rtypes.uint4_of_int n
    with _ -> raise(Encode_error "String too long (longstr)") in
  let p = Rtypes.uint4_as_string n_rt in
  ( [s; p], n+4 )


let encode_longstr_for_field s =
  let n = String.length s in
  let n_rt =
    try Rtypes.uint4_of_int n
    with _ -> raise(Encode_error "String too long (longstr)") in
  let p = String.create 5 in
  String.unsafe_set p 0 'S';
  Rtypes.write_uint4_unsafe p 1 n_rt;
  ( [p; s], n+5 )


let rec parse_table s c l =
  let rec next_field() =
    if !c < l then (
      let name = decode_shortstr s c l in
      let v = decode_field s c l in
      (name, v) :: next_field()
    )
    else
      [] 
  in
  next_field()


and decode_field s c l : table_field =
  let expect n =
    if !c >= l - n + 1 then
      raise(Decode_error "Message too short") in

  expect 1;
  let t = s.[ !c ] in
  incr c;
  match t with
    | 't' ->
	expect 1;
	let v = `Bool(s.[!c] <> '\000') in
	incr c;
	v
    | 'b' ->
	expect 1;
	let x = Char.code s.[!c] in
	let v = if x >= 128 then `Sint1(x - 256) else `Sint1 x in
	incr c;
	v
(*
    | 'B' ->
	expect 1;
	let x = Char.code s.[!c] in
	let v = `Uint1 x in
	incr c;
	v
    | 'U' ->
	expect 2;
	let x = read_uint2_unsafe s !c in
	let v = if x >= 32768 then `Sint2(x - 65536) else `Sint2 x in
	c := !c + 2;
	v
    | 'u' ->
	expect 2;
	let x = read_uint2_unsafe s !c in
	let v = `Uint2 x in
	c := !c + 2;
	v
 *)
    | 'I' ->
	expect 4;
	let x = Rtypes.read_int4_unsafe s !c in
	let v = `Sint4 x in
	c := !c + 4;
	v
(*
    | 'i' ->
	expect 4;
	let x = Rtypes.read_uint4_unsafe s !c in
	let v = `Uint4 x in
	c := !c + 4;
	v
    | 'L' ->
	expect 8;
	let x = Rtypes.read_int8_unsafe s !c in
	let v = `Sint8 x in
	c := !c + 8;
	v
    | 'l' ->
	expect 8;
	let x = Rtypes.read_uint8_unsafe s !c in
	let v = `Uint8 x in
	c := !c + 8;
	v
 *)
    | 'f' ->
	expect 4;
	let x = Rtypes.float_of_fp4(Rtypes.read_fp4 s !c) in
	let v = `Float x in
	c := !c + 4;
	v
    | 'd' ->
	expect 8;
	let x = Rtypes.float_of_fp8(Rtypes.read_fp8 s !c) in
	let v = `Double x in
	c := !c + 8;
	v
    | 'D' ->
	expect 5;
	let scale = Char.code s.[!c] in
	let x = Rtypes.read_uint4_unsafe s (!c+1) in
	let v = `Decimal(scale,x) in
	c := !c + 5;
	v
(*
    | 's' ->
	let x = decode_shortstr s c l in
	let v = `Shortstr x in
	v
 *)
    | 'S' -> 
	let x = decode_longstr s c l in
	let v = `Longstr x in
	v
(*
    | 'A' ->
	let x = decode_array s c l in
	let v = `Array x in
	v
 *)
    | 'T' ->
	expect 8;
	let x = Rtypes.read_uint8_unsafe s !c in
	let t =
	  try Int64.to_float(Rtypes.int64_of_uint8 x)
	  with _ ->
	    raise(Decode_error "Timestamp out of supported range") in
	let v = `Timestamp t in
	c := !c + 8;
	v
    | 'F' ->
	let x = decode_table s c l in
	let v = `Table x in
	v
    | 'V' ->
	`Null
    | _ ->
	raise(Decode_error "Bad field type in table")
	  
and decode_array s c l =
  let (p,n) = decode_longstr_nocopy s c l in
  let c' = ref p in
  let acc = ref [] in
  while !c' < !c do
    let v = decode_field s c' !c in
    acc := v :: !acc
  done;
  List.rev !acc

and decode_table s c l =
  let (p,n) = decode_longstr_nocopy s c l in
  let c' = ref p in
  let t = parse_table s c' !c in
  if !c <> !c' then
    raise(Decode_error "Table does not fit into field");
  t


let rec encode_field field =
  (* Note: the list is built in the right order! *)
  match field with
    | `Bool b ->
	(if b then ["t\001"] else ["t\000"]), 2
    | `Sint1 x ->
	if x < (-128) || x > 127 then
	  raise(Encode_error "Value out of range (Sint1)");
	let s = String.create 2 in
	String.unsafe_set s 0 'b';
	String.unsafe_set s 1
	  (Char.unsafe_chr (if x < 0 then x+256 else x));
	([s], 2)
(*
    | `Uint1 x ->
	if x < 0 || x > 255 then
	  raise(Encode_error "Value out of range (Uint1)");
	let s = String.create 2 in
	String.unsafe_set s 0 'B';
	String.unsafe_set s 1 (Char.unsafe_chr x);
	([s], 2)
    | `Sint2 x ->
	if x < (-32768) || x > 32767 then
	  raise(Encode_error "Value out of range (Sint2)");
	let s = String.create 3 in
	String.unsafe_set s 0 'U';
	write_uint2_unsafe s 1 (if x < 0 then x + 65536 else x);
	([s], 3)
    | `Uint2 x ->
	if x < 0 || x > 65535 then
	  raise(Encode_error "Value out of range (Uint2)");
	let s = String.create 3 in
	String.unsafe_set s 0 'u';
	write_uint2_unsafe s 1 x;
	([s], 3)
 *)
     | `Sint4 x ->
	let s = String.create 5 in
	String.unsafe_set s 0 'I';
	Rtypes.write_int4_unsafe s 1 x;
	([s], 5)
(*
    | `Uint4 x ->
	let s = String.create 5 in
	String.unsafe_set s 0 'i';
	Rtypes.write_uint4_unsafe s 1 x;
	([s], 5)
    | `Sint8 x ->
	let s = String.create 9 in
	String.unsafe_set s 0 'L';
	Rtypes.write_int8_unsafe s 1 x;
	([s], 9)
    | `Uint8 x ->
	let s = String.create 9 in
	String.unsafe_set s 0 'l';
	Rtypes.write_uint8_unsafe s 1 x;
	([s], 9)
 *)
    | `Float x ->
	let s = "f" ^ Rtypes.fp4_as_string (Rtypes.fp4_of_float x) in
	([s], 5)
    | `Double x ->
	let s = "d" ^ Rtypes.fp8_as_string (Rtypes.fp8_of_float x) in
	([s], 9)
    | `Decimal(scale, x) ->
	if scale < 0 || scale > 255 then
	  raise(Encode_error "Value out of range (Decimal)");
	let s = String.create 6 in
	String.unsafe_set s 0 'D';
	String.unsafe_set s 1 (Char.unsafe_chr scale);
	Rtypes.write_uint4_unsafe s 2 x;
	([s], 6)
(*
    | `Shortstr x ->
	encode_shortstr_for_field x
 *)
    | `Longstr x ->
	encode_longstr_for_field x
(*
    | `Array x ->
	let len = ref 0 in
	let x' = 
	  List.flatten
	    (List.map 
	       (fun xe ->
		  let (l,n) = encode_field xe in
		  len := !len + n;
		  l
	       )
	       x
	    ) in
	let s = String.create 5 in
	String.unsafe_set s 0 'A';
	Rtypes.write_uint4_unsafe s 1 (Rtypes.uint4_of_int !len);
	( s :: x', !len + 5 )
 *)
    | `Timestamp x ->
	let s = String.create 9 in
	String.unsafe_set s 0 'T';
	Rtypes.write_uint8_unsafe s 1
	  ( try
	      (Rtypes.uint8_of_int64 (Int64.of_float x))
	    with
	      | _ -> raise(Encode_error "Cannot represent timestamp")
	  );
	([s], 9)
    | `Table x ->
	let (l, n) = encode_table_straight x in
	("F" :: l, n+1)
    | `Null ->
	(["V"], 1)

and encode_table_straight x =
  let n = ref 0 in
  let l =
    List.flatten
      (List.map
	 (fun (name, xe) ->
	    let (l1, n1) = encode_shortstr_straight name in
	    let (l2, n2) = encode_field xe in
	    n := !n + n1 + n2;
	    l1 @ l2
	 )
	 x 
      ) in
  let p = Rtypes.uint4_as_string (Rtypes.uint4_of_int !n) in
  (p :: l, !n + 4)


let encode_table x =
  let (l,n) = encode_table_straight x in
  (List.rev l, n)


let mk_mstring s =
  Xdr_mstring.string_based_mstrings # create_from_string
    s 0 (String.length s) false


let unsafe_rev_concat l n =
  let s = String.create n in
  let k = ref n in
  List.iter
    (fun x ->
       let p = String.length x in
       k := !k - p;
       String.unsafe_blit x 0 s !k p
    )
    l;
  assert(!k = 0);
  s
