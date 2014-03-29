(* $Id: netamqp_rpc_util.ml, section copied from rpc_util.ml of ocamlnet 3.6.5:`:w: 1701 2012-02-14 14:48:46Z gerd $ *)

open Printf

let hex_dump_m m pos len =
  let b = Buffer.create 100 in
  for k = 0 to len - 1 do
    let c = Bigarray.Array1.get m (pos+k) in
    bprintf b "%02x " (Char.code c)
  done;
  Buffer.contents b


let hex_dump_s s pos len =
  let b = Buffer.create 100 in
  for k = 0 to len - 1 do
    let c = s.[pos+k] in
    bprintf b "%02x " (Char.code c)
  done;
  Buffer.contents b
