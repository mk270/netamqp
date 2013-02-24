#use "topfind";;
#require "netamqp";;

open Netamqp_types
open Printf

let () =
  Netamqp_endpoint.Debug.enable := true;
  Netamqp_transport.Debug.enable := true


let esys = Unixqueue.create_unix_event_system()
let p = `TCP(`Inet("localhost", Netamqp_endpoint.default_port))
let ep = Netamqp_endpoint.create p (`AMQP_0_9 `One) esys
let c = Netamqp_connection.create ep
let auth = Netamqp_connection.plain_auth "guest" "guest"

let test1() =
  Netamqp_connection.open_s c [ auth ] (`Pref "en") "/";
  eprintf "*** Connection could be opened, and the proto handshake is done!\n%!";
  Netamqp_connection.close_s c;
  eprintf "*** Connection could be closed!\n%!"

let test2() =
  Netamqp_connection.open_s c [ auth ] (`Pref "en") "/";
  eprintf "*** Connection could be opened, and the proto handshake is done!\n%!";
  let co = Netamqp_channel.open_s c 1 in
  eprintf "*** Channel could be opened!\n%!";
  co

let test3 co =
  Netamqp_channel.close_s co;
  eprintf "*** Channel could be closed!\n%!";
  Netamqp_connection.close_s c;
  eprintf "*** Connection could be closed!\n%!"
