(* $Id: amqp_gen.ml 53397 2011-03-09 21:09:13Z gerd $ *)

(* Generate Netamqp_methods from amqp-0-9-1.xml *)

open Printf

let in_file = "amqp0-9-1.xml"

let out_mod = "netamqp_methods_0_9"

type prim_type =
    [ `Octet | `Short | `Long | `Longlong | `Bit | `Shortstr | `Longstr 
    | `Timestamp | `Field_table
    ]

type check =
    [ `Length of int
    | `Notnull
    | `Regexp of string
    ]

type domain =
    { dom_name : string;
      dom_type : prim_type;
      dom_checks : check list;
    }

type field =
    { field_name : string;
      field_dom : domain;
      field_advance : int;
        (* For fixed-length prim_types: How many bytes to advance the cursor
	   after reading the field
	 *)
      field_bit : int;
        (* For `Bit: which bit number *)
      field_checks : check list;
        (* additional checks *)
    }

type meth =
    { meth_name : string;
      meth_sync : bool;        (* whether method is synchronous *)
      meth_content : bool;     (* whether method is followed by content *)
      meth_c2s : bool;         (* client-to-server *)
      meth_s2c : bool;         (* server-to-client *)
      meth_index : int;
      meth_response : string list;
      meth_fields : field list;
    }

type clas =
    { class_name : string;
      class_index : int;
      class_props : field list;
      class_meths : meth list;
    }

type spec =
    { spec_doms : (string,domain) Hashtbl.t;
      spec_classes : clas list
    }


let advance =
  function
    | `Octet -> 1
    | `Short -> 2
    | `Long -> 4
    | `Longlong -> 8
    | `Bit -> 0   (* to be fixed up *)
    | `Shortstr -> (-1)
    | `Longstr  -> (-1)
    | `Timestamp -> 8
    | `Field_table -> (-1)

(* (-1): used for fields with dynamic length *)



let parse_type =
  function
    | "octet" -> `Octet
    | "short" -> `Short
    | "long" -> `Long
    | "longlong" -> `Longlong
    | "bit" -> `Bit 
    | "shortstr" -> `Shortstr
    | "longstr" -> `Longstr 
    | "timestamp" -> `Timestamp
    | "table" -> `Field_table
    | s -> failwith ("Unknown primitive type: " ^ s)


let parse_assert node =
  let check = node # required_string_attribute "check" in
  match check with
    | "length" ->
	let v = node # required_string_attribute "value" in
	( try
	    let l = int_of_string v in
	    [ `Length l ]
	  with
	    | e -> 
		print_endline ("Warning: cannot parse length-check value: " 
				  ^ v);
		[]
	)
    | "notnull" -> 
	[ `Notnull ]
    | "regexp" ->
	let v = node # required_string_attribute "value" in
	( try
	    let _ = Pcre.regexp v in
	    [ `Regexp v ]
	  with
	    | e -> 
		print_endline ("Warning: cannot parse regexp-check value: " 
				  ^ v);
		[]
	)
    | _ ->
	print_endline ("Warning: unknown check found: " ^ check);
	[]


let parse_spec ~verbose () =
  let config = Pxp_types.default_config in
  let spec = Pxp_tree_parser.default_spec in
  let source = Pxp_types.from_file in_file in
  let doc = Pxp_tree_parser.parse_wfdocument_entity config source spec in
  
  let domain_nodes =
    Pxp_document.find_all_elements "domain" doc#root in
  let doms = 
    Hashtbl.create 27 in
  List.iter
    (fun node ->
       let name = node # required_string_attribute "name" in
       if verbose then print_endline("Parsing domain " ^ name);
       let tstr = node # required_string_attribute "type" in
       let typ = parse_type tstr in
       let asserts = Pxp_document.find_all_elements "assert" node in
       let checks =
	 List.flatten
	   (List.map parse_assert asserts) in

       let dom =
	 { dom_name = name;
	   dom_type = typ;
	   dom_checks = checks;
	 } in
       Hashtbl.add doms name dom
    )
    domain_nodes;

  let parse_fields root name_prefix =
    let field_nodes = Pxp_document.find_all_elements "field" root in
    let fields1 =
      List.map
	(fun fnode ->
	   let fname = fnode # required_string_attribute "name" in
	   let full_fname = name_prefix ^ "." ^ fname in
	   if verbose then print_endline ("Parsing field " ^ full_fname);
	   let dom_name = 
	     try fnode # required_string_attribute "domain" 
	     with _ ->
	       fnode # required_string_attribute "type" in
	   let dom =
	     try Hashtbl.find doms dom_name
	     with Not_found ->
	       failwith ("Domain " ^ dom_name ^ " not found in: " ^ 
			   full_fname) in
	   let asserts = Pxp_document.find_all_elements "assert" fnode in
	   let checks =
	     List.flatten
	       (List.map parse_assert asserts) in
	   
	   let adv = advance dom.dom_type in
	   { field_name = fname;
	     field_dom = dom;
	     field_advance = adv;
	     field_bit = 0;
	     field_checks = checks
	   }
	)
	field_nodes in
    (* We still have to fix up fields of type `Bit: *)
    let (last, fields2) =
      List.fold_left
	(fun (last,acc) field ->
	   match field.field_dom.dom_type with
	     | `Bit ->
		 let bitnr, fixup_adv =
		   match last with
		     | None -> 0, false
		     | Some n ->
			 if n=7 then 0, true else (n+1), false in
		 let field' = { field with field_bit = bitnr } in
		 let acc' =
		   if fixup_adv then
		     match acc with
		       | last_node :: rest ->
			   { last_node with field_advance = 1 } :: rest
		       | [] ->
			   assert false
		   else
		     acc in
		 (Some bitnr, field'::acc')
	     | _ ->
		 if last = None then
		   (None, field::acc)
		 else (
		   match acc with
		     | last_node :: rest ->
			 (None,
			  field::{last_node with field_advance = 1 }::rest)
		     | _ ->
			 assert false 
		 )
	)
	(None, [])
	fields1 in
    let fields3 =
      if last=None then fields2 else (
	match fields2 with
	  | last_node :: rest ->
	      {last_node with field_advance = 1 }::rest
	  | _ ->
	      assert false 
      ) in
    List.rev fields3
  in

  let class_nodes =
    Pxp_document.find_all_elements "class" doc#root in
  let classes =
    List.map
      (fun cnode ->
	 let name = cnode # required_string_attribute "name" in
	 if verbose then print_endline("Parsing class " ^ name);
	 let index_s = cnode # required_string_attribute "index" in
	 let index =
	   try int_of_string index_s
	   with _ ->
	     failwith("Cannot parse index attr of class: " ^ name) in
	 let props = parse_fields cnode name in
	 let method_nodes =
	   Pxp_document.find_all_elements "method" cnode in
	 let meths =
	   List.map
	     (fun mnode ->
		let mname = mnode # required_string_attribute "name" in
		let full_mname = name ^ "#" ^ mname in
		if verbose then print_endline("Parsing method " ^ full_mname);
		let mindex_s = mnode # required_string_attribute "index" in
		let mindex =
		  try int_of_string mindex_s
		  with _ ->
		    failwith("Cannot parse index attr of method: " ^ 
			      full_mname ) in
		let content =
		  match mnode # optional_string_attribute "content" with
		    | None -> false
		    | Some "1" -> true
		    | Some "0" -> false
		    | Some s ->
			failwith("Cannot parse content attr of method: " ^ 
				   full_mname) in
		let sync =
		  match mnode # optional_string_attribute "synchronous" with
		    | None -> false
		    | Some "1" -> true
		    | Some "0" -> false
		    | Some s ->
			failwith("Cannot parse synchronous attr of method: " ^ 
				   full_mname) in
		let response_nodes =
		  Pxp_document.find_all_elements "response" mnode in
		let resp =
		  List.map
		    (fun rnode ->
		       rnode # required_string_attribute "name"
		    )
		    response_nodes in
		let chassis_nodes =
		  Pxp_document.find_all_elements "chassis" mnode in
		let c2s =
		  List.exists
		    (fun n -> n#required_string_attribute "name" = "server")
		    chassis_nodes in
		let s2c =
		  List.exists
		    (fun n -> n#required_string_attribute "name" = "client")
		    chassis_nodes in
		let fields =
		  parse_fields mnode full_mname in
		{ meth_name = mname;
		  meth_sync = sync;
		  meth_content = content;
		  meth_c2s = c2s;
		  meth_s2c = s2c;
		  meth_index = mindex;
		  meth_response = resp;
		  meth_fields = fields;
		}
	     )
	     method_nodes in
	 { class_name = name;
	   class_index = index;
	   class_props = props;
	   class_meths = meths
	 }
      )
      class_nodes in

  let spec =
    { spec_doms = doms;
      spec_classes = classes
    } in
  spec
  

let gen_warning =
  "(* This file is generated! Do not edit! *)\n\n"

let transl_name s =
  match s with
    | "type" -> "typ"
    | _ ->
	Pcre.qreplace ~pat:"-" ~templ:"_" s

let transl_meth_name cls meth =
  sprintf "%s_%s"
    (String.capitalize
       (transl_name cls.class_name))
    (transl_name meth.meth_name)

let transl_meth_name_1 cls meth_name =
  sprintf "%s_%s"
    (String.capitalize
       (transl_name cls.class_name))
    (transl_name meth_name)

let prim_repr =
  [ `Octet, "int  (* 0..255 *)";
    `Short, "int  (* 0..65535 *)";
    `Long,  "Rtypes.uint4";
    `Longlong, "Rtypes.uint8";
    `Bit, "bool";
    `Shortstr, "string  (* up to 255 chars *)";
    `Longstr, "string  (* up to 4G chars *)";
    `Timestamp, "float";
    `Field_table, "Netamqp_rtypes.table";
  ]

let check_comment ch =
  match ch with
    | `Length n -> sprintf "length <= %d" n
    | `Notnull -> sprintf "not null"
    | `Regexp re -> sprintf "matches regexp %s" re


let output_domain_type_definitions spec f =
  Hashtbl.iter
    (fun name dom ->
       fprintf f "type %s = %s\n"
	 (transl_name name)
	 ( try 
	     List.assoc dom.dom_type prim_repr
	   with Not_found -> assert false
	 );
       List.iter
	 (fun ch ->
	    fprintf f "  (* where: %s *)\n" (check_comment ch)
	 )
	 dom.dom_checks;
       fprintf f "\n";
    )
    spec.spec_doms


let output_method_type_definition spec f mli =
  (* Arguments: *)
  List.iter
    (fun cls ->
       List.iter
	 (fun meth ->
	    if meth.meth_fields <> [] then (
	      let name = transl_meth_name cls meth in
	      fprintf f "type arg_%s =\n" 
		name;
	      let first = ref true in
	      List.iter
		(fun field ->
		   fprintf f "     %c %-15s (* %s *)\n"
		     (if !first then '(' else '*')
		     (transl_name field.field_dom.dom_name)
		     (field.field_name ^
			(if field.field_checks <> [] then
			   ", where " ^ 
			     (String.concat " and "
				(List.map check_comment field.field_checks))
			 else
			   ""
			)
		     );
		   first := false
		)
		meth.meth_fields;
	      fprintf f "     )\n";
	      fprintf f "\n";
	    )
	 )
	 cls.class_meths
    )
    spec.spec_classes;

  (* Methods: *)
  let subsets =
    [ "method_t",
        (fun m -> true);
      "sync_client_to_server_method_t", 
        (fun m -> m.meth_sync && m.meth_c2s);
      "sync_server_to_client_method_t", 
        (fun m -> m.meth_sync && m.meth_s2c);
      "sync_client_initiated_method_t", 
        (fun m -> m.meth_sync && m.meth_c2s && m.meth_response <> []);
      "sync_server_initiated_method_t", 
        (fun m -> m.meth_sync && m.meth_s2c && m.meth_response <> []);
      "async_client_to_server_method_t", 
        (fun m -> not m.meth_sync && m.meth_c2s);
      "async_server_to_client_method_t",
        (fun m -> not m.meth_sync && m.meth_s2c);
      "content_method_t",
        (fun m -> m.meth_content)
    ] in
  List.iter
    (fun (tname, cond) ->
       fprintf f "type %s = [\n" tname;
       List.iter
	 (fun cls ->
	    List.iter
	      (fun meth ->
		 if cond meth then (
		   let name = transl_meth_name cls meth in
		   fprintf f " | `%s%s\n" 
		     name
		     (if meth.meth_fields <> [] then
			sprintf " of arg_%s" name
		      else 
			""
		     )
		 )
	      )
	      cls.class_meths
	 )
	 spec.spec_classes;
       fprintf f " ]\n\n";
    )
    subsets;

  (* Method types *)
  let type_subsets =
    [ "method_type_t",
        (fun m -> true);
      "sync_client_to_server_method_type_t", 
        (fun m -> m.meth_sync && m.meth_c2s);
      "sync_server_to_client_method_type_t", 
        (fun m -> m.meth_sync && m.meth_s2c);
      "sync_client_initiated_method_type_t", 
        (fun m -> m.meth_sync && m.meth_c2s && m.meth_response <> []);
      "sync_server_initiated_method_type_t", 
        (fun m -> m.meth_sync && m.meth_s2c && m.meth_response <> []);
      "async_client_to_server_method_type_t", 
        (fun m -> not m.meth_sync && m.meth_c2s);
      "async_server_to_client_method_type_t",
        (fun m -> not m.meth_sync && m.meth_s2c);
      "content_method_type_t",
        (fun m -> m.meth_content)
    ] in
  List.iter
    (fun (tname,cond) ->
       fprintf f "type %s = [\n" tname;
       List.iter
	 (fun cls ->
	    List.iter
	      (fun meth ->
		 if cond meth then (
		   let name = transl_meth_name cls meth in
		   fprintf f " | `%s\n" name;
		 )
	      )
	      cls.class_meths
	 )
	 spec.spec_classes;
       fprintf f " ]\n\n";
    )
    type_subsets;

  (* Get type of method: *)
  if mli then
    fprintf f "val type_of_method : method_t -> method_type_t\n\n"
  else (
    fprintf f "let type_of_method m =\n";
    fprintf f "  match m with\n";
    List.iter
      (fun cls ->
	 List.iter
	   (fun meth ->
	      let name = transl_meth_name cls meth in
	      if meth.meth_fields = [] then
		fprintf f "   | `%s -> `%s\n" name name
	      else
		fprintf f "   | `%s _ -> `%s\n" name name
	   )
	   cls.class_meths
      )
      spec.spec_classes;
    fprintf f "\n"
  );

  (* Get possible responses: *)
  if mli then
    fprintf f "val responses_of_method : method_type_t -> method_type_t list\n\n"
  else (
    fprintf f "let responses_of_method m =\n";
    fprintf f "  match m with\n";
    List.iter
      (fun cls ->
	 List.iter
	   (fun meth ->
	      let name = transl_meth_name cls meth in
	      let responses =
		String.concat "; "
		  (List.map
		     (fun n -> "`" ^ transl_meth_name_1 cls n) 
		     meth.meth_response) in
	      fprintf f "   | `%s -> [ %s ]\n" name responses
	   )
	   cls.class_meths
      )
      spec.spec_classes;
    fprintf f "\n"
  );

  (* Content methods: *)
  if mli then
    fprintf f "val content_method_types : method_type_t list\n\n"
  else (
    fprintf f "let content_method_types =\n";
    fprintf f " [ ";
    let first = ref true in
    List.iter
      (fun cls ->
	 List.iter
	   (fun meth ->
	      let name = transl_meth_name cls meth in
	      if meth.meth_content then (
		if not !first then fprintf f "; ";
		fprintf f "`%s" name;
		first := false
	      )
	   )
	   cls.class_meths
      )
      spec.spec_classes;
    fprintf f " ]\n\n";
  )


let output_string_helpers spec f mli =
  if mli then (
    fprintf f
      "val string_of_method_type : method_type_t -> string\n\n";
    fprintf f
      "val string_of_method_id : class_id:int -> meth_id:int -> string\n\n";
  )
  else (
    fprintf f "let string_of_method_type =\n";
    fprintf f "  function\n";
    List.iter
      (fun cls ->
	 List.iter
	   (fun meth ->
	      let name = transl_meth_name cls meth in
	      fprintf f "   | `%s -> \"%s.%s\"\n" 
		name cls.class_name meth.meth_name;
	   )
	   cls.class_meths
      )
      spec.spec_classes;
    fprintf f "let string_of_method_id ~class_id ~meth_id =\n";
    fprintf f "  match (class_id,meth_id) with\n";
    List.iter
      (fun cls ->
	 List.iter
	   (fun meth ->
	      fprintf f "   | (%d, %d) -> \"%s.%s\"\n" 
		cls.class_index meth.meth_index
		cls.class_name meth.meth_name;
	   )
	   cls.class_meths
      )
      spec.spec_classes;
    fprintf f "   | _ -> raise Not_found\n\n"
  )


let output_props_type_definition spec f =
  fprintf f "type props_t = [\n";
  List.iter
    (fun cls ->
       fprintf f " | `P_%s%s\n" 
	 cls.class_name
	 (if cls.class_props <> [] then " of" else "\n");
       let first = ref true in
       List.iter
	 (fun field ->
	    let t =
	      if field.field_dom.dom_type = `Bit then
		transl_name field.field_dom.dom_name
	      else
		transl_name field.field_dom.dom_name ^ " option" in
	    fprintf f "     %c %-15s (* %s *)\n"
	      (if !first then '(' else '*')
	      t
	      (field.field_name ^
		 (if field.field_checks <> [] then
		    ", where " ^ 
		      (String.concat " and "
			 (List.map check_comment field.field_checks))
		  else
		    ""
		 )
	      );
	    first := false
	 )
	 cls.class_props;
       if cls.class_props <> [] then
	 fprintf f "     )\n"
    )
    spec.spec_classes;
  fprintf f " ]\n\n"


let output_message_type_definition spec f =
  fprintf f "type message_t = [\n";
  fprintf f "  | `Method of method_t\n";
  fprintf f "  | `Header of props_t * int64 (* size *)\n";
  fprintf f "  | `Body of Xdr_mstring.mstring list\n";
  fprintf f "  | `Heartbeat\n";
  fprintf f "  | `Proto_header of string\n";
  fprintf f "]\n\n"


let rec get_cumulated_length fields =
  match fields with
    | field :: fields' ->
	let adv = field.field_advance in
	if adv >= 0 then
	  adv + get_cumulated_length fields'
	else
	  0
    | [] ->
	0


let exn_too_short =
  "raise(Netamqp_types.Decode_error \"Message too short\")" 

let exn_too_long =
  "raise(Netamqp_types.Decode_error \"Message too long\")"


let output_field_decoder fields f indent =
  (* _s: the string to analyze
     _c: the cursor (int ref)
     _l: the end position of _s (i.e. indexes < _l are allowed)

     This function creates code like:

     let <fieldname1> = ... in
     let <fieldname2> = ... in
     ...
     let <fieldnameN> = ... in

     so that the following expression can access all fieldnames.

   *)
  let istr = String.make indent ' ' in

  let rec emit offset checked same_byte fields =
    match fields with
      | field :: fields' ->
	  let t = field.field_dom.dom_type in
	  let adv = field.field_advance in
	  if adv >= 0 then (
	    (* Field with fixed length *)
	    if not checked then (
	      let n = get_cumulated_length fields in
	      fprintf f
		"%sif !_c > _l-%d then %s;\n" 
		istr n exn_too_short;
	    );
	    if t = `Bit && not same_byte then
	      fprintf f
		"%slet _x = Char.code(String.unsafe_get _s (!_c+%d)) in\n"
		istr offset;
	    fprintf f "%slet %s = " istr (transl_name field.field_name);
	    ( match t with
		| `Octet ->
		    fprintf f
		      "Char.code(String.unsafe_get _s (!_c+%d)) in\n"
		      offset;
		    emit (offset+1) true false fields'
		| `Short ->
		    fprintf f
		      "Netamqp_rtypes.read_uint2_unsafe _s (!_c+%d) in\n"
		      offset;
		    emit (offset+2) true false fields'
		| `Long ->
		    fprintf f
		      "Rtypes.read_uint4_unsafe _s (!_c+%d) in\n"
		      offset;
		    emit (offset+4) true false fields'
		| `Longlong ->
		    fprintf f
		      "Rtypes.read_uint8_unsafe _s (!_c+%d) in\n"
		      offset;
		    emit (offset+8) true false fields'
		| `Bit ->
		    fprintf f
		      "(_x lsr %d) land 1 <> 0 in\n"
		      field.field_bit;
		    emit (offset+adv) true (adv=0) fields'
		| `Timestamp ->
		    fprintf f
		      "Int64.to_float(Rtypes.int64_of_uint8\
                         (Rtypes.read_uint8_unsafe _s (!_c+%d))) in\n"
		      offset;
		    emit (offset+8) true false fields'
		| _ ->
		    assert false
	    )
	  )
	  else (
	    (* Field with variable length *)
	    if offset > 0 then
	      fprintf f "%s_c := !_c + %d;\n" istr offset;
	    fprintf f "%slet %s = " istr (transl_name field.field_name);
	    ( match t with
		| `Shortstr ->
		    fprintf f "Netamqp_rtypes.decode_shortstr _s _c _l in\n"
		| `Longstr ->
		    fprintf f "Netamqp_rtypes.decode_longstr _s _c _l in\n"
		| `Field_table ->
		    fprintf f "Netamqp_rtypes.decode_table _s _c _l in\n"
		| _ ->
		    assert false
	    );
	    emit 0 false false fields'
	  )

      | [] ->
	  if offset > 0 then
	    fprintf f "%s_c := !_c + %d;\n" istr offset;
	  fprintf f 
	    "%sif !_c <> _l then %s;\n" istr exn_too_long
  in
  emit 0 false false fields


let output_method_decoder spec f =
  (* _frame: input frame
   *)
  fprintf f
    "let decode_method_message _frame =\n";
  fprintf f
    "  let _s = Xdr_mstring.concat_mstrings \
          _frame.Netamqp_types.frame_payload in\n";
  fprintf f
    "  let _l = String.length _s in\n";
  fprintf f
    "  if _l < 4 then %s;\n" exn_too_short;
  fprintf f
    "  let _class_index = Netamqp_rtypes.read_uint2_unsafe _s 0 in\n";
  fprintf f
    "  let _meth_index = Netamqp_rtypes.read_uint2_unsafe _s 2 in\n";
  fprintf f 
    "  let _c = ref 4 in\n";
  fprintf f
    "  match (_class_index lsl 16) + _meth_index with\n";
  
  List.iter
    (fun cls ->
       List.iter
	 (fun meth ->
	    fprintf f
	      "   | %d ->\n" 
	      (cls.class_index * 65536 + meth.meth_index);
	    output_field_decoder meth.meth_fields f 8;
	    fprintf f
	      "        `%s%s\n"
	      (transl_meth_name cls meth)
	      (if meth.meth_fields <> [] then
		 "(" ^ 
		   String.concat ","
		     (List.map
			(fun f -> transl_name f.field_name) 
			meth.meth_fields) ^  ")"
	       else
		 ""
	      );
	 )
	 cls.class_meths
    )
    spec.spec_classes;

  fprintf f
    "    | _ -> raise(Netamqp_types.Decode_error \"Unkown class/method\")\n\n"


let output_prop_decoder fields f indent =
  (* _s: the string to analyze
     _c: the cursor (int ref)
     _l: the end position of _s (i.e. indexes < _l are allowed)
     _e_<fieldname>: whether this field is enabled

     This function creates code like:

     let <fieldname1> = ... in
     let <fieldname2> = ... in
     ...
     let <fieldnameN> = ... in

     so that the following expression can access all fieldnames.
     Disabled fieldnames are [None].

   *)
  let istr = String.make indent ' ' in

  let need_c0 =
    function
      | `Octet | `Short | `Long | `Longlong | `Timestamp -> true
      | _ -> false in

  let rec emit fields =
    match fields with
      | field :: fields' ->
	  let tn = transl_name field.field_name in
	  let t = field.field_dom.dom_type in
	  if t = `Bit then (* special case: no Some/None *)
	    fprintf f
	      "%slet %s = _e_%s in\n" istr tn tn
	  else (
	    if need_c0 t then
	      fprintf f "%slet _c0 = !_c in\n" istr;
	    fprintf f "%slet %s =\n" istr tn;
	    fprintf f "%s  if _e_%s then (\n" istr tn;
	    ( match t with
		| `Octet ->
		    fprintf f
		      "%s    if _c0 > _l-1 then %s;\n" istr exn_too_short;
		    fprintf f
		      "%s    _c := _c0 + 1;\n" istr;
		    fprintf f
		      "%s    Some(Char.code(String.unsafe_get _s _c0))\n"
		      istr;
		| `Short ->
		    fprintf f
		      "%s    if _c0 > _l-2 then %s;\n" istr exn_too_short;
		    fprintf f
		      "%s    _c := _c0 + 2;\n" istr;
		    fprintf f
		      "%s    Some(Netamqp_rtypes.read_uint2_unsafe _s _c0)\n"
		      istr;
		| `Long ->
		    fprintf f
		      "%s    if _c0 > _l-4 then %s;\n" istr exn_too_short;
		    fprintf f
		      "%s    _c := _c0 + 4;\n" istr;
		    fprintf f
		      "%s    Some(Rtypes.read_uint4_unsafe _s _c0)\n"
		      istr;
		| `Longlong ->
		    fprintf f
		      "%s    if _c0 > _l-8 then %s;\n" istr exn_too_short;
		    fprintf f
		      "%s    _c := _c0 + 8;\n" istr;
		    fprintf f
		      "%s    Some(Rtypes.read_uint8_unsafe _s _c0)\n"
		      istr;
		| `Bit ->
		    assert false (* already handled above *)
		| `Timestamp ->
		    fprintf f
		      "%s    if _c0 > _l-8 then %s;\n" istr exn_too_short;
		    fprintf f
		      "%s    _c := _c0 + 8;\n" istr;
		    fprintf f
		      "%s    Some(Int64.to_float(Rtypes.int64_of_uint8\
                               (Rtypes.read_uint8_unsafe _s _c0)))\n"
		      istr;
		| `Shortstr ->
		    fprintf f 
		      "%s    Some(Netamqp_rtypes.decode_shortstr _s _c _l)\n"
		      istr
		| `Longstr ->
		    fprintf f
		      "%s    Some(Netamqp_rtypes.decode_longstr _s _c _l)\n"
		      istr
		| `Field_table ->
		    fprintf f 
		      "%s    Some(Netamqp_rtypes.decode_table _s _c _l)\n"
		      istr
	    );
	    fprintf f "%s  ) else None in\n" istr;
	  );
	  emit fields'
      | [] ->
	  fprintf f 
	    "%sif !_c <> _l then %s;\n" istr exn_too_long
  in
  emit fields

let output_header_decoder spec f =
  fprintf f
    "let decode_header_message _frame =\n";
  fprintf f
    "  let _s = Xdr_mstring.concat_mstrings \
          _frame.Netamqp_types.frame_payload in\n";
  fprintf f
    "  let _l = String.length _s in\n";
  fprintf f
    "  if _l < 14 then %s;\n" exn_too_short;
  fprintf f
    "  let _class_index = Netamqp_rtypes.read_uint2_unsafe _s 0 in\n";
  fprintf f
    "  let _size_rt = Rtypes.read_uint8_unsafe _s 4 in\n";
  fprintf f
    "  let _flags = Netamqp_rtypes.read_uint2_unsafe _s 12 in\n";
  fprintf f 
    "  let _c = ref 14 in\n";
  fprintf f
    "  match _class_index with\n";
  
  List.iter
    (fun cls ->
       fprintf f
	 "   | %d ->\n" 
	 cls.class_index;
       if List.length cls.class_props > 15 then
	 failwith "Implementation restriction: only up to 15 properties";
       let bit = ref 15 in
       List.iter
	 (fun field ->
	    let tn = transl_name field.field_name in
	    fprintf f
	      "        let _e_%s = ((_flags lsr %d) land 1) = 1 in\n"
	      tn !bit;
	    decr bit
	 )
	 cls.class_props;
       output_prop_decoder cls.class_props f 8;
       fprintf f
	 "        (`P_%s%s"
	 (transl_name cls.class_name)
	 (if cls.class_props <> [] then
	    "(" ^ 
	      String.concat ","
	      (List.map
		 (fun f -> transl_name f.field_name) 
		 cls.class_props) ^  ")"
	  else
	    ""
	 );
       fprintf f
	 ",Rtypes.int64_of_uint8 _size_rt)\n"
    )
    spec.spec_classes;

  fprintf f
    "    | _ -> raise(Netamqp_types.Decode_error \"Unkown class/method\")\n\n"



let exn_range =
  "raise(Netamqp_types.Encode_error \"Value out of range\")"


let output_field_encoder fields f indent =
  (* For the generated code: The function assumes that there are variables 
     for each field (having the name of the field). It creates code like:

     let _s = String.create <n> in
     ... <fill _s>;
     let _acc = _s :: _acc in
     let _acc_len = _acc_len + <n> in
     ...

     At the end _acc contains a list of strings in reverse order, and
     _acc_len the length of the strings.

     _acc should have been initialized with [], and _acc_len with 0.
   *)
  let istr = String.make indent ' ' in

  let finish_bitfield offset =
    fprintf f 
      "%sString.unsafe_set _s %d (Char.chr _x);\n" istr offset in

  let finish_s length =
    fprintf f
      "%slet _acc = _s :: _acc in\n" istr;
    fprintf f
      "%slet _acc_len = _acc_len + %d in\n" istr length in

  let rec emit offset s_flag bitfield_flag fields =
    (* s_flag: whether we still fill _s *)
    match fields with
      | field :: fields' ->
	  let n = transl_name field.field_name in
	  let t = field.field_dom.dom_type in
	  let adv = field.field_advance in
	  if adv >= 0 then (
	    (* Field with fixed length *)
	    if not s_flag then (
	      let num = get_cumulated_length fields in
	      fprintf f
		"%slet _s = String.create %d in\n" istr num;
	    );
	    if t = `Bit && not bitfield_flag then
	      fprintf f
		"%slet _x = 0 in\n" istr;
	    ( match t with
		| `Octet ->
		    fprintf f
		      "%sif %s < 0 || %s > 255 then %s;\n" istr n n exn_range;
		    fprintf f
		      "%sString.unsafe_set _s %d (Char.unsafe_chr %s);\n"
		      istr offset n;
		    emit (offset+1) true false fields'
		| `Short ->
		    fprintf f
		      "%sif %s < 0 || %s > 65535 then %s;\n" istr n n exn_range;
		    fprintf f
		      "%sNetamqp_rtypes.write_uint2_unsafe _s %d %s;\n"
		      istr offset n;
		    emit (offset+2) true false fields'
		| `Long ->
		    fprintf f
		      "%sRtypes.write_uint4_unsafe _s %d %s;\n"
		      istr offset n;
		    emit (offset+4) true false fields'
		| `Longlong ->
		    fprintf f
		      "%sRtypes.write_uint8_unsafe _s %d %s;\n"
		      istr offset n;
		    emit (offset+8) true false fields'
		| `Bit ->
		    fprintf f
		      "%slet _x = ((if %s then 1 else 0) lsl %d) lor _x in\n"
		      istr n field.field_bit;
		    if adv>0 then finish_bitfield offset;
		    emit (offset+adv) true (adv=0) fields'
		| `Timestamp ->
		    fprintf f
		      "%slet _x = Rtypes.uint8_of_int64(Int64.of_float %s) in\n"
		      istr n;
		    fprintf f
		      "%sRtypes.write_uint8_unsafe _s %d _x;\n"
		      istr offset;
		    emit (offset+8) true false fields'
		| _ ->
		    assert false
	    )
	  )
	  else (
	    (* Field with variable length *)
	    if s_flag then finish_s offset;
	    ( match t with
		| `Shortstr ->
		    fprintf f
		      "%slet (_x,_l) = Netamqp_rtypes.encode_shortstr %s in\n" istr n
		| `Longstr ->
		    fprintf f
		      "%slet (_x,_l) = Netamqp_rtypes.encode_longstr %s in\n" istr n
		| `Field_table ->
		    fprintf f
		      "%slet (_x,_l) = Netamqp_rtypes.encode_table %s in\n" istr n
		| _ ->
		    assert false
	    );
	    fprintf f
	      "%slet _acc = _x @ _acc in\n" istr;
	    fprintf f
	      "%slet _acc_len = _acc_len + _l in\n" istr;
	    emit 0 false false fields'
	  )

      | [] ->
	  if s_flag then finish_s offset;
  in
  emit 0 false false fields


let output_method_encoder spec f =
  fprintf f
    "let encode_method_message _msg _channel =\n";
  fprintf f 
    "  let _payload =\n";
  fprintf f
    "    match _msg with\n";
  
  List.iter
    (fun cls ->
       List.iter
	 (fun meth ->
	    fprintf f
	      "     | `%s%s ->\n" 
	      (transl_meth_name cls meth)
	      (if meth.meth_fields <> [] then
		 "(" ^ 
		   String.concat ","
		     (List.map
			(fun f -> transl_name f.field_name) 
			meth.meth_fields) ^  ")"
	       else
		 ""
	      );
	    let s = String.create 4 in
	    s.[0] <- Char.chr ((cls.class_index lsr 8) land 0xff);
	    s.[1] <- Char.chr (cls.class_index land 0xff);
	    s.[2] <- Char.chr ((meth.meth_index lsr 8) land 0xff);
	    s.[3] <- Char.chr (meth.meth_index land 0xff);
	    if meth.meth_fields = [] then
	      fprintf f
		"          %S\n" s
	    else (
	      fprintf f
		"          let _acc = [ %S ] in\n" s;
	      fprintf f
		"          let _acc_len = 4 in\n";
	      output_field_encoder meth.meth_fields f 10;
	      fprintf f
		"          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len\n"
	    )
	 )
	 cls.class_meths
    )
    spec.spec_classes;

  fprintf f
    "  in\n";
  fprintf f
    "  { Netamqp_types.frame_type = `Method;\n";
  fprintf f
    "    frame_channel = _channel;\n";
  fprintf f
    "    frame_payload = [Netamqp_rtypes.mk_mstring _payload];\n";
  fprintf f
    "  }\n\n"


let output_prop_encoder fields f indent =
  (* For the generated code: The function assumes that there are variables 
     for each field (having the name of the field). Depending on the
     value of the field, the field is included or not.

     It creates code like:

     let _s = String.create <n> in
     ... <fill _s>;
     let _acc = _s :: _acc in
     let _acc_len = _acc_len + <n> in
     ...

     At the end _acc contains a list of strings in reverse order, and
     _acc_len the length of the strings.

     _acc should have been initialized with [], and _acc_len with 0.
   *)
  let istr = String.make indent ' ' in
  let istr6 = String.make (indent+6) ' ' in

  let rec emit fields =
    match fields with
      | field :: fields' ->
	  let n = transl_name field.field_name in
	  let t = field.field_dom.dom_type in
	  if t <> `Bit then (
	    fprintf f
	      "%slet (_acc, _acc_len) =\n" istr;
	    fprintf f
	      "%s  match %s with\n" istr n;
	    fprintf f
	      "%s   | None -> (_acc,_acc_len)\n" istr;
	    fprintf f
	      "%s   | Some _x ->\n" istr;
	    match t with
	      | `Octet ->
		  fprintf f
		    "%s if _x < 0 || _x > 255 then %s;\n" istr6 exn_range;
		  fprintf f
		    "%s let _s = String.create 1 in\n" istr6;
		  fprintf f
		    "%s String.unsafe_set _s 0 (Char.unsafe_chr _x);\n"
		    istr6;
		  fprintf f
		    "%s (_s :: _acc, _acc_len+1) in\n" istr6
	      | `Short ->
		  fprintf f
		    "%s if _x < 0 || _x > 65535 then %s;\n" istr6 exn_range;
		  fprintf f
		    "%s let _s = String.create 2 in\n" istr6;
		  fprintf f
		    "%s Netamqp_rtypes.write_uint2_unsafe _s 0 _x;\n" istr6;
		  fprintf f
		    "%s (_s :: _acc, _acc_len+2) in\n" istr6
	      | `Long ->
		  fprintf f
		    "%s let _s = String.create 4 in\n" istr6;
		  fprintf f
		    "%s Rtypes.write_uint4_unsafe _s 0 _x;\n"
		    istr6;
		  fprintf f
		    "%s (_s :: _acc, _acc_len+4) in\n" istr6
	      | `Longlong ->
		  fprintf f
		      "%s let _s = String.create 8 in\n" istr6;
		  fprintf f
		    "%s Rtypes.write_uint8_unsafe _s 0 _x;\n" istr6;
		  fprintf f
		    "%s (_s :: _acc, _acc_len+8) in\n" istr6
	      | `Bit ->
		  assert false
	      | `Timestamp ->
		  fprintf f
		    "%s let _s = String.create 8 in\n" istr6;
		  fprintf f
		    "%s let _x' = \
                       Rtypes.uint8_of_int64(Int64.of_float _x) in\n"
		    istr6;
		  fprintf f
		    "%s Rtypes.write_uint8_unsafe _s 0 _x';\n"
		    istr6;
		  fprintf f
		    "%s (_s :: _acc, _acc_len+8) in\n" istr6
	      | `Shortstr ->
		  fprintf f
		    "%s let (_a,_l) = \
                       Netamqp_rtypes.encode_shortstr _x in\n" istr6;
		  fprintf f
		    "%s (_a @ _acc, _acc_len+_l) in\n" istr6
	      | `Longstr ->
		  fprintf f
		    "%s let (_a,_l) = \
                       Netamqp_rtypes.encode_longstr _x in\n" istr6;
		  fprintf f
		    "%s (_a @ _acc, _acc_len+_l) in\n" istr6
	      | `Field_table ->
		  fprintf f
		    "%s let (_a,_l) = \
                       Netamqp_rtypes.encode_table _x in\n" istr6;
		  fprintf f
		    "%s (_a @ _acc, _acc_len+_l) in\n" istr6
	  );
	  emit fields'

      | [] ->
	  ()
  in
  emit fields


let output_header_encoder spec f =
  fprintf f
    "let encode_header_message _props _size _channel =\n";
  fprintf f 
    "  let _payload =\n";
  fprintf f
    "    match _props with\n";
  
  List.iter
    (fun cls ->
       fprintf f
	 "     | `P_%s%s ->\n" 
	 (transl_name cls.class_name)
	 (if cls.class_props <> [] then
	    "(" ^ 
	      String.concat ","
	      (List.map
		 (fun f -> transl_name f.field_name) 
		 cls.class_props) ^  ")"
	  else
	    ""
	 );
       fprintf f 
	 "          let _s = String.make 14 '\\000' in\n";
       let c0 = Char.chr ((cls.class_index lsr 8) land 0xff) in
       let c1 = Char.chr (cls.class_index land 0xff) in
       fprintf f
	 "          String.unsafe_set _s 0 '\\x%02x';\n" (Char.code c0);
       fprintf f
	 "          String.unsafe_set _s 1 '\\x%02x';\n" (Char.code c1);
       fprintf f
	 "          Rtypes.write_uint8_unsafe _s 4 \
                      (Rtypes.uint8_of_int64 _size);\n";
       if cls.class_props = [] then
	 fprintf f
	   "          _s\n"
       else (
	 fprintf f
	   "          let _flags =\n";
	 let bit = ref 15 in
	 List.iter
	   (fun field ->
	      let lor_s =
		if !bit = 15 then "" else "lor " in
	      fprintf f
		"            %s(if %s then %d else 0)\n"
		lor_s
		(if field.field_dom.dom_type = `Bit then
		   transl_name field.field_name
		 else
		   transl_name field.field_name ^ " <> None"
		)
		(1 lsl !bit);
	      decr bit
	   )
	   cls.class_props;
	 fprintf f
	   "            in\n";
	 fprintf f
	   "          String.unsafe_set _s 12 \
                        (Char.chr((_flags lsr 8) land 0xff));\n";
	 fprintf f
	   "          String.unsafe_set _s 13 \
                        (Char.chr(_flags land 0xff));\n";
	 fprintf f
	   "          let _acc = [ _s ] in\n";
	 fprintf f
	   "          let _acc_len = 14 in\n";
	 output_prop_encoder cls.class_props f 10;
	 fprintf f
	   "          Netamqp_rtypes.unsafe_rev_concat _acc _acc_len\n"
       )
    )
    spec.spec_classes;

  fprintf f
    "  in\n";
  fprintf f
    "  { Netamqp_types.frame_type = `Header;\n";
  fprintf f
    "    frame_channel = _channel;\n";
  fprintf f
    "    frame_payload = [Netamqp_rtypes.mk_mstring _payload];\n";
  fprintf f
    "  }\n\n"


let output_heartbeat_encoder f =
  fprintf f
    "let encode_heartbeat_message() =\n";
  fprintf f
    "  { Netamqp_types.frame_type = `Heartbeat;\n";
  fprintf f
    "    frame_channel = 0;\n";
  fprintf f
    "    frame_payload = [Netamqp_rtypes.mk_mstring \"\\000\\000\"];\n";
  fprintf f
    "  }\n\n"


let output_body_encoder f =
  fprintf f
    "let encode_body_message data channel =\n";
  fprintf f
    "  { Netamqp_types.frame_type = `Body;\n";
  fprintf f
    "    frame_channel = channel;\n";
  fprintf f
    "    frame_payload = data;\n";
  fprintf f
    "  }\n\n"


let output_proto_header_encoder f =
  fprintf f
    "let encode_proto_header_message data =\n";
  fprintf f
    "  { Netamqp_types.frame_type = `Proto_header;\n";
  fprintf f
    "    frame_channel = 0;\n";
  fprintf f
    "    frame_payload = [Netamqp_rtypes.mk_mstring data];\n";
  fprintf f
    "  }\n\n"


let output_decoder f =
  fprintf f
    "let decode_message frame =\n";
  fprintf f
    "  match frame.Netamqp_types.frame_type with\n";
  fprintf f
    "   | `Method -> `Method(decode_method_message frame)\n";
  fprintf f
    "   | `Header -> `Header(decode_header_message frame)\n";
  fprintf f
    "   | `Body -> `Body frame.Netamqp_types.frame_payload\n";
  fprintf f
    "   | `Heartbeat -> `Heartbeat\n";
  fprintf f
    "   | `Proto_header -> `Proto_header \
          (Xdr_mstring.concat_mstrings frame.Netamqp_types.frame_payload)\n\n"


let output_encoder f =
  fprintf f
    "let encode_message msg channel =\n";
  fprintf f
    "  match msg with\n";
  fprintf f
    "   | `Method m -> encode_method_message m channel\n";
  fprintf f
    "   | `Header(props,size) -> encode_header_message props size channel\n";
  fprintf f
    "   | `Body data -> encode_body_message data channel\n";
  fprintf f
    "   | `Heartbeat -> encode_heartbeat_message()\n";
  fprintf f
    "   | `Proto_header p -> encode_proto_header_message p\n\n"



let output_mli spec =
  let f = open_out (out_mod ^  ".mli") in
  fprintf f "%s" gen_warning;
  
  (* First translate the parsed spec into types *)
  output_domain_type_definitions spec f;
  output_method_type_definition spec f true;
  output_string_helpers spec f true;
  output_props_type_definition spec f;
  output_message_type_definition spec f;

  (* Functions: *)
  fprintf f "val decode_method_message : Netamqp_types.frame -> method_t\n\n";
  fprintf f "val encode_method_message : method_t -> int -> Netamqp_types.frame\n\n";

  fprintf f "val decode_header_message : Netamqp_types.frame -> props_t * int64\n\n";
  fprintf f "val encode_header_message : props_t -> int64 -> int -> Netamqp_types.frame\n\n";

  fprintf f "val encode_heartbeat_message : unit -> Netamqp_types.frame\n\n";
  fprintf f "val encode_body_message : Xdr_mstring.mstring list -> int -> Netamqp_types.frame\n\n";
  fprintf f "val encode_proto_header_message : string -> Netamqp_types.frame\n\n";

  fprintf f "val decode_message : Netamqp_types.frame -> message_t\n\n";
  fprintf f "val encode_message : message_t -> int -> Netamqp_types.frame\n\n";

  close_out f


let output_ml spec =
  let f = open_out (out_mod ^  ".ml") in
  fprintf f "%s" gen_warning;
  
  (* First translate the parsed spec into types *)
  output_domain_type_definitions spec f;
  output_method_type_definition spec f false;
  output_string_helpers spec f false;
  output_props_type_definition spec f;
  output_message_type_definition spec f;

  (* Implementation *)
  output_method_decoder spec f;
  output_method_encoder spec f;

  output_header_decoder spec f;
  output_header_encoder spec f;

  output_heartbeat_encoder f;
  output_body_encoder f;
  output_proto_header_encoder f;

  output_decoder f;
  output_encoder f;

  close_out f



let () =
  let spec =
    parse_spec ~verbose:true () in
  output_mli spec;
  output_ml spec

