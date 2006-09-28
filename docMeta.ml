type field = String of string | Int of int

type t = {mutable content : (int * field) list}

let empty () = {content = []}
	

let add_string meta id s =
	meta.content <- (id, String(s)) :: meta.content
	
let get_string meta id =
	match List.assoc id meta.content with
		String(s) -> s
	 | _  -> raise (Invalid_argument ("not string field: " ^ (string_of_int id)))


let get_int meta id =
	match List.assoc id meta.content with
		Int(i) -> i
	 | _  -> raise (Invalid_argument ("not int field: " ^ (string_of_int id)))

let read ic = 
	Marshal.from_channel ic
;;
	
let write meta oc =
	    Marshal.to_channel oc meta []
;;