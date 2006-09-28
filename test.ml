external zobel_hash : string -> int -> int = "zobel_hash" "zobel_hash" 
external zobel_strcmp : string -> string -> int = "zobel_strcmp" "zobel_strcmp"
external zobel_streq  : string -> string -> bool = "zobel_streq" "zobel_streq"
module ZHashedString = struct
	type t = String.t
	let hash  = zobel_hash
	let equal (s1:string) (s2:string) = (zobel_strcmp s1 s2 = 0) 
	let compare = zobel_strcmp
end ;;

module OHashedString = struct
	type t = String.t
	let hash  w i = (Hashtbl.hash w) mod i 
	let equal (s1:string) (s2:string) = ( s1 =s2) 
	let compare (s1:string) (s2:string) = String.compare s1 s2
end ;;

module Lex = Mfhash.Make (OHashedString)
let _ =
	let ic = stdin in
	let lex = Lex.create 200000 in
	let rec loop () =
		let word = input_line ic in
		Lex.update lex (0) word (succ);
		loop () 
	in	
	try 
		loop ()
	with End_of_file ->
	print_int (Lex.size lex) ; print_newline
	
