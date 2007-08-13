
let read_sentence chan = 
	(* returns the next field of the TAB separated line and the next character 
		pos after the field *)
	
  let next_item line  start = 
  	let len = String.length line in
     	let next_tab_pos = 
        	try  
          	String.index_from line start '\t'
        	with 
          	| Not_found -> len
          	| Invalid_argument _ ->
               	Printf.eprintf "ERROR: not enough fields. Input line follows\n%s\n" line;
               	raise (Failure "not enough fields") 
				in
				
       			let item = String.sub line start (next_tab_pos - start) in
						(next_tab_pos +1), item
	
	in
		let rec read_sentence empty =   (* hívhatod ugyanúgy a rekurzív segédfüggvényt *)
			let line =
				try input_line chan
				with 
				 End_of_file -> match empty with
					                | true -> raise End_of_file
									| false -> ""
			in
			let last, word = next_item line 0 in    (* beolvassuk a szót, ha nincs akkor az end 0 lesz *)
			match word, empty with
			| "", true -> read_sentence true (* consume redundant newlines if sentence is empty yet *)
			| "", false -> []                    (* genuine end of non_empty sentence *)
			| _, _ ->                           (* next real item *)
				let _, gold =
					(*	try
						*)
						
							 next_item line last
				(*
					with _ -> Printf.eprintf "invalid line %d %s\n" last line; failwith "invalid line"
				*)
				in
				(word, gold) :: read_sentence false (* már tudjuk, hogy nem üres a mondat *)
		in
					read_sentence true
;;

(* vegigmegy a chan minden mondatan is atadja az f-nek*)
let  iter_sentence chan f =
	let rec loop () =
		f (read_sentence chan);
		loop ()
	in
	
try 
	loop()
with End_of_file -> ()

;;

let rec fold_sentence f a chan =
	 try
	 	let sentence = read_sentence chan in
	 	fold_sentence f (f a sentence) chan
	with End_of_file -> a

let print_sentence sentence =
	let print_pair (word, gold) = 
		Printf.fprintf stdout "%s\t%s\n" word gold;
	
		 in
		List.iter (print_pair) sentence
;;

	
let output_vint64 oc i =
	let i = Int64.succ i in
	Codec.Int64.output_vbyte (output_byte oc) i
	
let input_vint64 ic =
	let i = Codec.Int64.input_vbyte (fun () -> input_byte ic) in
	Int64.pred i
	
	
let output_vnatint oc i =
	let i = succ i in
	Codec.NatInt.output_vbyte (output_byte oc) i
	
let input_vnatint ic =
	let i = Codec.NatInt.input_vbyte (fun () -> input_byte ic) in
	pred i
	

let output_string o s = 
	let l = (String.length s)  in
	output_vnatint o (l + 1);
	output o s 0 l

let input_string i =
	let l = (input_vnatint i) - 1  in
	let s = String.create l in
	really_input i s 0 l ;
	s
(*	
let _ =
	let o = open_out_bin "/tmp/vacak" in
	output_string o "hello, world";
	output_string o "szia vilag";
	 close_out o;
	let i = open_in_bin "/tmp/vacak" in
	Printf.printf "%s\n%s\n" (input_string i) (input_string i);
	flush_all;
*)
