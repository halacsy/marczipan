module Lex = (Bursttrie)


let _ = 
let modulename = ref "btrie" in

let speclist =
    [("-struct", Arg.String (fun s -> modulename := s), "used datastructure : btree | btrie")]
in 
let usage_msg = "usage : test -struct btree | btrie" in
Arg.parse speclist (fun s -> ()) usage_msg;

let chan = stdin in

let read_lines  =
		let lex = ref Lex.empty in
		let updatef data = match data with
			freq -> (succ freq)
		in
		let rec loop  () =
			let word =  input_line chan in
         (*   lex := Lex.update word (updatef) (1) !lex;*)
			let freq = try Lex.find  word !lex with Not_found -> 0 in	
			lex := Lex.add word (succ freq) !lex ;
			loop ();	
		in
		try
	 	loop()
		with End_of_file -> !lex

in
	
let lex = read_lines 
in
let print_word word freq =
	Printf.printf "%s\t%d\n" word freq 
in
Lex.iter  print_word lex;
