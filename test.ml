module Lex = (Hashlex)


let _ = 
let modulename = ref "btrie" in

let speclist =
    [("-struct", Arg.String (fun s -> modulename := s), "used datastructure : btree | btrie")]
in 
let usage_msg = "usage : test -struct btree | btrie" in
Arg.parse speclist (fun s -> ()) usage_msg;

let chan = stdin in

let read_lines  =
		let lex = Lex.create 100000000 in
	
		let rec loop  () =
			
			let word =  input_line chan in
    (*     	Lex.update lex word (fun freq -> (succ freq)) (1) ; *)
			incr (Lex.find lex word (ref 0)) ;
			loop ();	
		in
		try
	 	loop()
		with End_of_file -> lex

in
	
let lex = read_lines 
in
let print_word word freq =
	print_string word;
	print_char '\t';
	print_int !freq;
	print_newline ();
in
Lex.iter  print_word lex;
