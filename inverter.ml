module Lex = Mfhash.Make (Hashlex.HashedString)

type t = {mutable tokens : int;
		  lexicon        : (Terminfo.t) Lex.t}
		
	
let empty () = {tokens = 0; lexicon = Lex.create 100000}
	
let number_of_tokens ii = ii.tokens

let number_of_types ii = Lex.size ii.lexicon
	
let add_term_accurance ii doc term pos =		
		Lex.update ii.lexicon (Terminfo.empty ()) term (fun ti -> Terminfo.occurrence ti doc pos; ti) ;
	    ii.tokens <- succ ii.tokens

(*	
let write_posting o posting =
	output_binary_int o posting.doc_id;
	output_binary_int o posting.freq;
	Varray.iter  (output_binary_int o) posting.positions
	
let read_posting i = 
	let doc_id = input_binary_int i in
	let freq   = input_binary_int i in
	let ps = Varray.create freq 0 in
	let _ =
	for n = 1 to freq do
		let x = (input_binary_int i) in
		Varray.add ps (x)
	done in
	{doc_id = doc_id; freq = freq; positions = ps}
	

let write_terminfo o term terminfo = 
	Io.output_string o term;
	output_binary_int o terminfo.tf;
	output_binary_int o terminfo.df;
	List.iter (write_posting o) terminfo.postings
	
let read_terminfo i =
	let term = Io.input_string i in
	let tf   = input_binary_int  i in
	let df   = input_binary_int i in
	let postings = ref [] in
	let _ =
	for n = 1 to df do
		postings := (read_posting i):: !postings
	done in
	(term, {tf = tf; df = df; postings = !postings})

let write o ii =
	output_binary_int o ii.tokens;
	output_binary_int o (Lex.size ii.lexicon);
	Lex.sorted_iter (write_terminfo o) ii.lexicon  
	
let read i =
	let tokens = input_binary_int i in
	let types  = input_binary_int i in
	let lexicon = Lex.create types in
	let _ = try
	for n = 1 to types do
		let (term, terminfo) = read_terminfo i in 
		
		Lex.update lexicon terminfo term (fun _ -> terminfo)
	
	done
	with End_of_file -> ()
	in
	{tokens = tokens; lexicon = lexicon}
	
let calculate_size i = 
	let s = ref 8 in
	let lex = ref 0 in
	let postings = ref 0 in
	let positions = ref 0 in
	let aux1 term terminfo =
		s := !s + 4;
		let l = ((String.length term) + 4) in
		s := !s + l;
		lex := !lex + l;
		let aux2 posting =
			s:= !s + 8;
			postings := !postings + 8;
			let p = ((Varray.size posting.positions)*4) in
			s:= !s + p;
			positions := !positions + p
		in
		List.iter aux2 terminfo.postings
	in
	let _ = Lex.iter aux1 i.lexicon in
	(!s, !lex, !postings, !positions)
	
let iterate_over i = 
		let s = ref 8 in
		let lex = ref 0 in
		let postings = ref 0 in
		let positions = ref 0 in
		let aux1 term terminfo =
			s := !s + 4;
			let l = ((String.length term) + 4) in
			s := !s + l;
			lex := !lex + l;
			let aux2 posting =
				s:= !s + 8;
				postings := !postings + 8;
				let p = ref 0 in
				let aux3 pos = p := !p + 4 in
				Varray.iter aux3 posting.positions ;
				positions := !positions + !p;
				s := !s + !p
			in
			List.iter aux2 terminfo.postings
		in
		let _ = Lex.iter aux1 i.lexicon in
		(!s, !lex, !postings, !positions)
*)		


let pretty_print i = 
	let o = open_out_bin "terminfos" in
		Printf.printf "tokens = %d\n" i.tokens;
		let aux1 term terminfo =
			Printf.printf "%s\n" term;
			Terminfo.pretty_print terminfo;
			Terminfo.write o terminfo
		in
		Lex.iter aux1 i.lexicon;
		close_out o;
		let i = open_in_bin "terminfos" in
		Terminfo.pretty_print (Terminfo.read i);
		Terminfo.pretty_print (Terminfo.read i);
		Terminfo.pretty_print (Terminfo.read i);