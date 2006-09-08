module Lex = Mfhash.Make (Hashlex.HashedString)

type posting =  {doc_id : int;
				 mutable freq : int;
				 positions  : (int) Varray.t}


type terminfo = {mutable df: int;
				 mutable tf : int;
				 mutable postings :  posting list}
				

type t = {mutable tokens : int;
		  lexicon        : (terminfo) Lex.t}
		
let new_posting doc_id pos = 
	let t = {doc_id = doc_id; freq = 1; positions = Varray.create 2 0} in
	Varray.add t.positions pos;
	t
	
let empty_postinglist () = [] 
	
let empty_terminfo () = {df = 0; tf = 0; postings = empty_postinglist ()}
	
let empty () = {tokens = 0; lexicon = Lex.create 100000}
	
let number_of_tokens ii = ii.tokens

let number_of_types ii = Lex.size ii.lexicon
	
let add_term_accurance ii doc term pos =		
		let add_term_accurance  doc pos terminfo =  
		
			terminfo.tf <- succ terminfo.tf;
			let _ = 
			match terminfo.postings with
				| posting :: tail when posting.doc_id == doc -> 
					
					posting.freq <- succ posting.freq;
					Varray.add posting.positions pos 
				
                | _ -> 	
	 				terminfo.df <- (succ terminfo.df) ;
					terminfo.postings <- ((new_posting doc pos) :: terminfo.postings) 
			in
			terminfo

		in

		Lex.update ii.lexicon (empty_terminfo ()) term (add_term_accurance doc pos) ;
		 ii.tokens <- succ ii.tokens;
		ii

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
		
let pretty_print i = 
		let aux1 term terminfo =
			Printf.printf "term = %s tf = %d df = %d\n" term terminfo.tf terminfo.df ;
			let aux2 posting =
				Printf.printf "doc_id = %d freq = %d\n" posting.doc_id posting.freq;
				let aux3 pos = Printf.printf "pos = %d\n" pos in
				Varray.iter aux3 posting.positions ;

			in
			List.iter aux2 terminfo.postings
		in
		Printf.printf "tokens = %d\n" i.tokens;
		Lex.iter aux1 i.lexicon 